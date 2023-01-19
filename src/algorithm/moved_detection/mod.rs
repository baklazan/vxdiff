use super::{
    indices::{IndexConverter, LineIndex, WordIndex},
    main_sequence::{get_aligner, Aligner},
    scoring::{affine_scoring::AffineWordScoring, TScore},
    AlignedFragment, DiffOp, MainSequenceAlgorithm, PartitionedText,
};

mod moved_cores;

const MIN_LINES_IN_CORE: usize = 3;

#[derive(Debug)]
struct Core {
    file_ids: [usize; 2],
    start: [LineIndex; 2],
    end: [LineIndex; 2],

    aligned_start: [WordIndex; 2],
    aligned_end: [WordIndex; 2],
    word_alignment: Vec<DiffOp>,
}
struct Hole {
    file_id: usize,
    start: LineIndex,
    end: LineIndex,
}

fn trim_to_core(
    alignment: &[DiffOp],
    start: [WordIndex; 2],
    end: [WordIndex; 2],
    index_converters: [&IndexConverter; 2],
    file_ids: [usize; 2],
) -> Core {
    let mut start_lines = [0, 1].map(|side| index_converters[side].word_to_line_after(start[side]));
    let mut start_words = start;
    let mut start_i = 0;
    let mut end_i = alignment.len();
    while (start_words[0] < index_converters[0].line_to_word(start_lines[0])
        || start_words[1] < index_converters[1].line_to_word(start_lines[1]))
        && start_i < end_i
    {
        for side in 0..2 {
            start_words[side] += alignment[start_i].movement()[side];
        }
        start_i += 1;
    }

    let mut end_words = end;
    let mut end_lines = [0, 1].map(|side| index_converters[side].word_to_line_before(end[side]));
    while (end_words[0] > index_converters[0].line_to_word(end_lines[0])
        || end_words[1] > index_converters[1].line_to_word(end_lines[1]))
        && end_i > start_i
    {
        for side in 0..2 {
            end_words[side] -= alignment[end_i - 1].movement()[side];
        }
        end_i -= 1;
    }
    for side in 0..2 {
        start_lines[side] = index_converters[side].word_to_line_before(start_words[side]);
        end_lines[side] = index_converters[side].word_to_line_after(end_words[side]);
    }
    Core {
        file_ids,
        start: start_lines,
        end: end_lines,
        aligned_start: start_words,
        aligned_end: end_words,
        word_alignment: Vec::from(&alignment[start_i..end_i]),
    }
}

fn filter_long_matches(
    word_alignment: &[DiffOp],
    index_converters: &[IndexConverter; 2],
    file_id: usize,
) -> (Vec<Core>, [Vec<Hole>; 2]) {
    #[derive(Clone, Copy, Debug)]
    struct ClusterBound {
        text_position: [WordIndex; 2],
        alignment_index: usize,
    }

    let mut current = [WordIndex::new(0); 2];
    let mut cluster_start: Option<ClusterBound> = None;
    let mut cluster_end: Option<ClusterBound> = None;

    let mut clusters: Vec<(ClusterBound, ClusterBound)> = vec![];

    for (i, &op) in word_alignment.iter().enumerate() {
        if op == DiffOp::Match && cluster_start.is_none() {
            cluster_start = Some(ClusterBound {
                text_position: current,
                alignment_index: i,
            });
        }

        for side in 0..2 {
            current[side] += op.movement()[side];
        }

        match op {
            DiffOp::Match => {
                cluster_end = Some(ClusterBound {
                    text_position: current,
                    alignment_index: i + 1,
                });
            }
            DiffOp::Delete | DiffOp::Insert => {
                if let Some(end) = cluster_end {
                    for side in 0..2 {
                        let whole_lines_skipped: LineIndex = index_converters[side]
                            .word_to_line_before(current[side])
                            .saturating_sub(index_converters[side].word_to_line_after(end.text_position[side]));
                        const MIN_SKIPPED_LINES_BETWEEN_CLUSTERS: usize = 2;
                        if whole_lines_skipped >= MIN_SKIPPED_LINES_BETWEEN_CLUSTERS {
                            clusters.push((cluster_start.unwrap(), end));
                            cluster_start = None;
                            cluster_end = None;
                            break;
                        }
                    }
                }
            }
        }
    }
    if let Some(cluster_end) = cluster_end {
        clusters.push((cluster_start.unwrap(), cluster_end));
    }

    let mut holes = [vec![], vec![]];
    let mut cores = vec![];
    let mut current_hole_start = [LineIndex::new(0); 2];
    for (start, end) in clusters {
        let core = trim_to_core(
            &word_alignment[start.alignment_index..end.alignment_index],
            start.text_position,
            end.text_position,
            [&index_converters[0], &index_converters[1]],
            [file_id, file_id],
        );

        if core.end[0] - core.start[0] >= MIN_LINES_IN_CORE
            && core.end[1] - core.start[1] >= MIN_LINES_IN_CORE
            && !core.word_alignment.is_empty()
        {
            for side in 0..2 {
                if core.start[side] > current_hole_start[side] {
                    holes[side].push(Hole {
                        file_id,
                        start: current_hole_start[side],
                        end: core.start[side],
                    });
                }
            }
            current_hole_start = core.end;
            cores.push(core);
        }
    }
    for side in 0..2 {
        if index_converters[side].lines_count() > current_hole_start[side] {
            holes[side].push(Hole {
                file_id,
                start: current_hole_start[side],
                end: index_converters[side].lines_count(),
            });
        }
    }

    (cores, holes)
}

#[derive(Clone, Copy, Debug)]
struct ExtensionTradeoffPoint<'a> {
    position: [LineIndex; 2],
    alignment: &'a [DiffOp],
    aligned_position: [WordIndex; 2],
    score: TScore,
}

fn prefix_tradeoffs<'a>(
    alignment: &'a [DiffOp],
    prefix_scores: &[TScore],
    start: [WordIndex; 2],
    converters: [&IndexConverter; 2],
    file_ids: [usize; 2],
    aligner: &dyn Aligner,
) -> Vec<ExtensionTradeoffPoint<'a>> {
    let mut word_position = start;
    let mut line_position = [0, 1].map(|side| converters[side].word_to_line_after(word_position[side]));
    let mut best_seen_score = prefix_scores[0];
    let mut result = vec![ExtensionTradeoffPoint {
        position: line_position,
        alignment: &[],
        aligned_position: word_position,
        score: best_seen_score,
    }];
    for (i, op) in alignment.iter().enumerate() {
        for side in 0..2 {
            word_position[side] += op.movement()[side];
            line_position[side] = converters[side].word_to_line_after(word_position[side]);
        }
        let word_position_of_line_end = [0, 1].map(|side| converters[side].line_to_word(line_position[side]));

        let proposed_score =
            prefix_scores[i + 1] + aligner.score_gaps_between(file_ids, word_position, word_position_of_line_end);
        if proposed_score > best_seen_score {
            best_seen_score = proposed_score;
            if result.last().unwrap().position == line_position {
                result.pop();
            }
            result.push(ExtensionTradeoffPoint {
                position: line_position,
                alignment: &alignment[..i + 1],
                aligned_position: word_position,
                score: best_seen_score,
            });
        }
    }
    result
}

fn suffix_tradeoffs<'a>(
    alignment: &'a [DiffOp],
    suffix_scores: &[TScore],
    end: [WordIndex; 2],
    converters: [&IndexConverter; 2],
    file_ids: [usize; 2],
    aligner: &dyn Aligner,
) -> Vec<ExtensionTradeoffPoint<'a>> {
    let mut word_position = end;
    let mut line_position = [0, 1].map(|side| converters[side].word_to_line_before(word_position[side]));
    let mut best_seen_score = *suffix_scores.last().unwrap();
    let mut result = vec![ExtensionTradeoffPoint {
        position: line_position,
        alignment: &[],
        aligned_position: word_position,
        score: best_seen_score,
    }];
    for (i, op) in alignment.iter().enumerate().rev() {
        for side in 0..2 {
            word_position[side] -= op.movement()[side];
            line_position[side] = converters[side].word_to_line_before(word_position[side]);
        }
        let word_position_of_line_start = [0, 1].map(|side| converters[side].line_to_word(line_position[side]));
        let proposed_score =
            suffix_scores[i] + aligner.score_gaps_between(file_ids, word_position_of_line_start, word_position);
        if proposed_score > best_seen_score {
            best_seen_score = proposed_score;
            if result.last().unwrap().position == line_position {
                result.pop();
            }
            result.push(ExtensionTradeoffPoint {
                position: line_position,
                alignment: &alignment[i..],
                aligned_position: word_position,
                score: best_seen_score,
            });
        }
    }
    result.reverse();
    result
}

struct TradeoffsSolver<'a, 'b> {
    candidates: &'a [Vec<ExtensionTradeoffPoint<'b>>],
    score_buffer: Vec<Vec<TScore>>,
    come_from_buffer: Vec<Vec<usize>>,
}

impl<'a, 'b> TradeoffsSolver<'a, 'b> {
    fn new(tradeoff_candidates: &'a [Vec<ExtensionTradeoffPoint<'b>>]) -> Self {
        TradeoffsSolver {
            candidates: tradeoff_candidates,
            score_buffer: tradeoff_candidates
                .iter()
                .map(|row| vec![TScore::NEG_INFINITY; row.len()])
                .collect(),
            come_from_buffer: tradeoff_candidates
                .iter()
                .map(|row| vec![usize::MAX; row.len()])
                .collect(),
        }
    }

    fn best_path_with_start(
        &mut self,
        low_inclusive: &[usize],
        high_inclusive: &[usize],
        start: usize,
    ) -> (TScore, Vec<usize>) {
        self.score_buffer[0][start] = self.candidates[0][start].score;
        for row_id in 0..self.candidates.len() - 1 {
            let next_row_id = row_id + 1;
            let row = &self.candidates[row_id];
            let next_row = &self.candidates[next_row_id];
            if row_id % 2 == 0 {
                let mut post_id = low_inclusive[row_id];
                let mut best_post_score = TScore::NEG_INFINITY;
                let mut best_post_id = post_id;
                for pre_id in low_inclusive[next_row_id]..=high_inclusive[next_row_id] {
                    while post_id <= high_inclusive[row_id] && row[post_id].position[0] <= next_row[pre_id].position[0]
                    {
                        if self.score_buffer[row_id][post_id] > best_post_score {
                            best_post_score = self.score_buffer[row_id][post_id];
                            best_post_id = post_id;
                        }
                        post_id += 1;
                    }
                    self.score_buffer[next_row_id][pre_id] = best_post_score + next_row[pre_id].score;
                    self.come_from_buffer[next_row_id][pre_id] = best_post_id;
                }
            } else {
                let mut pre_id = high_inclusive[row_id];
                let mut best_pre_score = TScore::NEG_INFINITY;
                let mut best_pre_id = pre_id;
                for post_id in (low_inclusive[next_row_id]..=high_inclusive[next_row_id]).rev() {
                    while pre_id >= low_inclusive[row_id] && row[pre_id].position[1] >= next_row[post_id].position[1] {
                        if self.score_buffer[row_id][pre_id] > best_pre_score {
                            best_pre_score = self.score_buffer[row_id][pre_id];
                            best_pre_id = pre_id;
                        }
                        if pre_id == low_inclusive[row_id] {
                            break;
                        }
                        pre_id -= 1;
                    }
                    self.score_buffer[next_row_id][post_id] = best_pre_score + next_row[post_id].score;
                    self.come_from_buffer[next_row_id][post_id] = best_pre_id;
                }
            }
        }
        let mut best_score = TScore::NEG_INFINITY;
        let mut best_last_id = 0;
        let last_row_id = self.candidates.len() - 1;
        for last_id in low_inclusive[last_row_id]..=high_inclusive[last_row_id] {
            let candidate = &self.candidates[last_row_id][last_id];
            if candidate.position[1] >= self.candidates[0][start].position[1]
                && self.score_buffer[last_row_id][last_id] > best_score
            {
                best_score = self.score_buffer[last_row_id][last_id];
                best_last_id = last_id;
            }
        }
        let mut result = vec![best_last_id];
        let mut current_id = best_last_id;
        for row_id in (0..self.candidates.len() - 1).rev() {
            current_id = self.come_from_buffer[row_id + 1][current_id];
            result.push(current_id);
        }
        result.reverse();
        self.score_buffer[0][start] = TScore::NEG_INFINITY;
        (best_score, result)
    }

    fn optimal_tradeoffs_recursive(
        &mut self,
        low_inclusive: &[usize],
        high_inclusive: &[usize],
    ) -> (TScore, Vec<usize>) {
        let mid_start = (low_inclusive[0] + high_inclusive[0]) / 2;
        let (mid_score, mut mid_path) = self.best_path_with_start(low_inclusive, high_inclusive, mid_start);
        let mut best_score = mid_score;
        let mut best_path = mid_path.clone();
        if mid_start > low_inclusive[0] {
            mid_path[0] = mid_start - 1;
            let (left_score, letf_path) = self.optimal_tradeoffs_recursive(low_inclusive, &mid_path);
            if left_score > best_score {
                best_score = left_score;
                best_path = letf_path;
            }
        }
        if mid_start < high_inclusive[0] {
            mid_path[0] = mid_start + 1;
            let (right_score, right_path) = self.optimal_tradeoffs_recursive(&mid_path, high_inclusive);
            if right_score > best_score {
                best_score = right_score;
                best_path = right_path;
            }
        }
        (best_score, best_path)
    }
}

fn optimal_tradeoffs<'a>(
    tradeoff_candidates: &[Vec<ExtensionTradeoffPoint<'a>>],
) -> (TScore, Vec<ExtensionTradeoffPoint<'a>>) {
    let left_inclusive = vec![0; tradeoff_candidates.len()];
    let right_inclusive: Vec<usize> = tradeoff_candidates.iter().map(|row| row.len() - 1).collect();

    let mut solver = TradeoffsSolver::new(tradeoff_candidates);
    let (score, path) = solver.optimal_tradeoffs_recursive(&left_inclusive, &right_inclusive);

    let mut result = vec![];

    for row_id in 0..tradeoff_candidates.len() {
        result.push(tradeoff_candidates[row_id][path[row_id]]);
    }

    (score, result)
}

fn extend_cores(
    main_cores: Vec<Core>,
    mut moved_cores: Vec<Core>,
    index_converters: &[[IndexConverter; 2]],
    aligner: &dyn Aligner,
) -> Vec<(AlignedFragment, bool)> {
    let mut cores = main_cores;
    let mut is_main = vec![true; cores.len()];
    cores.append(&mut moved_cores);
    is_main.resize(cores.len(), false);

    // add virtual zero-length cores at the start and at the end of each file
    for (file_id, file_converters) in index_converters.iter().enumerate() {
        cores.push(Core {
            file_ids: [file_id, file_id],
            start: [LineIndex::new(0); 2],
            end: [LineIndex::new(0); 2],
            aligned_start: [WordIndex::new(0); 2],
            aligned_end: [WordIndex::new(0); 2],
            word_alignment: vec![],
        });
        is_main.push(true);
        let end_lines = [0, 1].map(|side| file_converters[side].lines_count());
        let end_words = [0, 1].map(|side| file_converters[side].words_count());
        cores.push(Core {
            file_ids: [file_id, file_id],
            start: end_lines,
            end: end_lines,
            aligned_start: end_words,
            aligned_end: end_words,
            word_alignment: vec![],
        });
        is_main.push(true);
    }

    let mut previous_core: Vec<[Option<usize>; 2]> = vec![[None; 2]; cores.len()];
    let mut next_core: Vec<[Option<usize>; 2]> = vec![[None; 2]; cores.len()];

    // a group of cores that are consecutive on both sides can be potentially joined into
    // a single aligned fragment.
    let mut is_first_core_in_chain = vec![true; cores.len()];
    let mut next_core_in_chain: Vec<Option<usize>> = vec![None; cores.len()];
    let mut bridges_after_cores_in_chain: Vec<Option<Vec<DiffOp>>> = vec![None; cores.len()];

    for side in 0..2 {
        let mut cores_by_side = vec![];
        for (core_id, core) in cores.iter().enumerate() {
            // sort by sum of coordinates, to avoid corner cases with zero-length virtual cores
            cores_by_side.push((core.file_ids[side], core.start[side] + core.end[side], core_id));
        }
        cores_by_side.sort();
        for i in 0..cores_by_side.len() - 1 {
            let earlier_core_id = cores_by_side[i].2;
            let later_core_id = cores_by_side[i + 1].2;
            if cores[earlier_core_id].file_ids[side] == cores[later_core_id].file_ids[side] {
                previous_core[later_core_id][side] = Some(earlier_core_id);
                next_core[earlier_core_id][side] = Some(later_core_id);
            }
        }

        // detect and relabel moved cores that can be inserted into the main sequence
        // TODO: switch from greedy to DP (can be done in O(n log n))
        // TODO: extract function
        if side == 0 {
            let mut next_main_core = vec![None; cores.len()];
            let mut last_main_seen = None;
            for i in (0..cores_by_side.len()).rev() {
                let core_id = cores_by_side[i].2;
                if is_main[core_id] {
                    last_main_seen = Some(core_id);
                    continue;
                }
                if let Some(last_main) = last_main_seen {
                    if cores[core_id].file_ids != cores[last_main].file_ids {
                        last_main_seen = None;
                    }
                }
                if let Some(last_main) = last_main_seen {
                    if cores[core_id].end[1] <= cores[last_main].start[1] {
                        next_main_core[core_id] = Some(last_main);
                    }
                }
            }
            let mut last_main_seen: Option<usize> = None;
            for i in 0..cores_by_side.len() {
                let core_id = cores_by_side[i].2;
                if let Some(last_main) = last_main_seen {
                    if cores[last_main].file_ids != cores[core_id].file_ids {
                        last_main_seen = None;
                    }
                }

                if !is_main[core_id] && next_main_core[core_id].is_some() {
                    if let Some(last_main) = last_main_seen {
                        if cores[last_main].end[1] <= cores[core_id].start[1] {
                            is_main[core_id] = true;
                        }
                    }
                }

                if is_main[core_id] {
                    last_main_seen = Some(core_id);
                }
            }
        }
    }

    let mut post_extension_processed = vec![false; cores.len()];
    let mut core_pre_extension: Vec<Vec<DiffOp>> = vec![vec![]; cores.len()];
    let mut core_post_extension: Vec<Vec<DiffOp>> = vec![vec![]; cores.len()];
    let mut core_pre_extension_from: Vec<[WordIndex; 2]> = cores.iter().map(|c| c.aligned_start).collect();
    let mut core_post_extension_to: Vec<[WordIndex; 2]> = cores.iter().map(|c| c.aligned_end).collect();

    for start_core_id in 0..cores.len() {
        if post_extension_processed[start_core_id] {
            continue;
        }
        if next_core[start_core_id][0].is_none() {
            continue;
        }

        let mut extension_alignments = vec![];
        let mut extension_limits = vec![];
        let mut extended_core_id = vec![];
        let mut core_to_post_id = start_core_id;
        loop {
            let core_to_post = &cores[core_to_post_id];
            let post_extension_start = core_to_post.aligned_end;
            let post_extension_limit = [0, 1].map(|side| {
                let next_id = next_core[core_to_post_id][side].unwrap();
                let next = &cores[next_id];
                let file_id = core_to_post.file_ids[side];
                index_converters[file_id][side].line_to_word(next.start[side])
            });
            let alignment = aligner.align(core_to_post.file_ids, post_extension_start, post_extension_limit);
            extension_alignments.push(alignment);
            extension_limits.push(post_extension_limit);
            extended_core_id.push(core_to_post_id);
            post_extension_processed[core_to_post_id] = true;

            let core_to_pre_id = next_core[core_to_post_id][0].unwrap();
            let core_to_pre = &cores[core_to_pre_id];
            let pre_extension_end = core_to_pre.aligned_start;
            let pre_extension_limit = [0, 1].map(|side| {
                let prev_id = previous_core[core_to_pre_id][side].unwrap();
                let prev = &cores[prev_id];
                let file_id = core_to_pre.file_ids[side];
                index_converters[file_id][side].line_to_word(prev.end[side])
            });
            let alignment = aligner.align(core_to_pre.file_ids, pre_extension_limit, pre_extension_end);

            extension_alignments.push(alignment);
            extension_limits.push(pre_extension_limit);
            extended_core_id.push(core_to_pre_id);

            core_to_post_id = previous_core[core_to_pre_id][1].unwrap();
            if core_to_post_id == start_core_id {
                break;
            }
        }
        let mut tradeoff_point_candidates = vec![];
        for (i, alignment) in extension_alignments.iter().enumerate() {
            let core_id = extended_core_id[i];
            let core = &cores[core_id];
            let converters = [0, 1].map(|side| &index_converters[core.file_ids[side]][side]);
            tradeoff_point_candidates.push(if i % 2 == 0 {
                // TODO nasty, arguments passing, refactor
                let prefix_scores =
                    aligner.prefix_scores(core.file_ids, core.aligned_end, extension_limits[i], &alignment);
                prefix_tradeoffs(
                    &alignment,
                    &prefix_scores,
                    core.aligned_end,
                    converters,
                    core.file_ids,
                    aligner,
                )
            } else {
                let suffix_scores =
                    aligner.suffix_scores(core.file_ids, extension_limits[i], core.aligned_start, &alignment);
                suffix_tradeoffs(
                    &alignment,
                    &suffix_scores,
                    core.aligned_start,
                    converters,
                    core.file_ids,
                    aligner,
                )
            });
        }

        let (tradeoff_score, tradeoffs) = optimal_tradeoffs(&tradeoff_point_candidates);

        // a 2-cycle means we can potentially join these two cores
        if extension_alignments.len() == 2 {
            let start_core = &cores[start_core_id];
            let other_core_id = next_core[start_core_id][0].unwrap();
            let other_core = &cores[other_core_id];
            let alignment = aligner.align(start_core.file_ids, start_core.aligned_end, other_core.aligned_start);
            let whole_score = *aligner
                .prefix_scores(
                    start_core.file_ids,
                    start_core.aligned_end,
                    other_core.aligned_start,
                    &alignment,
                )
                .last()
                .unwrap();
            if whole_score > tradeoff_score {
                is_first_core_in_chain[other_core_id] = false;
                next_core_in_chain[start_core_id] = Some(other_core_id);
                bridges_after_cores_in_chain[start_core_id] = Some(alignment);
                continue;
            }
        }

        for (i, tradeoff) in tradeoffs.iter().enumerate() {
            let core_id = extended_core_id[i];
            if i % 2 == 0 {
                core_post_extension[core_id] = Vec::from(tradeoff.alignment);
                core_post_extension_to[core_id] = tradeoff.aligned_position;
            } else {
                core_pre_extension[core_id] = Vec::from(tradeoff.alignment);
                core_pre_extension_from[core_id] = tradeoff.aligned_position;
            }
        }
    }

    let mut result = vec![];
    for mut core_id in 0..cores.len() {
        if !is_first_core_in_chain[core_id] {
            continue;
        }

        let file_ids = cores[core_id].file_ids;
        let main = is_main[core_id];

        let aligned_start = core_pre_extension_from[core_id];
        let start = [0, 1].map(|side| index_converters[file_ids[side]][side].word_to_line_before(aligned_start[side]));
        let start_word_indices =
            [0, 1].map(|side| index_converters[file_ids[side]][side].line_to_word(start[side]).raw());

        let mut extended_alignment = vec![];
        for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
            for _ in start_word_indices[side]..aligned_start[side].raw() {
                extended_alignment.push(op);
            }
        }

        extended_alignment.append(&mut core_pre_extension[core_id]);
        extended_alignment.append(&mut cores[core_id].word_alignment);

        while let Some(next_id) = next_core_in_chain[core_id] {
            extended_alignment.append(&mut bridges_after_cores_in_chain[core_id].as_mut().unwrap());
            core_id = next_id;
            extended_alignment.append(&mut cores[core_id].word_alignment);
        }

        extended_alignment.append(&mut core_post_extension[core_id]);

        let aligned_end = core_post_extension_to[core_id];
        let end = [0, 1].map(|side| index_converters[file_ids[side]][side].word_to_line_after(aligned_end[side]));
        let end_word_indices = [0, 1].map(|side| index_converters[file_ids[side]][side].line_to_word(end[side]).raw());

        for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
            for _ in aligned_end[side].raw()..end_word_indices[side] {
                extended_alignment.push(op);
            }
        }

        if start == end {
            continue;
        }

        result.push((
            AlignedFragment {
                starts: start_word_indices,
                ends: end_word_indices,
                file_ids,
                alignment: extended_alignment,
            },
            main,
        ));
    }
    result
}

pub(super) fn main_then_moved(
    text_words: &[[PartitionedText; 2]],
    text_lines: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let scoring = AffineWordScoring::new(text_words);
    let aligner = get_aligner(text_words, &scoring, algorithm);

    let mut alignments: Vec<Vec<DiffOp>> = vec![];
    for (file_id, file_text_words) in text_words.iter().enumerate() {
        alignments.push(aligner.align(
            [file_id, file_id],
            [WordIndex::new(0), WordIndex::new(0)],
            [0, 1].map(|side| WordIndex::new(file_text_words[side].part_count())),
        ));
    }

    let mut main_cores = vec![];
    let mut holes = [vec![], vec![]];

    let mut index_converters: Vec<[IndexConverter; 2]> = vec![];

    for (file_id, alignment) in alignments.iter().enumerate() {
        let file_index_converters = [0, 1].map(|side| {
            IndexConverter::new(
                text_words[file_id][side].part_bounds,
                text_lines[file_id][side].part_bounds,
            )
        });

        let (mut found_cores, mut found_holes) = filter_long_matches(alignment, &file_index_converters, file_id);
        index_converters.push(file_index_converters);
        main_cores.append(&mut found_cores);
        for side in 0..2 {
            holes[side].append(&mut found_holes[side]);
        }
    }
    let moved_cores = moved_cores::find_moved_cores(text_words, &index_converters, &holes);

    extend_cores(main_cores, moved_cores, &index_converters, aligner.as_ref())
}
