use super::{main_sequence::main_sequence_fragments, AlignedFragment, DiffOp, MainSequenceAlgorithm, PartitionedText};
use index_vec::{index_vec, IndexVec};

index_vec::define_index_type! {
    struct LineIndex = usize;
}
index_vec::define_index_type! {
    struct WordIndex = usize;
}

struct Core {
    file_ids: [usize; 2],
    start: [LineIndex; 2],
    end: [LineIndex; 2],

    aligned_start: [WordIndex; 2],
    aligned_end: [WordIndex; 2],
    word_alignment: Vec<DiffOp>,
}

#[allow(dead_code)]
struct Hole {
    file_id: usize,
    start: LineIndex,
    end: LineIndex,
}

fn filter_long_matches(
    word_alignment: &[DiffOp],
    line_to_word_index: &[IndexVec<LineIndex, WordIndex>; 2],
    file_id: usize,
) -> (Vec<Core>, [Vec<Hole>; 2]) {
    #[derive(Clone, Copy, Debug)]
    struct Indices {
        line: [LineIndex; 2],
        word: [WordIndex; 2],
    }

    #[derive(Clone, Copy, Debug)]
    struct ClusterBound {
        text_position: Indices,
        alignment_index: usize,
    }

    let mut current = Indices {
        line: [LineIndex::new(0); 2],
        word: [WordIndex::new(0); 2],
    };
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
            current.word[side] += op.movement()[side];
            while current.line[side] + 1 < line_to_word_index[side].len()
                && line_to_word_index[side][current.line[side] + 1] <= current.word[side]
            {
                current.line[side] += 1;
            }
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
                        let mut whole_lines_skipped: usize =
                            usize::from(current.line[side] - end.text_position.line[side]);
                        if line_to_word_index[side][end.text_position.line[side]] != end.text_position.word[side] {
                            whole_lines_skipped = whole_lines_skipped.saturating_sub(1);
                        }
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
        let mut start_lines = start.text_position.line;
        let mut start_words = start.text_position.word;
        for side in 0..2 {
            if line_to_word_index[side][start_lines[side]] < start_words[side] {
                start_lines[side] += 1;
            }
        }
        let mut start_i = start.alignment_index;
        let mut end_i = end.alignment_index;
        while (start_words[0] < line_to_word_index[0][start_lines[0]]
            || start_words[1] < line_to_word_index[1][start_lines[1]])
            && start_i < end_i
        {
            for side in 0..2 {
                start_words[side] += word_alignment[start_i].movement()[side];
                if start_lines[side] + 1 < line_to_word_index[side].len()
                    && start_words[side] >= line_to_word_index[side][start_lines[side] + 1]
                {
                    start_lines[side] += 1;
                }
            }
            start_i += 1;
        }

        let mut end_words = end.text_position.word;
        let mut end_lines = end.text_position.line;
        while (end_words[0] > line_to_word_index[0][end_lines[0]] || end_words[1] > line_to_word_index[1][end_lines[1]])
            && end_i > start_i
        {
            for side in 0..2 {
                end_words[side] -= word_alignment[end_i - 1].movement()[side];
                if end_lines[side] > 0 && end_words[side] <= line_to_word_index[side][end_lines[side] - 1] {
                    end_lines[side] -= 1;
                }
            }
            end_i -= 1;
        }
        const MIN_LINES_IN_CORE: usize = 5;
        if end_i > start_i
            && end_lines[0] - start_lines[0] >= MIN_LINES_IN_CORE
            && end_lines[1] - start_lines[1] >= MIN_LINES_IN_CORE
        {
            for side in 0..2 {
                if start_lines[side] > current_hole_start[side] {
                    holes[side].push(Hole {
                        file_id,
                        start: current_hole_start[side],
                        end: start_lines[side],
                    });
                }
            }
            cores.push(Core {
                file_ids: [file_id; 2],
                start: start_lines,
                end: end_lines,
                aligned_start: start_words,
                aligned_end: end_words,
                word_alignment: Vec::from(&word_alignment[start_i..end_i]),
            });
            current_hole_start = end_lines;
        }
    }
    for side in 0..2 {
        if line_to_word_index[side].len() - 1 > current_hole_start[side] {
            holes[side].push(Hole {
                file_id,
                start: current_hole_start[side],
                end: LineIndex::new(line_to_word_index[side].len() - 1),
            });
        }
    }

    (cores, holes)
}

fn find_moved_cores(_holes: &[Vec<Hole>; 2]) -> Vec<Core> {
    vec![]
}

fn extend_cores(
    main_cores: Vec<Core>,
    _moved_cores: Vec<Core>,
    line_to_word_index: &[[IndexVec<LineIndex, WordIndex>; 2]],
) -> Vec<(AlignedFragment, bool)> {
    main_cores
        .into_iter()
        .map(|mut core| {
            let core_start_word_indices =
                [0, 1].map(|side| line_to_word_index[core.file_ids[side]][side][core.start[side]].raw());

            let mut alignment = vec![];
            for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
                for _ in core_start_word_indices[side]..core.aligned_start[side].raw() {
                    alignment.push(op);
                }
            }
            alignment.append(&mut core.word_alignment);

            let core_end_word_indices =
                [0, 1].map(|side| line_to_word_index[core.file_ids[side]][side][core.end[side]].raw());

            for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
                for _ in core.aligned_end[side].raw()..core_end_word_indices[side] {
                    alignment.push(op);
                }
            }

            (
                AlignedFragment {
                    starts: core_start_word_indices,
                    ends: core_end_word_indices,
                    file_ids: core.file_ids,
                    alignment,
                },
                true,
            )
        })
        .collect()
}

pub(super) fn main_then_moved(
    text_words: &[[PartitionedText; 2]],
    text_lines: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let alignments: Vec<Vec<DiffOp>> = main_sequence_fragments(text_words, algorithm)
        .into_iter()
        .map(|(fragment, _is_main)| fragment.alignment)
        .collect(); // TODO: this is ridiculous, refactor main sequence algorithm ecosystem

    let mut main_cores = vec![];
    let mut holes = [vec![], vec![]];
    let mut line_to_word_index: Vec<[IndexVec<LineIndex, WordIndex>; 2]> = vec![];
    for (file_id, alignment) in alignments.iter().enumerate() {
        let mut file_line_to_word_index = [index_vec![], index_vec![]];

        for side in 0..2 {
            let mut line_index = 0;
            for (word_index, &byte_index) in text_words[file_id][side].part_bounds.iter().enumerate() {
                if byte_index == text_lines[file_id][side].part_bounds[line_index] {
                    file_line_to_word_index[side].push(WordIndex::new(word_index));
                    line_index += 1;
                }
            }
        }

        let (mut found_cores, mut found_holes) = filter_long_matches(alignment, &file_line_to_word_index, file_id);
        line_to_word_index.push(file_line_to_word_index);
        main_cores.append(&mut found_cores);
        for side in 0..2 {
            holes[side].append(&mut found_holes[side]);
        }
    }
    let moved_cores = find_moved_cores(&holes);

    extend_cores(main_cores, moved_cores, &line_to_word_index)
}
