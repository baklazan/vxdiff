use std::ops::{Range, RangeInclusive};

use self::index_converter::IndexConverter;

use super::{main_sequence::main_sequence_fragments, AlignedFragment, DiffOp, MainSequenceAlgorithm, PartitionedText};

mod index_converter;
mod moved_cores;

const MIN_LINES_IN_CORE: usize = 3;

trait UsizeConvertible {
    fn from_usize(val: usize) -> Self;
    fn to_usize(&self) -> usize;
}

macro_rules! extend_index_type {
    (
        $type:ident
    ) => {
        impl UsizeConvertible for $type {
            fn from_usize(val: usize) -> Self {
                Self::new(val)
            }

            fn to_usize(&self) -> usize {
                self.raw()
            }
        }

        #[allow(dead_code)]
        impl $type {
            fn saturating_sub(&self, other: Self) -> Self {
                Self::new(self.raw().saturating_sub(other.raw()))
            }
        }
    };
}

index_vec::define_index_type! {
    struct LineIndex = usize;
}
extend_index_type!(LineIndex);

index_vec::define_index_type! {
    struct WordIndex = usize;
}
extend_index_type!(WordIndex);

fn range_iter<Index: UsizeConvertible>(range: Range<Index>) -> impl Iterator<Item = Index> {
    (range.start.to_usize()..range.end.to_usize()).map(Index::from_usize)
}

fn range_incl_iter<Index: UsizeConvertible>(range: RangeInclusive<Index>) -> impl Iterator<Item = Index> {
    (range.start().to_usize()..=range.end().to_usize()).map(Index::from_usize)
}

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
        let mut start_lines = [0, 1].map(|side| index_converters[side].word_to_line_after(start.text_position[side]));
        let mut start_words = start.text_position;
        let mut start_i = start.alignment_index;
        let mut end_i = end.alignment_index;
        while (start_words[0] < index_converters[0].line_to_word(start_lines[0])
            || start_words[1] < index_converters[1].line_to_word(start_lines[1]))
            && start_i < end_i
        {
            for side in 0..2 {
                start_words[side] += word_alignment[start_i].movement()[side];
            }
            start_i += 1;
        }

        let mut end_words = end.text_position;
        let mut end_lines = [0, 1].map(|side| index_converters[side].word_to_line_before(end.text_position[side]));
        while (end_words[0] > index_converters[0].line_to_word(end_lines[0])
            || end_words[1] > index_converters[1].line_to_word(end_lines[1]))
            && end_i > start_i
        {
            for side in 0..2 {
                end_words[side] -= word_alignment[end_i - 1].movement()[side];
            }
            end_i -= 1;
        }
        for side in 0..2 {
            start_lines[side] = index_converters[side].word_to_line_before(start_words[side]);
            end_lines[side] = index_converters[side].word_to_line_after(end_words[side]);
        }

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

fn extend_cores(
    main_cores: Vec<Core>,
    mut moved_cores: Vec<Core>,
    index_converters: &[[IndexConverter; 2]],
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

    println!("{} cores", cores.len());

    let mut previous_core: Vec<[Option<usize>; 2]> = vec![[None; 2]; cores.len()];
    let mut next_core: Vec<[Option<usize>; 2]> = vec![[None; 2]; cores.len()];

    for side in 0..2 {
        let mut core_ends_by_side = vec![];
        for (core_id, core) in cores.iter().enumerate() {
            core_ends_by_side.push((core.file_ids[side], core.start[side], core_id));
            core_ends_by_side.push((core.file_ids[side], core.end[side], core_id));
        }
        core_ends_by_side.sort();
        for i in (1..core_ends_by_side.len() - 1).step_by(2) {
            let earlier_core_id = core_ends_by_side[i].2;
            let later_core_id = core_ends_by_side[i + 1].2;
            previous_core[later_core_id][side] = Some(earlier_core_id);
            next_core[earlier_core_id][side] = Some(later_core_id);
        }

        // detect and relabel moved cores that can be inserted into the main sequence
        // TODO: switch from greedy to DP (can be done in O(n log n))
        // TODO: extract function
        if side == 0 {
            let mut next_main_core = vec![None; cores.len()];
            let mut last_main_seen = None;
            for i in (0..core_ends_by_side.len()).step_by(2).rev() {
                let core_id = core_ends_by_side[i].2;
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
            for i in (0..core_ends_by_side.len()).step_by(2) {
                let core_id = core_ends_by_side[i].2;
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

    let mut result = vec![];
    for (core_id, mut core) in cores.into_iter().enumerate() {
        if core.start == core.end {
            continue;
        }
        let main = is_main[core_id];
        println!("Core from {:?} to {:?}, main: {}", core.start, core.end, main);

        let core_start_word_indices = [0, 1].map(|side| {
            index_converters[core.file_ids[side]][side]
                .line_to_word(core.start[side])
                .raw()
        });
        let mut alignment = vec![];
        for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
            for _ in core_start_word_indices[side]..core.aligned_start[side].raw() {
                alignment.push(op);
            }
        }
        alignment.append(&mut core.word_alignment);

        let core_end_word_indices = [0, 1].map(|side| {
            index_converters[core.file_ids[side]][side]
                .line_to_word(core.end[side])
                .raw()
        });

        for (side, op) in [(0, DiffOp::Delete), (1, DiffOp::Insert)] {
            for _ in core.aligned_end[side].raw()..core_end_word_indices[side] {
                alignment.push(op);
            }
        }
        result.push((
            AlignedFragment {
                starts: core_start_word_indices,
                ends: core_end_word_indices,
                file_ids: core.file_ids,
                alignment,
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
    let alignments: Vec<Vec<DiffOp>> = main_sequence_fragments(text_words, algorithm)
        .into_iter()
        .map(|(fragment, _is_main)| fragment.alignment)
        .collect(); // TODO: this is ridiculous, refactor main sequence algorithm ecosystem

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

    extend_cores(main_cores, moved_cores, &index_converters)
}
