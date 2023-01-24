use std::{cmp::Ordering, collections::HashMap, ops::Add};

use crate::algorithm::{indices::range_iter, main_sequence::Aligner, suffix_array, PartitionedText};
use index_vec::{index_vec, IndexVec};
use string_interner::{backend::StringBackend, StringInterner};

use super::{trim_to_core, Core, Hole, IndexConverter, LineIndex, WordIndex, MIN_LINES_IN_CORE};

fn next_lower(array: &[usize]) -> Vec<usize> {
    let mut stack: Vec<(usize, usize)> = vec![];
    let mut result = vec![0; array.len()];
    for (i, &value) in array.iter().enumerate().rev() {
        while let Some(&(stack_value, _)) = stack.last() {
            if stack_value < value {
                break;
            }
            stack.pop();
        }
        result[i] = stack.last().unwrap_or(&(0, array.len())).1;
        stack.push((value, i));
    }
    result
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Seed {
    file_ids: [usize; 2],
    start: [WordIndex; 2],
    end: [WordIndex; 2],
}

fn find_seeds(
    text_words: &[[PartitionedText; 2]],
    index_converters: &[[IndexConverter; 2]],
    holes: &[Vec<Hole>; 2],
) -> Vec<Seed> {
    let mut interner: StringInterner<StringBackend<usize>> = StringInterner::new();

    #[derive(Clone, Copy)]
    struct Position {
        file_id: usize,
        side: usize,
        word: WordIndex,
    }

    let mut origin: Vec<Position> = vec![];
    let mut words_till_hole_end: Vec<usize> = vec![];
    let mut string: Vec<usize> = vec![];
    for (side, side_holes) in holes.iter().enumerate() {
        for hole in side_holes {
            let start_word: WordIndex = index_converters[hole.file_id][side].line_to_word(hole.start);
            let end_word: WordIndex = index_converters[hole.file_id][side].line_to_word(hole.end);
            for word in range_iter(start_word..end_word) {
                let symbol = interner.get_or_intern(text_words[hole.file_id][side].get_part(word.raw()));
                string.push(symbol + 2);
                origin.push(Position {
                    file_id: hole.file_id,
                    side,
                    word,
                });
                words_till_hole_end.push((end_word - word).raw());
            }
            string.push(1);
            origin.push(Position {
                file_id: hole.file_id,
                side,
                word: end_word,
            });
            words_till_hole_end.push(0);
        }
    }
    if string.is_empty() {
        return vec![];
    }
    let suffix_array = suffix_array::suffix_array(&string);
    let mut new_suffixes_until = vec![0; suffix_array.len() + 1];
    for (i, &string_index) in suffix_array.iter().enumerate() {
        new_suffixes_until[i + 1] = new_suffixes_until[i] + origin[string_index].side;
    }

    let mut lcp_array = suffix_array::longest_common_prefix_array(&suffix_array, &string);
    lcp_array.push(0);
    let next_lower_lcp = next_lower(&lcp_array);

    const INTERESTING_MAX_OCCURENCES: usize = 3;
    let mut sa_index = 0;
    let mut min_seed_length = 1;
    let mut result = vec![];
    while sa_index < suffix_array.len() {
        let mut until_inclusive = sa_index;
        let mut length = usize::max(min_seed_length, lcp_array[until_inclusive] + 1);

        while until_inclusive + 1 < suffix_array.len() && lcp_array[until_inclusive] >= min_seed_length {
            let next_until_inclusive = next_lower_lcp[until_inclusive];
            let total_suffixes = next_until_inclusive + 1 - sa_index;
            let new_suffixes = new_suffixes_until[next_until_inclusive + 1] - new_suffixes_until[sa_index];
            let old_suffixes = total_suffixes - new_suffixes;
            if new_suffixes > INTERESTING_MAX_OCCURENCES && old_suffixes > INTERESTING_MAX_OCCURENCES {
                break;
            }
            until_inclusive = next_until_inclusive;
            length = usize::max(min_seed_length, lcp_array[until_inclusive] + 1);
        }

        let total_count = until_inclusive + 1 - sa_index;
        let new_suffixes_count = new_suffixes_until[until_inclusive + 1] - new_suffixes_until[sa_index];
        let old_suffixes_count = total_count - new_suffixes_count;
        if (new_suffixes_count <= INTERESTING_MAX_OCCURENCES || old_suffixes_count <= INTERESTING_MAX_OCCURENCES)
            && length <= words_till_hole_end[suffix_array[sa_index]]
        {
            let mut side_suffixes = [vec![], vec![]];
            for i in sa_index..=until_inclusive {
                side_suffixes[origin[suffix_array[i]].side].push(suffix_array[i]);
            }
            for &old_suffix in side_suffixes[0].iter() {
                for &new_suffix in side_suffixes[1].iter() {
                    result.push(Seed {
                        file_ids: [origin[old_suffix].file_id, origin[new_suffix].file_id],
                        start: [origin[old_suffix].word, origin[new_suffix].word],
                        end: [origin[old_suffix].word + length, origin[new_suffix].word + length],
                    });
                }
            }
        }
        min_seed_length = lcp_array[until_inclusive] + 1;
        sa_index = until_inclusive + 1;
    }
    result
}

#[derive(Debug)]
struct MatchInterval {
    start: [WordIndex; 2],
    end: [WordIndex; 2],
}

#[derive(Debug)]
struct Sapling {
    file_ids: [usize; 2],
    intervals: Vec<MatchInterval>,
}

impl Sapling {
    fn merge(a: &[([WordIndex; 2], i8)], b: &[([WordIndex; 2], i8)]) -> Vec<([WordIndex; 2], i8)> {
        let mut a_iter = a.iter().peekable();
        let mut b_iter = b.iter().peekable();
        let mut result = vec![];
        while a_iter.peek().is_some() || b_iter.peek().is_some() {
            if a_iter.peek().is_none() {
                result.push(*b_iter.next().unwrap());
                continue;
            }
            if b_iter.peek().is_none() {
                result.push(*a_iter.next().unwrap());
                continue;
            }
            let a_value = a_iter.peek().unwrap().0;
            let b_value = b_iter.peek().unwrap().0;
            if a_value[0] + a_value[1] < b_value[0] + b_value[1] {
                result.push(*a_iter.next().unwrap());
            } else {
                result.push(*b_iter.next().unwrap());
            }
        }
        result
    }

    pub fn intersect_intervals(
        &self,
        side_intervals: &[Vec<(WordIndex, WordIndex)>; 2],
    ) -> Vec<([WordIndex; 2], [WordIndex; 2])> {
        let mut side_events = [vec![], vec![]];
        for side in 0..2 {
            let mut match_interval_id = 0;
            for &(start, end) in side_intervals[side].iter() {
                while self.intervals[match_interval_id].end[side] <= start {
                    match_interval_id += 1;
                }
                let match_interval = &self.intervals[match_interval_id];
                if match_interval.start[side] < start {
                    let offset = start - match_interval.start[side];
                    side_events[side].push(([0, 1].map(|s| match_interval.start[s] + offset), 1));
                } else {
                    side_events[side].push((match_interval.start, 1));
                }
                while match_interval_id + 1 < self.intervals.len()
                    && self.intervals[match_interval_id + 1].start[side] < end
                {
                    match_interval_id += 1;
                }
                let match_interval = &self.intervals[match_interval_id];
                if match_interval.end[side] > end {
                    let offset = match_interval.end[side] - end;
                    side_events[side].push(([0, 1].map(|s| match_interval.end[s] - offset), -1));
                } else {
                    side_events[side].push((match_interval.end, -1));
                }
            }
        }

        let mut result = vec![];
        let all_events = Self::merge(&side_events[0], &side_events[1]);
        let mut coverage = 0;
        for (i, event) in all_events.iter().enumerate() {
            coverage += event.1;
            if coverage == 2 {
                let next_event = &all_events[i + 1];
                result.push((event.0, next_event.0));
            }
        }
        result
    }
}

fn build_saplings_in_file_combination(seeds: &[Seed], file_size: [usize; 2]) -> Vec<Sapling> {
    let diagonals_count = file_size[0] + file_size[1] + 1;

    #[derive(Clone)]
    struct Edge {
        to_id: usize,
        skipped_prefix: usize,
    }
    let mut edges: Vec<Vec<Edge>> = vec![vec![]; seeds.len()];

    const MAX_GAP_BETWEEN_SEEDS: usize = 10;
    for side in 0..2 {
        let mut last_seen: Vec<Option<usize>> = vec![None; diagonals_count];

        let mut ids_by_dimension: Vec<_> = seeds
            .iter()
            .enumerate()
            .map(|(i, seed)| (file_size[side] - seed.start[side], seed.start[1 - side], i))
            .collect();
        ids_by_dimension.sort();

        for (_, _, seed_id) in ids_by_dimension {
            let seed = &seeds[seed_id];
            let diagonal = seed.start[1 - side].raw() + file_size[side] - seed.start[side].raw();
            for to_diagonal in diagonal..=usize::min(diagonal + MAX_GAP_BETWEEN_SEEDS, diagonals_count) {
                if let Some(to_id) = last_seen[to_diagonal] {
                    let to_seed = &seeds[to_id];
                    let gap = usize::max(
                        to_diagonal - diagonal,
                        (to_seed.start[0] + to_seed.start[1])
                            .raw()
                            .saturating_sub((seed.end[0] + seed.end[1]).raw()),
                    );
                    if gap <= MAX_GAP_BETWEEN_SEEDS {
                        let skipped_prefix = seed.end[side].raw().saturating_sub(to_seed.start[side].raw());
                        edges[seed_id].push(Edge { to_id, skipped_prefix });
                    }
                }
            }
            last_seen[diagonal] = Some(seed_id);
        }
    }

    struct StartForRoot {
        start_id: usize,
        score: usize,
    }
    let mut score = vec![0; seeds.len()];
    let mut next: Vec<Option<usize>> = vec![None; seeds.len()];
    let mut root = vec![0; seeds.len()];
    let mut best_start_for_root: HashMap<usize, StartForRoot> = HashMap::new();

    for (seed_id, seed) in seeds.iter().enumerate().rev() {
        let seed_length = (seed.end[0] - seed.start[0]).raw();
        score[seed_id] = seed_length;
        root[seed_id] = seed_id;
        for edge in edges[seed_id].iter() {
            let proposed_score = seed_length + score[edge.to_id] - edge.skipped_prefix;
            if proposed_score > score[seed_id] {
                score[seed_id] = proposed_score;
                next[seed_id] = Some(edge.to_id);
                root[seed_id] = root[edge.to_id];
            }
        }
        let former_root_best_start = best_start_for_root
            .get(&root[seed_id])
            .unwrap_or(&StartForRoot { start_id: 0, score: 0 });
        if score[seed_id] > former_root_best_start.score {
            best_start_for_root.insert(
                root[seed_id],
                StartForRoot {
                    start_id: seed_id,
                    score: score[seed_id],
                },
            );
        }
    }

    let mut result = vec![];
    for (_, start) in best_start_for_root {
        let mut intervals: Vec<MatchInterval> = vec![];
        let mut current_id = start.start_id;
        loop {
            let current_seed = &seeds[current_id];
            if let Some(previous_interval) = intervals.last_mut() {
                let current_diagonal = current_seed.start[0].raw().wrapping_sub(current_seed.start[1].raw());
                let previous_diagonal = previous_interval.start[0]
                    .raw()
                    .wrapping_sub(previous_interval.start[1].raw());
                if current_diagonal == previous_diagonal {
                    if previous_interval.end[0] >= current_seed.start[0] {
                        previous_interval.end = current_seed.end;
                    } else {
                        intervals.push(MatchInterval {
                            start: current_seed.start,
                            end: current_seed.end,
                        });
                    }
                } else {
                    let mut overlap = 0;
                    for side in 0..2 {
                        overlap = usize::max(
                            overlap,
                            previous_interval.end[side]
                                .raw()
                                .saturating_sub(current_seed.start[side].raw()),
                        );
                    }
                    intervals.push(MatchInterval {
                        start: [0, 1].map(|side| current_seed.start[side] + overlap),
                        end: current_seed.end,
                    });
                }
            } else {
                intervals.push(MatchInterval {
                    start: current_seed.start,
                    end: current_seed.end,
                });
            }

            if let Some(next_id) = next[current_id] {
                current_id = next_id;
            } else {
                break;
            }
        }
        result.push(Sapling {
            intervals,
            file_ids: seeds[0].file_ids,
        });
    }
    result
}

fn build_saplings(mut seeds: Vec<Seed>, file_sizes: &[[usize; 2]]) -> Vec<Sapling> {
    // note: this could be replaced by a radix sort by file_ids and a topological sort in each file combination
    seeds.sort_by(|a, b| -> Ordering {
        for side in 0..2 {
            if a.file_ids[side] != b.file_ids[side] {
                return usize::cmp(&a.file_ids[side], &b.file_ids[side]);
            }
        }
        WordIndex::cmp(&(a.start[0] + a.start[1]), &(b.start[0] + b.start[1]))
    });
    let mut result = vec![];
    let mut current_combination_start = 0;
    for (i, seed) in seeds.iter().enumerate() {
        if i + 1 >= seeds.len() || seed.file_ids != seeds[i + 1].file_ids {
            let mut saplings = build_saplings_in_file_combination(
                &seeds[current_combination_start..=i],
                [0, 1].map(|side| file_sizes[seed.file_ids[side]][side]),
            );
            result.append(&mut saplings);
            current_combination_start = i + 1;
        }
    }
    result
}

struct LineCoverage {
    // TODO: implement as a segment tree
    coverage: IndexVec<LineIndex, usize>,
}

impl LineCoverage {
    fn new(len: LineIndex) -> Self {
        LineCoverage {
            coverage: index_vec![0; len.raw()],
        }
    }

    fn add(&mut self, from: LineIndex, to: LineIndex) {
        for i in range_iter(from..to) {
            self.coverage[i] += 1;
        }
    }

    fn remove(&mut self, from: LineIndex, to: LineIndex) {
        for i in range_iter(from..to) {
            self.coverage[i] -= 1;
        }
    }

    fn once_covered_invervals(&self, from: LineIndex, to: LineIndex) -> Vec<(LineIndex, LineIndex)> {
        let mut current_interval_start = None;
        let mut result = vec![];
        for i in range_iter(from..to) {
            if self.coverage[i] == 1 {
                if current_interval_start.is_none() {
                    current_interval_start = Some(i);
                }
            }
            if let Some(interval_start) = current_interval_start {
                if i + 1 >= to || self.coverage[i + 1] != 1 {
                    result.push((interval_start, i + 1));
                    current_interval_start = None;
                }
            }
        }
        result
    }
}

fn select_cores(saplings: &[Sapling], index_converters: &[[IndexConverter; 2]], aligner: &dyn Aligner) -> Vec<Core> {
    let mut coverage: Vec<_> = index_converters
        .iter()
        .map(|file_converters| [0, 1].map(|side| LineCoverage::new(file_converters[side].lines_count())))
        .collect();

    let mut by_score = vec![];
    for (sapling_id, sapling) in saplings.iter().enumerate() {
        for side in 0..2 {
            let file_id = sapling.file_ids[side];
            let start_word = sapling.intervals[0].start[side];
            let end_word = sapling.intervals.last().unwrap().end[side];
            let start_line = index_converters[file_id][side].word_to_line_before(start_word);
            let end_line = index_converters[file_id][side].word_to_line_after(end_word);
            coverage[file_id][side].add(start_line, end_line);
        }

        let score = sapling
            .intervals
            .iter()
            .map(|interval| interval.end[0] - interval.start[0])
            .fold(WordIndex::new(0), WordIndex::add);
        by_score.push((score, sapling_id));
    }

    by_score.sort();

    let mut result = vec![];
    for (_, sapling_id) in by_score {
        let sapling = &saplings[sapling_id];
        let start_words = sapling.intervals[0].start;
        let end_words = sapling.intervals.last().unwrap().end;

        let mut unique_word_intervals: [Vec<(WordIndex, WordIndex)>; 2] = [vec![], vec![]];
        for side in 0..2 {
            let file_id = sapling.file_ids[side];
            let start_line = index_converters[file_id][side].word_to_line_before(start_words[side]);
            let end_line = index_converters[file_id][side].word_to_line_after(end_words[side]);
            let unique_line_intervals = coverage[file_id][side].once_covered_invervals(start_line, end_line);
            unique_word_intervals[side] = unique_line_intervals
                .iter()
                .map(|(from, to)| {
                    (
                        index_converters[file_id][side].line_to_word(*from),
                        index_converters[file_id][side].line_to_word(*to),
                    )
                })
                .collect();
        }
        let unique_sapling_intervals = sapling.intersect_intervals(&unique_word_intervals);
        let mut sapling_produced_core = false;
        for (start, end) in unique_sapling_intervals {
            let alignment = aligner.align(sapling.file_ids, start, end);
            let core = trim_to_core(
                &alignment,
                start,
                end,
                [
                    &index_converters[sapling.file_ids[0]][0],
                    &index_converters[sapling.file_ids[1]][1],
                ],
                sapling.file_ids,
            );

            let mut good = true;
            for side in 0..2 {
                if core.end[side] - core.start[side] < MIN_LINES_IN_CORE {
                    good = false;
                }
            }

            if good {
                result.push(core);
                sapling_produced_core = true;
            }
        }
        if !sapling_produced_core {
            for side in 0..2 {
                let file_id = sapling.file_ids[side];
                let start_line = index_converters[file_id][side].word_to_line_before(start_words[side]);
                let end_line = index_converters[file_id][side].word_to_line_after(end_words[side]);
                coverage[file_id][side].remove(start_line, end_line);
            }
        }
    }
    result
}

pub(super) fn find_moved_cores(
    text_words: &[[PartitionedText; 2]],
    index_converters: &[[IndexConverter; 2]],
    holes: &[Vec<Hole>; 2],
    aligner: &dyn Aligner,
) -> Vec<Core> {
    let seeds = find_seeds(text_words, index_converters, holes);
    let file_sizes_words: Vec<[usize; 2]> = text_words
        .iter()
        .map(|file_words| [0, 1].map(|side| file_words[side].part_count()))
        .collect();
    let saplings = build_saplings(seeds, &file_sizes_words);
    select_cores(&saplings, index_converters, aligner)
}
