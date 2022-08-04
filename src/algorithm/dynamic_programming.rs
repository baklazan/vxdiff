use super::seed_selection::*;

use super::scoring::*;
use super::*;
use std::fs::File;
use std::io::Write;

#[derive(Debug)]
pub struct OriginalCandidate {
    pub starts: [usize; 2],
    pub ends: [usize; 2],
    pub alignment: Vec<DiffOp>,
    pub prefix_scores: Vec<TScore>,
    pub word_to_alignment: [Vec<usize>; 2], // words map to alignment steps
    pub alignment_to_word: [Vec<usize>; 2], // points between alignment steps map to points between words
}

impl OriginalCandidate {
    pub fn word_interval_to_alignment_interval(&self, interval: (usize, usize), side: usize) -> (usize, usize) {
        let interval = clamp_interval(interval, (self.starts[side], self.ends[side]));
        if interval.0 >= self.ends[side] {
            return (self.alignment.len(), self.alignment.len());
        }
        if interval.1 <= self.starts[side] {
            return (0, 0);
        }
        let first_word_index = interval.0 - self.starts[side];
        let last_word_index = interval.1 - 1 - self.starts[side];
        (
            self.word_to_alignment[side][first_word_index],
            self.word_to_alignment[side][last_word_index] + 1,
        )
    }
}

#[derive(Clone)]
pub struct Interval {
    start: usize,
    end: usize,
}

type RowSubset = Vec<Interval>;

struct RowIterator<'a> {
    row_subset: &'a RowSubset,
    interval_index: usize,
    index_in_interval: usize,
    index_in_row: usize,
}

impl<'a> RowIterator<'a> {
    const INVALID_INDEX: usize = usize::max_value();

    pub fn new(row_subset: &RowSubset) -> RowIterator {
        if row_subset.is_empty() {
            RowIterator {
                row_subset,
                interval_index: RowIterator::INVALID_INDEX,
                index_in_interval: 0,
                index_in_row: RowIterator::INVALID_INDEX,
            }
        } else {
            let interval = &row_subset.last().unwrap();
            RowIterator {
                row_subset,
                interval_index: row_subset.len() - 1,
                index_in_interval: interval.end - interval.start - 1,
                index_in_row: interval.end - 1,
            }
        }
    }

    pub fn new_at_index(row_subset: &RowSubset, index_in_row: usize) -> RowIterator {
        let mut low = 0;
        let mut high = row_subset.len();
        while low + 1 < high {
            let mid = (low + high) / 2;
            if row_subset[mid].start > index_in_row {
                high = mid;
                continue;
            }
            if row_subset[mid].end <= index_in_row {
                low = mid + 1;
                continue;
            }
            low = mid;
            break;
        }
        RowIterator {
            row_subset,
            interval_index: low,
            index_in_interval: index_in_row - row_subset[low].start,
            index_in_row,
        }
    }

    pub fn is_valid(&self) -> bool {
        if self.interval_index >= self.row_subset.len() {
            return false;
        }
        let interval = &self.row_subset[self.interval_index];
        self.index_in_interval < interval.end - interval.start
    }

    pub fn descend_to_previous_valid(&mut self) {
        if self.interval_index >= self.row_subset.len() {
            return;
        }
        let interval = &self.row_subset[self.interval_index];
        if self.index_in_interval >= interval.end - interval.start {
            self.index_in_interval = interval.end - interval.start - 1;
            self.index_in_row = interval.start + self.index_in_interval;
            return;
        }
        if self.index_in_interval > 0 {
            self.index_in_interval -= 1;
            self.index_in_row -= 1;
            return;
        }
        if self.interval_index > 0 {
            self.interval_index -= 1;
            let interval = &self.row_subset[self.interval_index];
            self.index_in_interval = interval.end - interval.start - 1;
            self.index_in_row = interval.start + self.index_in_interval;
            return;
        }
        self.interval_index = RowIterator::INVALID_INDEX;
    }

    pub fn move_to_position(&mut self, position: usize) {
        // TODO: make this actually work
        // currently only works for moving left and for moving one position to the right
        if position > self.index_in_row {
            if self.interval_index < self.row_subset.len() {
                let interval = &self.row_subset[self.interval_index];
                self.index_in_interval = position - interval.start;
                self.index_in_row = position;
            }
            return;
        }

        loop {
            if self.interval_index >= self.row_subset.len() {
                return;
            }
            let interval = &self.row_subset[self.interval_index];
            if interval.start <= position {
                self.index_in_interval = position - interval.start;
                self.index_in_row = position;
                return;
            }
            if self.interval_index > 0 {
                self.interval_index -= 1;
            } else {
                self.interval_index = RowIterator::INVALID_INDEX;
                self.index_in_row = RowIterator::INVALID_INDEX;
                return;
            }
        }
    }
}

type MatrixSubset = Vec<RowSubset>;

struct SparseMatrix<T: Clone> {
    values: Vec<Vec<Vec<T>>>,
}

struct MatrixIndex<'a> {
    pub row_iterator: RowIterator<'a>,
    pub row_index: usize,
}

impl<T: Clone> SparseMatrix<T> {
    pub fn new(element_locations: &MatrixSubset, default_value: &T) -> SparseMatrix<T> {
        let mut result = vec![];
        for row in element_locations {
            let mut result_row = vec![];
            for interval in row {
                result_row.push(vec![default_value.clone(); interval.end - interval.start]);
            }
            result.push(result_row);
        }
        SparseMatrix { values: result }
    }
}

impl<'a, T: Clone> std::ops::Index<&MatrixIndex<'a>> for SparseMatrix<T> {
    type Output = T;

    fn index(&self, index: &MatrixIndex) -> &T {
        &self.values[index.row_index][index.row_iterator.interval_index][index.row_iterator.index_in_interval]
    }
}

impl<'a, T: Clone> std::ops::IndexMut<&MatrixIndex<'a>> for SparseMatrix<T> {
    fn index_mut(&mut self, index: &MatrixIndex) -> &mut T {
        &mut self.values[index.row_index][index.row_iterator.interval_index][index.row_iterator.index_in_interval]
    }
}

fn consolidate_row(intervals: &Vec<Interval>) -> RowSubset {
    #[derive(Ord, Eq, PartialEq, PartialOrd)]
    enum Type {
        // Force correct sorting of events (open events should come first,
        // so that we concatenate touching intervals)
        Open = 0,
        Close = 1,
    }
    let mut events = vec![];

    for interval in intervals {
        events.push((interval.start, Type::Open));
        events.push((interval.end, Type::Close));
    }
    events.sort();
    let mut inside = 0;
    let mut current_start = None;
    let mut result = vec![];
    for event in events {
        match event.1 {
            Type::Open => {
                if inside == 0 {
                    current_start = Some(event.0);
                }
                inside += 1;
            }
            Type::Close => {
                inside -= 1;
                if inside == 0 {
                    result.push(Interval {
                        start: current_start.unwrap(),
                        end: event.0,
                    });
                    current_start = None;
                }
            }
        }
    }
    result
}

fn covered_by_seeds(sizes: [usize; 2], seeds: &Vec<Seed>) -> MatrixSubset {
    let mut rows = vec![vec![]; sizes[0]];
    for seed in seeds {
        for i in 0..=seed.end[0] - seed.start[0] {
            rows[seed.start[0] + i].push(Interval {
                start: seed.start[1] + i,
                end: seed.start[1] + i + 1,
            });
        }
    }

    for row in rows.iter_mut() {
        *row = consolidate_row(row);
    }
    rows
}

fn seed_extensions(sizes: [usize; 2], seeds: &Vec<Seed>) -> MatrixSubset {
    let mut rows = vec![vec![]; sizes[0]];
    const BOX_RADIUS: i64 = 30;

    let mut plot_box = |midpoint: [usize; 2]| {
        let midpoint = [i64::try_from(midpoint[0]).unwrap(), i64::try_from(midpoint[1]).unwrap()];
        let low_y = std::cmp::max(midpoint[0] - BOX_RADIUS, 0);
        let high_y = std::cmp::min(i64::try_from(sizes[0]).unwrap(), midpoint[0] + BOX_RADIUS + 1);
        for row in low_y..high_y {
            let low_x = std::cmp::max(midpoint[1] - BOX_RADIUS, 0);
            let high_x = std::cmp::min(i64::try_from(sizes[1]).unwrap(), midpoint[1] + BOX_RADIUS + 1);
            rows[usize::try_from(row).unwrap()].push(Interval {
                start: usize::try_from(low_x).unwrap(),
                end: usize::try_from(high_x).unwrap(),
            });
        }
    };

    for seed in seeds {
        plot_box(seed.start);
        plot_box(seed.end);
    }
    for row in rows.iter_mut() {
        *row = consolidate_row(row);
    }
    rows
}

fn union(mut left: MatrixSubset, mut right: MatrixSubset) -> MatrixSubset {
    let mut result = vec![];
    for (left_row, right_row) in left.iter_mut().zip(right.iter_mut()) {
        left_row.append(right_row);
        result.push(consolidate_row(left_row));
    }
    result
}

fn dump_as_pgm(matrix_subset: &MatrixSubset, sizes: [usize; 2], filename: &str) {
    let mut file = File::create(filename).expect(&format!("Failed to create file {}", filename));
    file.write("P2\n".as_bytes()).expect("Can't write to file");
    file.write(format!("{} {}\n", sizes[1], sizes[0]).as_bytes())
        .expect("Can't write to file");
    file.write("1\n".as_bytes()).expect("Can't write to file");
    for row in matrix_subset {
        let mut is_in = vec![false; sizes[1]];
        for interval in row {
            for i in interval.start..interval.end {
                is_in[i] = true;
            }
        }
        for pixel in is_in {
            if pixel {
                file.write("1\n".as_bytes()).expect("Can't write to file");
            } else {
                file.write("0\n".as_bytes()).expect("Can't write to file");
            }
        }
    }
}

pub fn supersection_candidates(texts: &[PartitionedText; 2]) -> Vec<OriginalCandidate> {
    let seeds = select_seeds(texts);
    let sizes = [texts[0].word_count() + 1, texts[1].word_count() + 1];
    let matrix_subset = union(covered_by_seeds(sizes, &seeds), seed_extensions(sizes, &seeds));

    //dump_as_pgm(&matrix_subset, sizes, "subset.pgm");

    let scoring = AffineScoring::new(&texts);
    candidates_dp(&matrix_subset, &scoring, &scoring)
}

pub fn candidates_dp<AlignmentScoring: AlignmentScoringMethod, SupersectionScoring: SupersectionBoundsScoringMethod>(
    matrix_subset: &MatrixSubset,
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &SupersectionScoring,
) -> Vec<OriginalCandidate> {
    type Position = (usize, usize);
    const INVALID: usize = usize::max_value();

    let mut match_state: SparseMatrix<AlignmentScoring::State> =
        SparseMatrix::new(matrix_subset, &alignment_scoring.starting_state(f64::NEG_INFINITY));
    let mut mismatch_score: SparseMatrix<TScore> = SparseMatrix::new(matrix_subset, &0.0);
    let mut root_position: SparseMatrix<Vec<Position>> = SparseMatrix::new(
        matrix_subset,
        &vec![(INVALID, INVALID); AlignmentScoring::State::SUBSTATES_COUNT],
    );

    let mut best_leaf_for_root: std::collections::HashMap<Position, (TScore, Position, usize)> =
        std::collections::HashMap::new();

    let empty_row = &vec![];
    for old_index in (0..matrix_subset.len()).rev() {
        let next_row = if old_index + 1 < matrix_subset.len() {
            &matrix_subset[old_index + 1]
        } else {
            &empty_row
        };
        let mut current_iterator = MatrixIndex {
            row_index: old_index,
            row_iterator: RowIterator::new(&matrix_subset[old_index]),
        };
        let ops = [DiffOp::Insert, DiffOp::Delete, DiffOp::Match];
        let mut neighbor_iterators = [
            MatrixIndex {
                row_index: old_index,
                row_iterator: RowIterator::new(&matrix_subset[old_index]),
            },
            MatrixIndex {
                row_index: old_index + 1,
                row_iterator: RowIterator::new(&next_row),
            },
            MatrixIndex {
                row_index: old_index + 1,
                row_iterator: RowIterator::new(&next_row),
            },
        ];

        while current_iterator.row_iterator.is_valid() {
            let new_index = current_iterator.row_iterator.index_in_row;
            let change_state_score = bounds_scoring.supersection_bound_penalty(old_index, new_index);

            for (op, iterator) in ops.iter().zip(neighbor_iterators.iter_mut()) {
                let step = op.movement();
                iterator.row_iterator.move_to_position(new_index + step.1);
            }

            for neighbor in neighbor_iterators.iter() {
                if neighbor.row_iterator.is_valid() {
                    mismatch_score[&current_iterator] =
                        TScore::max(mismatch_score[&current_iterator], mismatch_score[&neighbor]);
                }
            }

            match_state[&current_iterator] =
                alignment_scoring.starting_state(mismatch_score[&current_iterator] + change_state_score);

            for (op, neighbor) in ops.iter().zip(neighbor_iterators.iter()) {
                if neighbor.row_iterator.is_valid() {
                    alignment_scoring.consider_step(
                        old_index,
                        new_index,
                        match_state[&neighbor].clone(),
                        &mut match_state[&current_iterator],
                        *op,
                    );
                }
            }
            let match_score = match_state[&current_iterator].best_score();
            mismatch_score[&current_iterator] =
                TScore::max(mismatch_score[&current_iterator], match_score + change_state_score);

            let can_change_state = change_state_score > TScore::NEG_INFINITY;
            let match_scores = match_state[&current_iterator].substate_scores();
            let position = (old_index, new_index);
            for (substate, movement) in match_state[&current_iterator].substate_movements().iter().enumerate() {
                match movement {
                    Some((op, next_substate)) => {
                        let step = op.movement();
                        let index_after_step = &neighbor_iterators[step.0 * 2 + step.1 - 1];
                        let root = root_position[index_after_step][*next_substate];
                        root_position[&current_iterator][substate] = root;
                        if can_change_state {
                            let old_value = best_leaf_for_root.get(&root).unwrap().0;
                            if match_scores[substate] > old_value {
                                best_leaf_for_root.insert(root, (match_scores[substate], position, substate));
                            }
                        }
                    }
                    None => {
                        root_position[&current_iterator][substate] = position;
                        if can_change_state {
                            best_leaf_for_root.insert(position, (match_scores[substate], position, substate));
                        }
                    }
                }
            }

            current_iterator.row_iterator.descend_to_previous_valid();
        }
    }
    let mut result = vec![];
    for (root, (score, position, substate)) in best_leaf_for_root.iter() {
        let root_index = MatrixIndex {
            row_index: root.0,
            row_iterator: RowIterator::new_at_index(&matrix_subset[root.0], root.1),
        };
        if *score - match_state[&root_index].best_score() < SupersectionScoring::SUPERSECTION_THRESHOLD {
            continue;
        }
        let mut alignment = Vec::<DiffOp>::new();
        let mut prefix_scores = vec![0.0];
        let mut word_to_alignment = [vec![], vec![]];
        let (mut old_index, mut new_index) = position;
        let mut substate = *substate;
        let mut alignment_to_word = [vec![old_index], vec![new_index]];
        while old_index < root.0 || new_index < root.1 {
            let current_index = MatrixIndex {
                row_index: old_index,
                row_iterator: RowIterator::new_at_index(&matrix_subset[old_index], new_index),
            };
            let (op, next_substate) = match_state[&current_index].substate_movements()[substate].unwrap();
            let old_score = match_state[&current_index].substate_scores()[substate];
            let step = op.movement();
            old_index += step.0;
            new_index += step.1;
            substate = next_substate;
            let current_index = MatrixIndex {
                row_index: old_index,
                row_iterator: RowIterator::new_at_index(&matrix_subset[old_index], new_index),
            };
            let new_score = match_state[&current_index].substate_scores()[substate];

            if step.0 == 1 {
                word_to_alignment[0].push(alignment.len());
            }
            if step.1 == 1 {
                word_to_alignment[1].push(alignment.len());
            }
            prefix_scores.push(prefix_scores.last().unwrap() + old_score - new_score);
            alignment.push(op);
            alignment_to_word[0].push(old_index);
            alignment_to_word[1].push(new_index);
        }
        result.push(OriginalCandidate {
            starts: [position.0, position.1],
            ends: [root.0, root.1],
            alignment,
            prefix_scores,
            word_to_alignment,
            alignment_to_word,
        });
    }
    result
}
