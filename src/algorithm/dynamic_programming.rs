use super::seed_selection::*;
use super::PartitionedText;

use super::scoring::*;
use super::*;
use std::cmp::max;
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

fn clamp_interval(interval: (usize, usize), bounds: (usize, usize)) -> (usize, usize) {
    (
        interval.0.clamp(bounds.0, bounds.1),
        interval.1.clamp(bounds.0, bounds.1),
    )
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

pub fn filter_seeds(seeds: Vec<Seed>, information_values: &[Vec<TScore>; 2]) -> Vec<Seed> {
    let mut prefix_information_values = vec![0.0];
    let mut prefix_value = 0.0;
    for value in information_values[0].iter() {
        prefix_value += value;
        prefix_information_values.push(prefix_value)
    }
    const SEED_INFORMATION_THRESHOLD: TScore = 20.0;
    let mut best_visible: [Vec<Option<(TScore, usize)>>; 2] = [
        vec![None; information_values[0].len()],
        vec![None; information_values[1].len()],
    ];

    let mut new_seeds = vec![];
    for seed in seeds {
        let information_value = prefix_information_values[seed.end[0]] - prefix_information_values[seed.start[0]];
        if information_value >= SEED_INFORMATION_THRESHOLD {
            for old_index in seed.start[0]..seed.end[0] {
                let new_index = seed.start[1] + old_index - seed.start[0];
                let indices = [old_index, new_index];
                for side in 0..2 {
                    if best_visible[side][indices[side]].is_none()
                        || best_visible[side][indices[side]].unwrap().0 < information_value
                    {
                        best_visible[side][indices[side]] = Some((information_value, new_seeds.len()));
                    }
                }
            }
            new_seeds.push(seed);
        }
    }
    let seeds = new_seeds;

    let mut shadow = [
        vec![false; information_values[0].len()],
        vec![false; information_values[1].len()],
    ];
    for old_index in 0..information_values[0].len() {
        if best_visible[0][old_index].is_none() {
            continue;
        }
        let seed_id = best_visible[0][old_index].unwrap().1;
        let seed = &seeds[seed_id];
        let new_index = old_index - seed.start[0] + seed.start[1];
        if !best_visible[1][new_index].is_none() && best_visible[1][new_index].unwrap().1 == seed_id {
            shadow[0][old_index] = true;
            shadow[1][new_index] = true;
        }
    }

    let mut new_seeds = vec![];
    for (seed_id, seed) in seeds.into_iter().enumerate() {
        for old_index in seed.start[0]..seed.end[0] {
            let new_index = old_index - seed.start[0] + seed.start[1];
            let shadowed_old = shadow[0][old_index] && best_visible[0][old_index].unwrap().1 != seed_id;
            let shadowed_new = shadow[1][new_index] && best_visible[1][new_index].unwrap().1 != seed_id;
            if !shadowed_old && !shadowed_new {
                new_seeds.push(seed);
                break;
            }
        }
    }
    new_seeds
}

pub fn candidate_fragments(texts: &[PartitionedText; 2]) -> Vec<OriginalCandidate> {
    let seeds = select_seeds(texts);

    let scoring = AffineScoring::new(&texts);
    let seeds = filter_seeds(seeds, &scoring.information_values);

    let sizes = [texts[0].word_count() + 1, texts[1].word_count() + 1];
    let matrix_subset = union(covered_by_seeds(sizes, &seeds), seed_extensions(sizes, &seeds));
    //dump_as_pgm(&matrix_subset, sizes, "subset.pgm");

    candidates_dp(&matrix_subset, &scoring, &scoring)
}

pub fn candidates_dp<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    matrix_subset: &MatrixSubset,
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &FragmentScoring,
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
            let change_state_score = bounds_scoring.fragment_bound_penalty(old_index, new_index);

            for (op, iterator) in ops.iter().zip(neighbor_iterators.iter_mut()) {
                let step = op.movement();
                iterator.row_iterator.move_to_position(new_index + step[1]);
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
                        DpDirection::Backward,
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
                        let index_after_step = &neighbor_iterators[step[0] * 2 + step[1] - 1];
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
        if *score - match_state[&root_index].best_score() < FragmentScoring::SUPERSECTION_THRESHOLD {
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
            old_index += step[0];
            new_index += step[1];
            substate = next_substate;
            let current_index = MatrixIndex {
                row_index: old_index,
                row_iterator: RowIterator::new_at_index(&matrix_subset[old_index], new_index),
            };
            let new_score = match_state[&current_index].substate_scores()[substate];

            if step[0] == 1 {
                word_to_alignment[0].push(alignment.len());
            }
            if step[1] == 1 {
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

pub fn extend_seed<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &FragmentScoring,
    seed: Seed,
    bounds_before: [usize; 2],
    bounds_after: [usize; 2],
) -> Option<AlignedFragment> {
    
    let mut dp_backward_start = seed.start;
    for side in 0..2 {
        while !bounds_scoring.is_viable_bound(side, dp_backward_start[side]) {
            dp_backward_start[side] += 1;
        }
    }
    let mut dp_forward_start = seed.end;
    if dp_forward_start[0] < dp_backward_start[0] || dp_forward_start[1] < dp_backward_start[1] {
        // seed didn't contain newline
        dp_forward_start = dp_backward_start;
    }
    else {
        for side in 0..2 {
            while !bounds_scoring.is_viable_bound(side, dp_forward_start[side]) {
                dp_forward_start[side] -= 1;
            }
        }
    }
    
    const BAND_SIZE_LINES: usize = 3;
    const BAND_EOLS_BEFOREQ_BEST: usize = (BAND_SIZE_LINES / 2) + 1;
    const BAND_EOLS_AFTER_BEST: usize = BAND_SIZE_LINES + 1 - BAND_EOLS_BEFOREQ_BEST;
    
    let mut extension_alignments = vec![];
    let mut extension_ends = vec![];
    for (start, bound, direction) in [
        (dp_backward_start, bounds_before, DpDirection::Backward),
        (dp_forward_start, bounds_after, DpDirection::Forward),
    ] {
        let directed_sub = |left : usize, right : usize| {
            match direction {
                DpDirection::Forward => {
                    left - right
                }
                DpDirection::Backward => {
                    right - left
                }
            }
        };
        
        let directed_le = |left : usize, right : usize| {
            match direction {
                DpDirection::Forward => {
                    left <= right
                }
                DpDirection::Backward => {
                    left >= right
                }
            }
        };
        
        
        let mut dp: Vec<Vec<AlignmentScoring::State>> = vec![];
        let mut row_starts = vec![];
        let mut mismatch_score = vec![];
        let mut is_alive = vec![];

        let mut best_start = (0, 0, 0);
        let mut best_score = TScore::NEG_INFINITY;

        let mut best_new_in_next_row = start[1];
        
        let dp_step = match direction {
            DpDirection::Backward => usize::wrapping_sub(0, 1),
            DpDirection::Forward => 1
        };
        
        for row_index in 0 ..= directed_sub(bound[0], start[0]) {
            let mut row_start = best_new_in_next_row;
            let mut seen_eols_beforeq = if bounds_scoring.is_viable_bound(1, row_start) {
                1
            } else {
                0
            };
            while row_start != start[1] && seen_eols_beforeq < BAND_EOLS_BEFOREQ_BEST {
                row_start = row_start.wrapping_sub(dp_step);
                if bounds_scoring.is_viable_bound(1, row_start) {
                    seen_eols_beforeq += 1;
                }
            }
            let mut row_end = best_new_in_next_row;
            let mut seen_eols_after = 0;
            while row_end != bound[1] && seen_eols_after < BAND_EOLS_AFTER_BEST {
                row_end = row_end.wrapping_add(dp_step);
                if bounds_scoring.is_viable_bound(1, row_end) {
                    seen_eols_after += 1;
                }
            }
            let row_size = directed_sub(row_end, row_start) + 1;
            row_starts.push(row_start);
            
            dp.push(vec![
                alignment_scoring.starting_state(TScore::NEG_INFINITY);
                row_size
            ]);
            mismatch_score.push(vec![TScore::NEG_INFINITY; row_size]);
            is_alive.push(vec![
                vec![true; AlignmentScoring::State::SUBSTATES_COUNT];
                row_size
            ]);

            let old_index = start[0].wrapping_add(row_index.wrapping_mul(dp_step));

            let mut best_alive_score_in_next_row = TScore::NEG_INFINITY;
            for col_index in 0..row_size {
                let new_index = row_start.wrapping_add(col_index.wrapping_mul(dp_step));

                let mut valid_steps = vec![];
                for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                    let old_after = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
                    let new_after = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
                    if directed_le(start[0], old_after) {
                        let row_index_after = directed_sub(old_after, start[0]);
                        if directed_le(row_starts[row_index_after], new_after) {
                            let col_index_after = directed_sub(new_after, row_starts[row_index_after]);
                            if col_index_after < dp[row_index_after].len() {
                                valid_steps.push((row_index_after, col_index_after, op));
                            }
                        }
                    }
                }
                
                for step in valid_steps.iter() {
                    mismatch_score[row_index][col_index] =
                        TScore::max(mismatch_score[row_index][col_index], mismatch_score[step.0][step.1]);
                }
                let change_state_score = bounds_scoring.fragment_bound_penalty(old_index, new_index);
                dp[row_index][col_index] =
                    alignment_scoring.starting_state(mismatch_score[row_index][col_index] + change_state_score);
                if old_index == start[0] && new_index == start[1] {
                    dp[row_index][col_index] = alignment_scoring.starting_state(0.0);
                }

                for step in valid_steps {
                    alignment_scoring.consider_step(
                        old_index,
                        new_index,
                        dp[step.0][step.1].clone(),
                        &mut dp[row_index][col_index],
                        step.2,
                        direction.clone(),
                    );
                }

                for (substate, movement) in dp[row_index][col_index].substate_movements().iter().enumerate() {
                    if movement.is_none() {
                        if old_index != start[0] || new_index != start[1] {
                            is_alive[row_index][col_index][substate] = false;
                        }
                    } else {
                        let (op, next_substate) = movement.unwrap();
                        let old_after = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
                        let row_index_after = directed_sub(old_after, start[0]);
                        let new_after = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
                        let col_index_after = directed_sub(new_after, row_starts[row_index_after]);
                        is_alive[row_index][col_index][substate] =
                            is_alive[row_index_after][col_index_after][next_substate];
                    }
                }

                for (substate, &score) in dp[row_index][col_index].substate_scores().iter().enumerate() {
                    if !is_alive[row_index][col_index][substate] {
                        continue;
                    }

                    if score > best_alive_score_in_next_row {
                        best_alive_score_in_next_row = score;
                        best_new_in_next_row = new_index;
                    }

                    let proposed_score = score + change_state_score;
                    if proposed_score > best_score {
                        best_score = proposed_score;
                        best_start = (old_index, new_index, substate);
                    }

                    mismatch_score[row_index][col_index] =
                        TScore::max(mismatch_score[row_index][col_index], proposed_score);
                }
            }
            if best_alive_score_in_next_row == TScore::NEG_INFINITY {
                break;
            }
        }
        let mut alignment = vec![];
        let mut old_index = best_start.0;
        let mut new_index = best_start.1;
        let mut substate = best_start.2;
        while old_index != start[0] || new_index != start[1] {
            let row_index = directed_sub(old_index, start[0]);
            let col_index = directed_sub(new_index, row_starts[row_index]);
            let movement = dp[row_index][col_index].substate_movements()[substate];
            assert!(movement.is_some());
            let (op, next_substate) = movement.unwrap();
            alignment.push(op);
            substate = next_substate;
            old_index = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
            new_index = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
        }
        extension_alignments.push(alignment);
        extension_ends.push([best_start.0, best_start.1]);
    }

    if extension_ends[0][0] >= extension_ends[1][0] || extension_ends[0][1] >= extension_ends[1][1] {
        return None;
    }
    
    let mut alignment = std::mem::take(&mut extension_alignments[0]);
    alignment.append(&mut vec![DiffOp::Match; dp_forward_start[0] - dp_backward_start[0]]);
    extension_alignments[1].reverse();
    alignment.append(&mut extension_alignments[1]);

    Some(AlignedFragment {
        starts: extension_ends[0],
        ends: extension_ends[1],
        alignment,
    })
}
