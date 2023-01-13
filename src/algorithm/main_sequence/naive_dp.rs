use crate::algorithm::{
    dp_substate_vec::DpStateVec,
    indices::WordIndex,
    scoring::{AlignmentScoringMethod, AlignmentSliceScoring, DpDirection::Forward, InputSliceBounds, TScore},
    DiffOp,
};

use super::Aligner;

fn compute_dp_matrix(alignment_scoring: &AlignmentSliceScoring, row_range: usize) -> Vec<DpStateVec> {
    let sizes = &alignment_scoring.slice.size;
    let mut result = vec![DpStateVec::new(sizes[1] + 1, alignment_scoring.substates_count()); row_range];

    for old_index in 0..=sizes[0] {
        for new_index in 0..=sizes[1] {
            let default_score = if old_index == 0 && new_index == 0 {
                0.0
            } else {
                TScore::NEG_INFINITY
            };
            alignment_scoring.set_starting_state(default_score, &mut result[old_index % row_range][new_index]);
            for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                if old_index >= op.movement()[0] && new_index >= op.movement()[1] {
                    alignment_scoring.consider_step(
                        [old_index, new_index],
                        &result[(old_index - op.movement()[0]) % row_range][new_index - op.movement()[1]],
                        &result[old_index % row_range][new_index],
                        op,
                    )
                }
            }
        }
    }

    result
}

fn naive_dp(alignment_scoring: &AlignmentSliceScoring) -> Vec<DiffOp> {
    let sizes = alignment_scoring.slice.size;
    let matrix = compute_dp_matrix(alignment_scoring, sizes[0] + 1);

    let mut best_substate = 0;
    let final_scores: Vec<TScore> = (0..alignment_scoring.substates_count())
        .map(|substate| alignment_scoring.substate_score(&matrix[sizes[0]][sizes[1]], substate, [sizes[0], sizes[1]]))
        .collect();
    for substate in 1..alignment_scoring.substates_count() {
        if final_scores[substate] > final_scores[best_substate] {
            best_substate = substate;
        }
    }

    let mut result = vec![];
    let mut substate = best_substate;
    let mut indices = sizes;
    while indices[0] > 0 || indices[1] > 0 {
        let (op, next_substate) = matrix[indices[0]][indices[1]][substate].previous_step.get().unwrap();
        substate = next_substate;
        result.push(op);
        for side in 0..2 {
            indices[side] -= op.movement()[side];
        }
    }
    result.reverse();
    result
}

pub fn compute_score(alignment_scoring: &AlignmentSliceScoring) -> TScore {
    let matrix = compute_dp_matrix(alignment_scoring, 2);
    let sizes = alignment_scoring.slice.size;
    let scores: Vec<TScore> = (0..alignment_scoring.substates_count())
        .map(|substate| {
            alignment_scoring.substate_score(&matrix[sizes[0] % 2][sizes[1]], substate, [sizes[0], sizes[1]])
        })
        .collect();

    let mut result = scores[0];
    for substate in 1..alignment_scoring.substates_count() {
        if scores[substate] > result {
            result = scores[substate];
        }
    }
    result
}

pub(in crate::algorithm) struct NaiveAligner<'a> {
    scoring: &'a dyn AlignmentScoringMethod,
}

impl<'a> NaiveAligner<'a> {
    pub fn new(scoring: &'a dyn AlignmentScoringMethod) -> Self {
        NaiveAligner { scoring }
    }
}

impl<'a> Aligner for NaiveAligner<'a> {
    fn align(&self, file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> Vec<DiffOp> {
        let slice = InputSliceBounds {
            file_ids,
            start: [usize::from(start[0]), usize::from(start[1])],
            size: [usize::from(end[0] - start[0]), usize::from(end[1] - start[1])],
            direction: Forward,
        };
        let slice_scoring = AlignmentSliceScoring {
            scoring: self.scoring,
            slice,
        };
        naive_dp(&slice_scoring)
    }
}
