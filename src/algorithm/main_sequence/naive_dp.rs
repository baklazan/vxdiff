use crate::algorithm::{
    dp_substate_vec::DpStateVec,
    indices::WordIndex,
    scoring::{AlignmentScorer, InputSliceBounds, SliceAlignmentPrioritizer, TScore},
    DiffOp,
};

use super::Aligner;

fn compute_dp_matrix(alignment_scoring: &SliceAlignmentPrioritizer, row_range: usize) -> Vec<DpStateVec> {
    let sizes = &alignment_scoring.slice.size;
    let mut result = vec![DpStateVec::new(sizes[1] + 1, alignment_scoring.substates_count()); row_range];

    for old_index in 0..=sizes[0] {
        for new_index in 0..=sizes[1] {
            let default_score = if old_index == 0 && new_index == 0 {
                0.0
            } else {
                TScore::NEG_INFINITY
            };
            alignment_scoring.set_starting_state(
                [old_index, new_index],
                default_score,
                &mut result[old_index % row_range][new_index],
            );
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

fn naive_dp(alignment_scoring: &SliceAlignmentPrioritizer) -> Vec<DiffOp> {
    let sizes = alignment_scoring.slice.size;
    let matrix = compute_dp_matrix(alignment_scoring, sizes[0] + 1);

    let mut result = vec![];
    let mut substate = alignment_scoring.final_substate();
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

pub fn compute_score(alignment_scoring: &SliceAlignmentPrioritizer) -> TScore {
    let matrix = compute_dp_matrix(alignment_scoring, 2);
    let sizes = alignment_scoring.slice.size;
    matrix[sizes[0] % 2][sizes[1]][alignment_scoring.final_substate()]
        .score
        .get()
}

pub(in crate::algorithm) struct NaiveAligner<'a> {
    scoring: &'a dyn AlignmentScorer,
}

impl<'a> NaiveAligner<'a> {
    pub fn new(scoring: &'a dyn AlignmentScorer) -> Self {
        NaiveAligner { scoring }
    }
}

impl<'a> Aligner for NaiveAligner<'a> {
    fn align(&self, file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> Vec<DiffOp> {
        let slice = InputSliceBounds {
            file_ids,
            start: [usize::from(start[0]), usize::from(start[1])],
            size: [usize::from(end[0] - start[0]), usize::from(end[1] - start[1])],
        };
        let slice_scoring = SliceAlignmentPrioritizer {
            scoring: self.scoring.as_prioritizer(),
            slice,
        };
        naive_dp(&slice_scoring)
    }

    fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        self.scoring
            .prefix_scores(file_ids, start.map(WordIndex::raw), end.map(WordIndex::raw), alignment)
    }

    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        self.scoring
            .suffix_scores(file_ids, start.map(WordIndex::raw), end.map(WordIndex::raw), alignment)
    }

    fn score_gaps_between(&self, file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> TScore {
        self.scoring
            .score_gaps_between(file_ids, start.map(WordIndex::raw), end.map(WordIndex::raw))
    }
}
