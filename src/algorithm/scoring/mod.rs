use std::cell::Cell;

use super::*;

pub type TScore = f64;
pub mod affine_scoring;
pub mod line_bounds_scoring;
pub mod simple;

#[derive(Clone, Default)]
pub struct DpSubstate {
    pub score: Cell<TScore>,
    pub previous_step: Cell<Option<(DiffOp, usize)>>,
}

pub trait AlignmentScoringMethod {
    fn substates_count(&self) -> usize;

    fn set_starting_state(
        &self,
        _dp_position: [usize; 2],
        _file_ids: [usize; 2],
        starting_score: TScore,
        state: &mut [DpSubstate],
    ) {
        state.fill(DpSubstate {
            score: Cell::from(starting_score),
            previous_step: Cell::from(None),
        });
    }

    fn consider_step(
        &self,
        word_indices: [usize; 2],
        file_ids: [usize; 2],
        state_before_step: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    );

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool;

    fn score_gaps_between(&self, start_indices: [usize; 2], end_indices: [usize; 2]) -> TScore;

    fn final_substate(&self) -> usize;

    fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [usize; 2],
        end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore>;
    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        start: [usize; 2],
        end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore>;
}

#[derive(Clone, Copy, Debug)]
pub struct InputSliceBounds {
    pub file_ids: [usize; 2],
    pub start: [usize; 2],
    pub size: [usize; 2],
}

impl InputSliceBounds {
    fn global_index(&self, side: usize, local_index: usize) -> usize {
        self.start[side] + local_index
    }

    fn global_indices(&self, local_indices: [usize; 2]) -> [usize; 2] {
        [0, 1].map(|side| self.global_index(side, local_indices[side]))
    }

    pub fn subslice(&self, start_local: [usize; 2], end_local: [usize; 2]) -> InputSliceBounds {
        InputSliceBounds {
            file_ids: self.file_ids,
            start: self.global_indices(start_local),
            size: [0, 1].map(|side| end_local[side] - start_local[side]),
        }
    }
}

pub struct AlignmentSliceScoring<'a> {
    pub slice: InputSliceBounds,
    pub scoring: &'a dyn AlignmentScoringMethod,
}

impl<'a> AlignmentSliceScoring<'a> {
    pub fn substates_count(&self) -> usize {
        self.scoring.substates_count()
    }

    pub fn final_substate(&self) -> usize {
        self.scoring.final_substate()
    }

    pub fn set_starting_state(&self, dp_position: [usize; 2], starting_score: TScore, state: &mut [DpSubstate]) {
        self.scoring.set_starting_state(
            self.slice.global_indices(dp_position),
            self.slice.file_ids,
            starting_score,
            state,
        );
    }

    pub fn consider_step(
        &self,
        dp_position: [usize; 2],
        state_after_move: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    ) {
        self.scoring.consider_step(
            self.slice.global_indices(dp_position),
            self.slice.file_ids,
            state_after_move,
            state,
            step,
        )
    }

    pub fn is_match(&self, part_indices: [usize; 2]) -> bool {
        self.scoring
            .is_match(self.slice.global_indices(part_indices), self.slice.file_ids)
    }
}
