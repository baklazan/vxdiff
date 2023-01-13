use std::cell::Cell;

use super::*;

pub type TScore = f64;
pub mod affine_scoring;
pub mod simple;

#[derive(Clone, Default)]
pub struct DpSubstate {
    pub score: Cell<TScore>,
    pub previous_step: Cell<Option<(DiffOp, usize)>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DpDirection {
    Backward,
    Forward,
}

pub trait AlignmentScoringMethod {
    fn substates_count(&self) -> usize;

    fn set_starting_state(&self, starting_score: TScore, state: &mut [DpSubstate]) {
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
        direction: DpDirection,
    );

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool;

    fn append_gaps(
        &self,
        file_ids: [usize; 2],
        start_indices: [usize; 2],
        end_indices: [usize; 2],
        starting_substate: usize,
    ) -> TScore;

    fn substate_score(
        &self,
        state: &[DpSubstate],
        substate: usize,
        _file_ids: [usize; 2],
        _position: [usize; 2],
        _direction: DpDirection,
    ) -> TScore {
        state[substate].score.get()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InputSliceBounds {
    pub file_ids: [usize; 2],
    pub start: [usize; 2],
    pub direction: DpDirection,
    pub size: [usize; 2],
}

impl InputSliceBounds {
    fn global_index(&self, side: usize, local_index: usize) -> usize {
        match self.direction {
            DpDirection::Forward => self.start[side] + local_index,
            DpDirection::Backward => self.start[side] - local_index,
        }
    }

    fn global_indices(&self, local_indices: [usize; 2]) -> [usize; 2] {
        [0, 1].map(|side| self.global_index(side, local_indices[side]))
    }

    fn local_index(&self, side: usize, global_index: usize) -> usize {
        match self.direction {
            DpDirection::Forward => global_index - self.start[side],
            DpDirection::Backward => self.start[side] - global_index,
        }
    }

    fn local_indices(&self, global_indices: [usize; 2]) -> [usize; 2] {
        [0, 1].map(|side| self.local_index(side, global_indices[side]))
    }

    pub fn subslice(&self, start_local: [usize; 2], end_local: [usize; 2]) -> InputSliceBounds {
        InputSliceBounds {
            file_ids: self.file_ids,
            start: self.global_indices(start_local),
            size: [0, 1].map(|side| end_local[side] - start_local[side]),
            direction: self.direction,
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

    pub fn set_starting_state(&self, starting_score: TScore, state: &mut [DpSubstate]) {
        self.scoring.set_starting_state(starting_score, state);
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
            self.slice.direction,
        )
    }

    pub fn is_match(&self, part_indices: [usize; 2]) -> bool {
        self.scoring
            .is_match(self.slice.global_indices(part_indices), self.slice.file_ids)
    }

    pub fn append_gaps(&self, mut start_indices: [usize; 2], mut end_indices: [usize; 2], substate: usize) -> TScore {
        if self.slice.direction == DpDirection::Backward {
            (start_indices, end_indices) = (end_indices, start_indices);
        }
        self.scoring.append_gaps(
            self.slice.file_ids,
            self.slice.global_indices(start_indices),
            self.slice.global_indices(end_indices),
            substate,
        )
    }

    pub fn substate_score(&self, state: &[DpSubstate], substate: usize, position: [usize; 2]) -> TScore {
        self.scoring.substate_score(
            state,
            substate,
            self.slice.file_ids,
            self.slice.global_indices(position),
            self.slice.direction,
        )
    }
}

pub trait FragmentBoundsScoringMethod {
    const SUPERSECTION_THRESHOLD: TScore;
    const MOVED_SUPERSECTION_THRESHOLD: TScore;

    fn fragment_bound_penalty(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore;
    fn is_viable_bound(&self, side: usize, index: usize, file_id: usize) -> bool;
    fn nearest_bound(&self, side: usize, index: usize, file_id: usize, direction: DpDirection) -> usize;
}

pub struct BoundsSliceScoring<'a, Scoring: FragmentBoundsScoringMethod> {
    pub slice: InputSliceBounds,
    pub scoring: &'a Scoring,
}

impl<'a, Scoring: FragmentBoundsScoringMethod> BoundsSliceScoring<'a, Scoring> {
    pub fn fragment_bound_penalty(&self, word_indices: [usize; 2]) -> TScore {
        self.scoring
            .fragment_bound_penalty(self.slice.global_indices(word_indices), self.slice.file_ids)
    }

    pub fn nearest_bound_point(&self, word_indices: [usize; 2]) -> [usize; 2] {
        let global_point_indices = [0, 1].map(|side| {
            self.scoring.nearest_bound(
                side,
                self.slice.global_index(side, word_indices[side]),
                self.slice.file_ids[side],
                self.slice.direction,
            )
        });
        self.slice.local_indices(global_point_indices)
    }
}
