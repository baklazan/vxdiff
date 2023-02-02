use std::cell::Cell;

use crate::algorithm::{indices::LineIndex, DiffOp};

use super::{line_bounds_scoring::LineBoundsScoring, simple::MatchScoring, AlignmentPrioritizer, DpSubstate, TScore};

pub(in crate::algorithm) struct AffineLineScoring<'a, Matcher: MatchScoring> {
    match_scoring: Matcher,
    bounds_scoring: &'a LineBoundsScoring,
}

impl<'a, Matcher: MatchScoring> AffineLineScoring<'a, Matcher> {
    const SUBSTATES_COUNT: usize = 3;
    const MATCH: usize = 0;
    const DELETE: usize = 1;
    const INSERT: usize = 2;

    const GAP_EDGE_COST: TScore = -2.0;

    // this matrix should be Self::SUBSTATES_COUNT * Self::SUBSTATES_COUNT, but Rust
    const TRANSITION_MATRIX: [[TScore; 3]; 3] = [
        [0.0, Self::GAP_EDGE_COST, Self::GAP_EDGE_COST],
        [Self::GAP_EDGE_COST, 0.0, Self::GAP_EDGE_COST * 2.0],
        [Self::GAP_EDGE_COST, Self::GAP_EDGE_COST * 2.0, 0.0],
    ];

    pub(in crate::algorithm) fn new(
        match_scoring: Matcher,
        bounds_scoring: &LineBoundsScoring,
    ) -> AffineLineScoring<Matcher> {
        AffineLineScoring {
            match_scoring,
            bounds_scoring,
        }
    }

    fn transition_cost(
        &self,
        file_ids: [usize; 2],
        position: [usize; 2],
        from_substate: usize,
        to_substate: usize,
    ) -> TScore {
        let mut cost = Self::TRANSITION_MATRIX[from_substate][to_substate];
        if from_substate != to_substate {
            if from_substate == Self::DELETE || to_substate == Self::DELETE {
                cost += self
                    .bounds_scoring
                    .score_side(0, file_ids[0], LineIndex::new(position[0]));
            }
            if from_substate == Self::INSERT || to_substate == Self::INSERT {
                cost += self
                    .bounds_scoring
                    .score_side(1, file_ids[1], LineIndex::new(position[1]));
            }
        }
        cost
    }
}

impl<'a, Matcher: MatchScoring> AlignmentPrioritizer for AffineLineScoring<'a, Matcher> {
    fn substates_count(&self) -> usize {
        Self::SUBSTATES_COUNT
    }

    fn final_substate(&self) -> usize {
        Self::MATCH
    }

    fn set_starting_state(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        starting_score: TScore,
        state: &mut [DpSubstate],
    ) {
        for substate in 0..Self::SUBSTATES_COUNT {
            state[substate] = DpSubstate {
                score: Cell::from(starting_score + self.transition_cost(file_ids, dp_position, Self::MATCH, substate)),
                previous_step: Cell::from(None),
            };
        }
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_before: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    ) {
        let mut scores_without_transition = [TScore::NEG_INFINITY; 3 /*Self::SUBSTATES_COUNT*/];
        match step {
            DiffOp::Match => {
                let line_indices = dp_position.map(|x| x - 1);
                scores_without_transition[Self::MATCH] =
                    state_before[Self::MATCH].score.get() + self.match_scoring.score(line_indices, file_ids);
            }
            DiffOp::Delete => {
                scores_without_transition[Self::DELETE] = state_before[Self::DELETE].score.get();
            }
            DiffOp::Insert => {
                scores_without_transition[Self::INSERT] = state_before[Self::INSERT].score.get();
            }
        }

        for substate in 0..Self::SUBSTATES_COUNT {
            for from_substate in 0..Self::SUBSTATES_COUNT {
                let proposed_score = scores_without_transition[from_substate]
                    + self.transition_cost(file_ids, dp_position, from_substate, substate);
                if proposed_score > state[substate].score.get() {
                    state[substate].score.set(proposed_score);
                    state[substate].previous_step.set(Some((step, from_substate)));
                }
            }
        }
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.match_scoring.is_match(part_indices, file_ids)
    }
}
