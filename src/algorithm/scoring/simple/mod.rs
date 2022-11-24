use crate::algorithm::DiffOp;

use super::{TScore, AlignmentScoringMethod, DpSubstate, DpDirection};

pub mod zero_one_scoring;

pub trait MatchScoring {
    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore;
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool;
}

pub struct SimpleScoring<Matcher: MatchScoring> {
    pub match_scoring: Matcher
}

impl <Matcher: MatchScoring> AlignmentScoringMethod for SimpleScoring<Matcher> {
    fn substates_count(&self) -> usize {
        1
    }
    
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.match_scoring.is_match(part_indices, file_ids)
    }
    
    fn append_gaps(
        &self,
        _file_ids: [usize; 2],
        _start_indices: [usize; 2],
        _end_indices: [usize; 2],
        _starting_substate: usize,
    ) -> super::TScore {
        0.0
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_before_step: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
        direction: DpDirection,
    ) {
        let step_score = match step {
            DiffOp::Insert => 0.0,
            DiffOp::Delete => 0.0,
            DiffOp::Match => {
                let index_correction = match direction {
                    DpDirection::Forward => 1,
                    DpDirection::Backward => 0,
                };
                let part_indices = dp_position.map(|i| i - index_correction);
                if self.is_match(part_indices, file_ids) {
                    1.0
                } else {
                    0.0
                }
            }
        };
        let proposed_score = state_before_step[0].score.get() + step_score;
        if proposed_score > state[0].score.get() {
            state[0].score.set(proposed_score);
            state[0].previous_step.set(Some((step, 0)));
        }
    }
}