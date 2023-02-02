use crate::algorithm::DiffOp;

use super::{AlignmentPrioritizer, DpSubstate, TScore};

pub mod k_gram_sampling;
pub mod whitespace_ignoring;
pub mod zero_one;
pub mod zero_or_information;

pub trait MatchScoring {
    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore;
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool;
}

pub struct SimpleScoring<Matcher: MatchScoring> {
    pub match_scoring: Matcher,
}

impl<Matcher: MatchScoring> AlignmentPrioritizer for SimpleScoring<Matcher> {
    fn substates_count(&self) -> usize {
        1
    }

    fn final_substate(&self) -> usize {
        0
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.match_scoring.is_match(part_indices, file_ids)
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_before_step: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    ) {
        let step_score = match step {
            DiffOp::Insert => 0.0,
            DiffOp::Delete => 0.0,
            DiffOp::Match => {
                let part_indices = dp_position.map(|i| i - 1);
                self.match_scoring.score(part_indices, file_ids)
            }
        };
        let proposed_score = state_before_step[0].score.get() + step_score;
        if proposed_score > state[0].score.get() {
            state[0].score.set(proposed_score);
            state[0].previous_step.set(Some((step, 0)));
        }
    }
}
