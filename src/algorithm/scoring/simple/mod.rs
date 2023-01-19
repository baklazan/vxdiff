use crate::algorithm::DiffOp;

use super::{AlignmentScoringMethod, DpSubstate, TScore};

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

impl<Matcher: MatchScoring> AlignmentScoringMethod for SimpleScoring<Matcher> {
    fn substates_count(&self) -> usize {
        1
    }

    fn final_substate(&self) -> usize {
        0
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.match_scoring.is_match(part_indices, file_ids)
    }

    fn score_gaps_between(&self, _start_indices: [usize; 2], _end_indices: [usize; 2]) -> super::TScore {
        0.0
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

    fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [usize; 2],
        _end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        let mut position = start;
        let mut score = 0.0;
        let mut result = vec![score];
        for &op in alignment {
            if op == DiffOp::Match {
                score += self.match_scoring.score(position, file_ids);
            }
            result.push(score);
            for side in 0..2 {
                position[side] += op.movement()[side];
            }
        }
        result
    }

    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        _start: [usize; 2],
        end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        let mut position = end;
        let mut score = 0.0;
        let mut result = vec![score];
        for &op in alignment.iter().rev() {
            for side in 0..2 {
                position[side] -= op.movement()[side];
            }
            if op == DiffOp::Match {
                score += self.match_scoring.score(position, file_ids);
            }
            result.push(score);
        }
        result.reverse();
        result
    }
}
