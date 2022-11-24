use crate::algorithm::{DiffOp, PartitionedText};

use super::{internalize_parts, AlignmentScoringMethod, DpDirection, DpSubstate};

pub struct ZeroOneScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
}

impl ZeroOneScoring {
    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]]) -> ZeroOneScoring {
        ZeroOneScoring {
            symbols: internalize_parts(text_parts),
        }
    }
}

impl AlignmentScoringMethod for ZeroOneScoring {
    fn substates_count(&self) -> usize {
        1
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
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
