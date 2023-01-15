use crate::algorithm::{DiffOp, PartitionedText};

use super::{
    preprocess::information_values, preprocess::internalize_parts, AlignmentScoringMethod, DpSubstate, TScore,
};

pub struct AffineWordScoring {
    // these matrices should be constant, but Rust doesn't allow floating point operations in const expressions
    transition_matrix: [[TScore; 2]; 2],
    transition_matrix_newline: [[TScore; 2]; 2],

    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    pub information_values: Vec<[Vec<TScore>; 2]>,
    line_splits_at: Vec<[Vec<bool>; 2]>,
}

impl AffineWordScoring {
    const MATCH: usize = 0;
    const GAP: usize = 1;

    fn compute_transition_matrix(p_start_gap: f64, p_end_gap: f64) -> [[TScore; 2]; 2] {
        [
            [(1.0 - p_start_gap).log2(), p_start_gap.log2()],
            [p_end_gap.log2(), (1.0 - p_end_gap).log2()],
        ]
    }

    const P_END_GAP: f64 = 0.3;
    const P_START_GAP: f64 = 0.05;
    const NEWLINE_STATE_CHANGE_COEF: f64 = 1.1;

    pub(in crate::algorithm) fn new(text_words: &[[PartitionedText; 2]]) -> AffineWordScoring {
        let information_values = information_values(text_words);

        let mut line_splits_at = vec![];
        for file_text_words in text_words {
            let mut file_line_splits_at = [vec![true], vec![true]];
            for side in 0..2 {
                for i in 0..file_text_words[side].part_count() {
                    let word = file_text_words[side].get_part(i);
                    if word == "\n" || i + 1 >= file_text_words[side].part_count() {
                        file_line_splits_at[side].push(true);
                    } else {
                        file_line_splits_at[side].push(false);
                    }
                }
            }
            line_splits_at.push(file_line_splits_at);
        }

        let transition_matrix = Self::compute_transition_matrix(Self::P_START_GAP, Self::P_END_GAP);
        let transition_matrix_newline = Self::compute_transition_matrix(
            Self::P_START_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
            Self::P_END_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
        );

        AffineWordScoring {
            transition_matrix,
            transition_matrix_newline,
            symbols: internalize_parts(text_words),
            information_values,
            line_splits_at,
        }
    }

    fn transition_cost(
        &self,
        file_ids: [usize; 2],
        position: [usize; 2],
        from_state: usize,
        to_state: usize,
    ) -> TScore {
        let transition_matrix =
            if self.line_splits_at[file_ids[0]][0][position[0]] && self.line_splits_at[file_ids[1]][1][position[1]] {
                &self.transition_matrix_newline
            } else {
                &self.transition_matrix
            };
        transition_matrix[from_state][to_state]
    }

    // alignment of length x will produce output of length 2*x - 1 (x steps + (x-1) transitions)
    fn score_alignment_steps(
        &self,
        alignment: &[DiffOp],
        start: [usize; 2],
        end: [usize; 2],
        file_ids: [usize; 2],
    ) -> Option<Vec<TScore>> {
        let mut position = start;
        let mut result = vec![];
        for (i, &op) in alignment.iter().enumerate() {
            if i > 0 {
                let op_to_state = |op: DiffOp| match op {
                    DiffOp::Match => Self::MATCH,
                    DiffOp::Insert => Self::GAP,
                    DiffOp::Delete => Self::GAP,
                };
                let old_state = op_to_state(alignment[i - 1]);
                let new_state = op_to_state(op);
                result.push(self.transition_cost(file_ids, position, old_state, new_state));
            }
            if op == DiffOp::Match {
                if !self.is_match(position, file_ids) {
                    return None;
                }
                result.push(self.information_values[file_ids[0]][0][position[0]]);
            } else {
                result.push(0.0);
            }
            for side in 0..2 {
                position[side] += op.movement()[side];
            }
        }
        for side in 0..2 {
            if position[side] != end[side] {
                return None;
            }
        }
        Some(result)
    }

    pub fn alignment_score(&self, alignment: &[DiffOp], file_ids: [usize; 2]) -> Option<TScore> {
        let step_scores = self.score_alignment_steps(
            alignment,
            [0, 0],
            [0, 1].map(|side| self.symbols[file_ids[side]][side].len()),
            file_ids,
        )?;
        let mut result = 0.0;
        for step_score in step_scores {
            result += step_score;
        }
        Some(result)
    }
}

impl AlignmentScoringMethod for AffineWordScoring {
    fn substates_count(&self) -> usize {
        2
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_after_move: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    ) {
        let word_indices = [dp_position[0].wrapping_sub(1), dp_position[1].wrapping_sub(1)];

        let improve = |substate: usize, proposed: TScore, proposed_movement: &Option<(DiffOp, usize)>| {
            if state[substate].score.get() < proposed {
                state[substate].score.set(proposed);
                state[substate].previous_step.set(*proposed_movement);
            }
        };

        if step == DiffOp::Match {
            let syms = [0, 1].map(|side| self.symbols[file_ids[side]][side][word_indices[side]]);

            if syms[0] != syms[1] {
                return;
            }
            let score_without_transition =
                state_after_move[Self::MATCH].score.get() + self.information_values[file_ids[0]][0][word_indices[0]];
            let movement = Some((DiffOp::Match, Self::MATCH));
            for to_state in [Self::MATCH, Self::GAP] {
                improve(
                    to_state,
                    score_without_transition + self.transition_cost(file_ids, dp_position, Self::MATCH, to_state),
                    &movement,
                );
            }
        } else {
            let score_without_transition = state_after_move[Self::GAP].score.get();
            let movement = Some((step, Self::GAP));
            for to_state in [Self::MATCH, Self::GAP] {
                improve(
                    to_state,
                    score_without_transition + self.transition_cost(file_ids, dp_position, Self::GAP, to_state),
                    &movement,
                );
            }
        }
    }

    fn is_match(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][word_indices[0]] == self.symbols[file_ids[1]][1][word_indices[1]]
    }

    fn append_gaps(
        &self,
        _file_ids: [usize; 2],
        start_indices: [usize; 2],
        end_indices: [usize; 2],
        starting_substate: usize,
    ) -> TScore {
        let gap_length = (end_indices[0] - start_indices[0]) + (end_indices[1] - start_indices[1]);
        if gap_length == 0 {
            return 0.0;
        }
        self.transition_matrix[starting_substate][Self::GAP]
            + self.transition_matrix[Self::GAP][Self::GAP] * (gap_length - 1) as f64
    }

    fn substate_score(
        &self,
        state: &[DpSubstate],
        substate: usize,
        file_ids: [usize; 2],
        position: [usize; 2],
    ) -> TScore {
        if state[substate].previous_step.get().is_none() {
            state[substate].score.get()
        } else {
            state[substate].score.get()
                - self.transition_cost(
                    file_ids,
                    position,
                    state[substate].previous_step.get().unwrap().1,
                    substate,
                )
        }
    }

    fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [usize; 2],
        end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        let step_scores = self.score_alignment_steps(alignment, start, end, file_ids).unwrap();
        let mut score = 0.0;
        let mut result = vec![score];
        for i in 0..alignment.len() {
            if i > 0 {
                score += step_scores[i * 2 - 1];
            }
            score += step_scores[i * 2];
            result.push(score);
        }
        result
    }

    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        start: [usize; 2],
        end: [usize; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        let step_scores = self.score_alignment_steps(alignment, start, end, file_ids).unwrap();
        let mut score = 0.0;
        let mut result = vec![score];
        for i in (0..alignment.len()).rev() {
            if i + 1 < alignment.len() {
                score += step_scores[i * 2 + 1];
            }
            score += step_scores[i * 2];
            result.push(score);
        }
        result.reverse();
        result
    }
}
