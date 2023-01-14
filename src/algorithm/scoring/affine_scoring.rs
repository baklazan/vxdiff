use crate::algorithm::{DiffOp, PartitionedText};

use super::{
    preprocess::information_values, preprocess::internalize_parts, AlignmentScoringMethod, DpSubstate,
    FragmentBoundsScoringMethod, TScore,
};

pub struct AffineWordScoring {
    // these matrices should be constant, but Rust doesn't allow floating point operations in const expressions
    transition_matrix: [[TScore; 2]; 2],
    transition_matrix_newline: [[TScore; 2]; 2],

    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    pub information_values: Vec<[Vec<TScore>; 2]>,
    line_splits_at: Vec<[Vec<bool>; 2]>,
    nearest_line_split_forward: Vec<[Vec<usize>; 2]>,
    bound_score: Vec<[Vec<TScore>; 2]>,
}

impl AffineWordScoring {
    const MATCH: usize = 0;
    const GAP: usize = 1;

    fn compute_transition_matrix(
        p_start_gap: f64,
        p_end_gap: f64,
    ) -> [[TScore; 2]; 2] {
        [
            [(1.0 - p_start_gap).log2(), p_start_gap.log2()],
            [p_end_gap.log2(), (1.0 - p_end_gap).log2()],
        ]
    }

    const P_END_GAP: f64 = 0.3;
    const P_START_GAP: f64 = 0.05;
    const NEWLINE_STATE_CHANGE_COEF: f64 = 1.1;

    const BASE_BOUND_SCORE: TScore = -1.0;
    const LINE_CONTENT_COEF: TScore = -0.8;

    pub(in crate::algorithm) fn new(text_words: &[[PartitionedText; 2]]) -> AffineWordScoring {
        let information_values = information_values(text_words);

        let mut line_splits_at = vec![];
        let mut bound_score = vec![];
        let mut nearest_line_split_forward = vec![];
        let mut nearest_line_split_backward = vec![];
        for (file_id, file_text_words) in text_words.iter().enumerate() {
            let mut file_line_splits_at = [vec![true], vec![true]];
            let mut file_bound_score = [
                vec![TScore::NEG_INFINITY; file_text_words[0].part_count() + 1],
                vec![TScore::NEG_INFINITY; file_text_words[1].part_count() + 1],
            ];
            let mut file_nearest_split_forward = [vec![], vec![]];
            let mut file_nearest_split_backward = [vec![], vec![]];
            for side in 0..2 {
                let mut this_line_value = 0.0;
                let mut last_line_value = 0.0;
                let mut last_line_end = 0;
                for i in 0..file_text_words[side].part_count() {
                    let word = file_text_words[side].get_part(i);

                    this_line_value += information_values[file_id][side][i];
                    if word == "\n" || i + 1 >= file_text_words[side].part_count() {
                        file_bound_score[side][last_line_end] = Self::BASE_BOUND_SCORE
                            + TScore::min(this_line_value, last_line_value) * Self::LINE_CONTENT_COEF;
                        last_line_value = this_line_value;
                        this_line_value = 0.0;
                        last_line_end = i + 1;
                        file_line_splits_at[side].push(true);
                    } else {
                        file_line_splits_at[side].push(false);
                    }
                }
                file_bound_score[side][last_line_end] =
                    Self::BASE_BOUND_SCORE + last_line_value * Self::LINE_CONTENT_COEF;

                let mut last_line_split = 0;
                for (i, &is_split) in file_line_splits_at[side].iter().enumerate() {
                    if is_split {
                        last_line_split = i;
                    }
                    file_nearest_split_backward[side].push(last_line_split);
                }
                let mut next_line_split = file_line_splits_at[side].len() - 1;
                for (i, &is_split) in file_line_splits_at[side].iter().enumerate().rev() {
                    if is_split {
                        next_line_split = i;
                    }
                    file_nearest_split_forward[side].push(next_line_split);
                }
                file_nearest_split_forward[side].reverse();
            }
            line_splits_at.push(file_line_splits_at);
            bound_score.push(file_bound_score);
            nearest_line_split_backward.push(file_nearest_split_backward);
            nearest_line_split_forward.push(file_nearest_split_forward);
        }

        let transition_matrix = Self::compute_transition_matrix(
            Self::P_START_GAP,
            Self::P_END_GAP,
        );
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
            nearest_line_split_forward,
            bound_score,
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

    pub fn alignment_score(&self, alignment: &[DiffOp], file_ids: [usize; 2]) -> Option<TScore> {
        let mut current_gap_score: TScore = 0.0;
        let mut in_gap = false;

        let mut word_indices = [0, 0];
        let mut result: TScore = 0.0;
        for (i, &op) in alignment.iter().enumerate() {
            if op == DiffOp::Match {
                if !self.is_match(word_indices, file_ids) {
                    return None;
                }
                if in_gap {
                    current_gap_score +=
                        self.transition_cost(file_ids, word_indices, Self::GAP, Self::MATCH);
                    result += current_gap_score;
                    in_gap = false;
                } else if i > 0 {
                    result +=
                        self.transition_cost(file_ids, word_indices, Self::MATCH, Self::MATCH);
                }
                result += self.information_values[file_ids[0]][0][word_indices[0]];
            } else {
                if !in_gap {
                    in_gap = true;
                    current_gap_score = 0.0;
                    if i > 0 {
                        current_gap_score +=
                            self.transition_cost(file_ids, word_indices, Self::MATCH, Self::GAP);
                    }
                } else {
                    current_gap_score +=
                        self.transition_cost(file_ids, word_indices, Self::GAP, Self::GAP);
                }
            }
            for side in 0..2 {
                word_indices[side] += op.movement()[side];
            }
        }
        if in_gap {
            result += current_gap_score;
        }

        for side in 0..2 {
            if word_indices[side] != self.symbols[file_ids[side]][side].len() {
                return None;
            }
        }
        Some(result)
    }
}

impl AlignmentScoringMethod for AffineWordScoring {
    fn substates_count(&self) -> usize {
        3
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
                    score_without_transition
                        + self.transition_cost(file_ids, dp_position, Self::MATCH, to_state),
                    &movement,
                );
            }
        } else {
            let score_without_transition = state_after_move[Self::GAP].score.get();
            let movement = Some((step, Self::GAP));
            for to_state in [Self::MATCH, Self::GAP] {
                improve(
                    to_state,
                    score_without_transition
                        + self.transition_cost(file_ids, dp_position, Self::GAP, to_state),
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
}

impl FragmentBoundsScoringMethod for AffineWordScoring {
    const SUPERSECTION_THRESHOLD: TScore = 5.0;
    const MOVED_SUPERSECTION_THRESHOLD: TScore = 10.0;

    fn fragment_bound_penalty(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        self.bound_score[file_ids[0]][0][word_indices[0]] + self.bound_score[file_ids[1]][1][word_indices[1]]
    }

    fn is_viable_bound(&self, side: usize, index: usize, file_id: usize) -> bool {
        index < self.bound_score[file_id][side].len() && self.bound_score[file_id][side][index] != TScore::NEG_INFINITY
    }

    fn nearest_bound(&self, side: usize, index: usize, file_id: usize) -> usize {
        self.nearest_line_split_forward[file_id][side][index]
    }
}
