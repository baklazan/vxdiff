use std::cell::Cell;

use crate::algorithm::{
    indices::{range_iter, IndexConverter, LineIndex, WordIndex},
    preprocess::{information_values, internalize_parts},
    DiffOp, PartitionedText,
};

use super::{line_bounds_scoring::LineBoundsScoring, AlignmentPrioritizer, AlignmentScorer, DpSubstate, TScore};

pub struct MultilineGapsScoring<'a> {
    index_converters: &'a [[IndexConverter; 2]],
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    information_values: Vec<[Vec<TScore>; 2]>,
    line_bounds_scoring: &'a LineBoundsScoring,
}

impl<'a> MultilineGapsScoring<'a> {
    const SUBSTATES_COUNT: usize = 3;
    const MATCH: usize = 0;
    const BIG_DELETE: usize = 1;
    const BIG_INSERT: usize = 2;

    const SMALL_GAP_COST: TScore = -1.0;
    const BIG_GAP_EDGE_COST: TScore = -0.0;
    const EOL_TRANSITION_MATRIX: [[TScore; 3]; 3] = [
        [0.0, Self::BIG_GAP_EDGE_COST, Self::BIG_GAP_EDGE_COST],
        [Self::BIG_GAP_EDGE_COST, 0.0, Self::BIG_GAP_EDGE_COST * 2.0],
        [Self::BIG_GAP_EDGE_COST, Self::BIG_GAP_EDGE_COST * 2.0, 0.0],
    ];

    pub(in crate::algorithm) fn new(
        text_words: &[[PartitionedText; 2]],
        index_converters: &'a [[IndexConverter; 2]],
        line_bounds_scoring: &'a LineBoundsScoring,
    ) -> Self {
        let symbols = internalize_parts(text_words);
        let information_values = information_values(text_words);
        MultilineGapsScoring {
            index_converters,
            symbols,
            information_values,
            line_bounds_scoring,
        }
    }

    fn get_line_indices(&self, file_ids: [usize; 2], word_indices: [WordIndex; 2]) -> Option<[LineIndex; 2]> {
        let mut result: [Option<LineIndex>; 2] = [None, None];
        for side in 0..2 {
            let line = self.index_converters[file_ids[side]][side].word_to_line_before(word_indices[side]);
            if self.index_converters[file_ids[side]][side].line_to_word(line) == word_indices[side] {
                result[side] = Some(line)
            }
        }
        if result[0].is_some() && result[1].is_some() {
            return Some(result.map(Option::unwrap));
        }
        None
    }
}

impl<'a> AlignmentPrioritizer for MultilineGapsScoring<'a> {
    fn substates_count(&self) -> usize {
        Self::SUBSTATES_COUNT
    }

    fn final_substate(&self) -> usize {
        Self::MATCH
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_before_step: &[DpSubstate],
        state: &[DpSubstate],
        step: DiffOp,
    ) {
        let improve = |substate: usize, proposed: TScore, proposed_movement: Option<(DiffOp, usize)>| {
            if state[substate].score.get() < proposed {
                state[substate].score.set(proposed);
                state[substate].previous_step.set(proposed_movement);
            }
        };

        let mut scores_without_transition = [TScore::NEG_INFINITY; Self::SUBSTATES_COUNT];

        if step == DiffOp::Delete {
            scores_without_transition[Self::BIG_DELETE] = state_before_step[Self::BIG_DELETE].score.get();
        }
        if step == DiffOp::Insert {
            scores_without_transition[Self::BIG_INSERT] = state_before_step[Self::BIG_INSERT].score.get();
        }
        let match_state_score = if step == DiffOp::Match {
            let word_indices = dp_position.map(|x| x - 1);
            let symbols = [0, 1].map(|side| self.symbols[file_ids[side]][side][word_indices[side]]);
            if symbols[0] != symbols[1] {
                return;
            }
            self.information_values[file_ids[0]][0][word_indices[0]]
        } else {
            Self::SMALL_GAP_COST
        };
        scores_without_transition[Self::MATCH] = state_before_step[Self::MATCH].score.get() + match_state_score;

        if let Some(lines_indices) = self.get_line_indices(file_ids, dp_position.map(WordIndex::new)) {
            for from_substate in 0..Self::SUBSTATES_COUNT {
                for to_substate in 0..Self::SUBSTATES_COUNT {
                    let mut proposed_score = scores_without_transition[from_substate]
                        + Self::EOL_TRANSITION_MATRIX[from_substate][to_substate];
                    if from_substate != to_substate {
                        if from_substate == Self::BIG_DELETE || to_substate == Self::BIG_DELETE {
                            proposed_score += self.line_bounds_scoring.score_side(0, file_ids[0], lines_indices[0]);
                        }
                        if from_substate == Self::BIG_INSERT || to_substate == Self::BIG_INSERT {
                            proposed_score += self.line_bounds_scoring.score_side(1, file_ids[1], lines_indices[1]);
                        }
                    }
                    improve(to_substate, proposed_score, Some((step, from_substate)));
                }
            }
        } else {
            for substate in 0..Self::SUBSTATES_COUNT {
                improve(substate, scores_without_transition[substate], Some((step, substate)));
            }
        }
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
    }

    fn set_starting_state(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        starting_score: TScore,
        state: &mut [DpSubstate],
    ) {
        state[Self::MATCH] = DpSubstate {
            score: Cell::from(starting_score + Self::EOL_TRANSITION_MATRIX[Self::MATCH][Self::MATCH]),
            previous_step: Cell::from(None),
        };

        if let Some(line_indices) = self.get_line_indices(file_ids, dp_position.map(WordIndex::new)) {
            state[Self::BIG_DELETE] = DpSubstate {
                score: Cell::from(
                    starting_score
                        + Self::EOL_TRANSITION_MATRIX[Self::MATCH][Self::BIG_DELETE]
                        + self.line_bounds_scoring.score_side(0, file_ids[0], line_indices[0]),
                ),
                previous_step: Cell::from(None),
            };
            state[Self::BIG_INSERT] = DpSubstate {
                score: Cell::from(
                    starting_score
                        + Self::EOL_TRANSITION_MATRIX[Self::MATCH][Self::BIG_INSERT]
                        + self.line_bounds_scoring.score_side(1, file_ids[1], line_indices[1]),
                ),
                previous_step: Cell::from(None),
            }
        } else {
            state[Self::BIG_DELETE] = DpSubstate {
                score: Cell::from(TScore::NEG_INFINITY),
                previous_step: Cell::from(None),
            };
            state[Self::BIG_INSERT] = DpSubstate {
                score: Cell::from(TScore::NEG_INFINITY),
                previous_step: Cell::from(None),
            }
        }
    }
}

impl<'a> AlignmentScorer for MultilineGapsScoring<'a> {
    fn score_gaps_between(
        &self,
        _file_ids: [usize; 2],
        start_indices: [usize; 2],
        end_indices: [usize; 2],
    ) -> super::TScore {
        let gap_length = (end_indices[0] - start_indices[0]) + (end_indices[1] - start_indices[1]);
        gap_length as TScore * Self::SMALL_GAP_COST
    }

    // TODO: this is not exactly correct, possibly redesign/refactor
    fn score_alignment_steps(
        &self,
        alignment: &[DiffOp],
        start: [usize; 2],
        end: [usize; 2],
        file_ids: [usize; 2],
    ) -> Option<Vec<TScore>> {
        let mut position = start;
        let transition_cost = |position: &[usize; 2]| -> TScore {
            if self.get_line_indices(file_ids, position.map(WordIndex::new)).is_some() {
                Self::EOL_TRANSITION_MATRIX[Self::MATCH][Self::MATCH]
            } else {
                0.0
            }
        };
        let mut result = vec![transition_cost(&position)];
        for &op in alignment {
            if op == DiffOp::Match {
                let symbols = [0, 1].map(|side| self.symbols[file_ids[side]][side][position[side]]);
                if symbols[0] != symbols[1] {
                    return None;
                }
                result.push(self.information_values[file_ids[0]][0][position[0]]);
            } else {
                result.push(Self::SMALL_GAP_COST);
            }
            for side in 0..2 {
                position[side] += op.movement()[side];
            }
            result.push(transition_cost(&position));
        }
        if position != end {
            return None;
        }
        Some(result)
    }

    fn as_prioritizer(&self) -> &dyn AlignmentPrioritizer {
        self
    }
}
