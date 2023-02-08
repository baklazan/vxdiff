use std::cell::Cell;

use crate::algorithm::{
    indices::{IndexConverter, LineIndex, WordIndex},
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

    fn transition_cost(&self, file_ids: [usize; 2], line_indices: [LineIndex; 2], from: usize, to: usize) -> TScore {
        let mut score = Self::EOL_TRANSITION_MATRIX[from][to];
        if from != to {
            if from == Self::BIG_DELETE || to == Self::BIG_DELETE {
                score += self.line_bounds_scoring.score_side(0, file_ids[0], line_indices[0]);
            }
            if from == Self::BIG_INSERT || to == Self::BIG_INSERT {
                score += self.line_bounds_scoring.score_side(1, file_ids[1], line_indices[1]);
            }
        }
        score
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

        if let Some(line_indices) = self.get_line_indices(file_ids, dp_position.map(WordIndex::new)) {
            for from_substate in 0..Self::SUBSTATES_COUNT {
                for to_substate in 0..Self::SUBSTATES_COUNT {
                    let proposed_score = scores_without_transition[from_substate]
                        + self.transition_cost(file_ids, line_indices, from_substate, to_substate);
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

    fn score_alignment_steps(
        &self,
        alignment: &[DiffOp],
        start: [usize; 2],
        end: [usize; 2],
        file_ids: [usize; 2],
    ) -> Option<Vec<TScore>> {
        let transition_cost = |position: &[usize; 2], from: usize, to: usize| -> TScore {
            if let Some(line_indices) = self.get_line_indices(file_ids, position.map(WordIndex::new)) {
                self.transition_cost(file_ids, line_indices, from, to)
            } else {
                0.0
            }
        };
        let mut result = vec![];

        let mut position = start;
        let mut index_in_alignment = 0;
        let mut current_state = Self::MATCH;
        while index_in_alignment < alignment.len() {
            let op = alignment[index_in_alignment];
            match op {
                DiffOp::Match => {
                    let symbols = [0, 1].map(|side| self.symbols[file_ids[side]][side][position[side]]);
                    if symbols[0] != symbols[1] {
                        return None;
                    }
                    result.push(transition_cost(&position, current_state, Self::MATCH));
                    current_state = Self::MATCH;
                    result.push(self.information_values[file_ids[0]][0][position[0]]);
                    for side in 0..2 {
                        position[side] += op.movement()[side];
                    }
                    index_in_alignment += 1;
                }
                DiffOp::Delete | DiffOp::Insert => {
                    let side = if op == DiffOp::Delete { 0 } else { 1 };
                    let gap_state = if op == DiffOp::Delete {
                        Self::BIG_DELETE
                    } else {
                        Self::BIG_INSERT
                    };

                    let mut first_line_break: Option<(usize, TScore)> = None;
                    let mut last_line_break: Option<(usize, TScore)> = None;

                    let start_index = index_in_alignment;
                    let start_position = position;
                    while index_in_alignment < alignment.len() && alignment[index_in_alignment] == op {
                        if let Some(line_indices) = self.get_line_indices(file_ids, position.map(WordIndex::new)) {
                            let bound_score =
                                self.line_bounds_scoring
                                    .score_side(side, file_ids[side], line_indices[side]);
                            if first_line_break.is_none() {
                                first_line_break = Some((index_in_alignment, bound_score));
                            }
                            last_line_break = Some((index_in_alignment, bound_score));
                        }
                        index_in_alignment += 1;
                        position[side] += 1;
                    }
                    if let Some(line_indices) = self.get_line_indices(file_ids, position.map(WordIndex::new)) {
                        let bound_score = self
                            .line_bounds_scoring
                            .score_side(side, file_ids[side], line_indices[side]);
                        last_line_break = Some((index_in_alignment, bound_score));
                    }

                    let score_with_match_state = (index_in_alignment - start_index) as TScore * Self::SMALL_GAP_COST;
                    position = start_position;
                    if let Some(first_line_break) = first_line_break {
                        let last_line_break = last_line_break.unwrap();
                        let steps_with_match =
                            first_line_break.0 - start_index + index_in_alignment - last_line_break.0;
                        let score_with_insert_state = steps_with_match as TScore * Self::SMALL_GAP_COST
                            + 2.0 * Self::BIG_GAP_EDGE_COST
                            + first_line_break.1
                            + last_line_break.1;
                        if score_with_insert_state > score_with_match_state {
                            for _ in start_index..first_line_break.0 {
                                result.push(transition_cost(&position, current_state, Self::MATCH));
                                current_state = Self::MATCH;
                                result.push(Self::SMALL_GAP_COST);
                                position[side] += 1;
                            }
                            for _ in first_line_break.0..last_line_break.0 {
                                result.push(transition_cost(&position, current_state, gap_state));
                                current_state = gap_state;
                                result.push(0.0);
                                position[side] += 1;
                            }
                            for _ in last_line_break.0..index_in_alignment {
                                result.push(transition_cost(&position, current_state, Self::MATCH));
                                current_state = Self::MATCH;
                                result.push(Self::SMALL_GAP_COST);
                                position[side] += 1;
                            }
                            continue;
                        }
                    }
                    for _ in start_index..index_in_alignment {
                        result.push(transition_cost(&position, current_state, Self::MATCH));
                        current_state = Self::MATCH;
                        result.push(Self::SMALL_GAP_COST);
                        position[side] += 1;
                    }
                }
            }
        }
        result.push(transition_cost(&position, current_state, Self::MATCH));
        if position != end {
            return None;
        }
        Some(result)
    }

    fn as_prioritizer(&self) -> &dyn AlignmentPrioritizer {
        self
    }
}
