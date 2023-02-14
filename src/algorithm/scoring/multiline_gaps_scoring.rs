use std::cell::Cell;

use crate::algorithm::{
    indices::{IndexConverter, LineIndex, WordIndex},
    preprocess::{information_values, internalize_parts},
    DiffOp, PartitionedText,
};

use super::{
    line_bounds_scoring::LineBoundsScoring,
    line_skipping::{LineGapsScoring, SKIP_SIMILARITY_THRESHOLD},
    AlignmentPrioritizer, AlignmentScorer, DpSubstate, TScore,
};

pub struct MultilineGapsScoring<'a> {
    index_converters: &'a [[IndexConverter; 2]],
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    information_values: Vec<[Vec<TScore>; 2]>,
    information_prefix_sums: Vec<[Vec<TScore>; 2]>,
    line_gap_scoring: LineGapsScoring<'a>,
}

fn prefix_sums(values: &Vec<TScore>) -> Vec<TScore> {
    let mut result = vec![0.0; values.len() + 1];
    for (i, val) in values.iter().enumerate() {
        result[i + 1] = result[i] + val;
    }
    result
}

impl<'a> MultilineGapsScoring<'a> {
    const SUBSTATES_COUNT: usize = 3;
    const MATCH: usize = 0;
    const BIG_DELETE: usize = 1;
    const BIG_INSERT: usize = 2;
    const SMALL_GAP_FACTOR: f64 = -SKIP_SIMILARITY_THRESHOLD / ((1.0 - SKIP_SIMILARITY_THRESHOLD) * 2.0);

    pub(in crate::algorithm) fn new(
        text_words: &[[PartitionedText; 2]],
        index_converters: &'a [[IndexConverter; 2]],
        line_bounds_scoring: &'a LineBoundsScoring,
    ) -> Self {
        let symbols = internalize_parts(text_words);
        let information_values = information_values(text_words);
        let information_prefix_sums = information_values
            .iter()
            .map(|file_values| [0, 1].map(|side| prefix_sums(&file_values[side])))
            .collect();
        MultilineGapsScoring {
            index_converters,
            symbols,
            information_values,
            information_prefix_sums,
            line_gap_scoring: LineGapsScoring::new(line_bounds_scoring),
        }
    }

    fn information_between(&self, side: usize, file_id: usize, start: WordIndex, end: WordIndex) -> TScore {
        self.information_prefix_sums[file_id][side][end.raw()]
            - self.information_prefix_sums[file_id][side][start.raw()]
    }

    fn get_line_index(&self, side: usize, file_id: usize, word_index: WordIndex) -> Option<LineIndex> {
        let line = self.index_converters[file_id][side].word_to_line_before(word_index);
        if self.index_converters[file_id][side].line_to_word(line) == word_index {
            Some(line)
        } else {
            None
        }
    }

    fn transition_cost(&self, file_ids: [usize; 2], word_indices: [WordIndex; 2], from: usize, to: usize) -> TScore {
        let line_indices = [0, 1].map(|side| self.get_line_index(side, file_ids[side], word_indices[side]));
        if from == to {
            return 0.0;
        }
        let mut score = 0.0;
        for (side, &state) in [Self::BIG_DELETE, Self::BIG_INSERT].iter().enumerate() {
            if from == state || to == state {
                score += line_indices[side].map_or(TScore::NEG_INFINITY, |line_index| {
                    self.line_gap_scoring.gap_edge(side, file_ids[side], line_index)
                });
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
        let match_state_score = match step {
            DiffOp::Match => {
                let word_indices = dp_position.map(|x| x - 1);
                let symbols = [0, 1].map(|side| self.symbols[file_ids[side]][side][word_indices[side]]);
                if symbols[0] != symbols[1] {
                    return;
                }
                self.information_values[file_ids[0]][0][word_indices[0]]
            }
            DiffOp::Delete => self.information_values[file_ids[0]][0][dp_position[0] - 1] * Self::SMALL_GAP_FACTOR,
            DiffOp::Insert => self.information_values[file_ids[1]][1][dp_position[1] - 1] * Self::SMALL_GAP_FACTOR,
        };
        scores_without_transition[Self::MATCH] = state_before_step[Self::MATCH].score.get() + match_state_score;

        for from_substate in 0..Self::SUBSTATES_COUNT {
            for to_substate in 0..Self::SUBSTATES_COUNT {
                let proposed_score = scores_without_transition[from_substate]
                    + self.transition_cost(file_ids, dp_position.map(WordIndex::new), from_substate, to_substate);
                improve(to_substate, proposed_score, Some((step, from_substate)));
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
            score: Cell::from(starting_score),
            previous_step: Cell::from(None),
        };

        for (side, &gap_state) in [Self::BIG_DELETE, Self::BIG_INSERT].iter().enumerate() {
            let score = if let Some(line_index) =
                self.get_line_index(side, file_ids[side], WordIndex::new(dp_position[side]))
            {
                starting_score + self.line_gap_scoring.gap_edge(side, file_ids[side], line_index)
            } else {
                TScore::NEG_INFINITY
            };
            state[gap_state] = DpSubstate {
                score: Cell::from(score),
                previous_step: Cell::from(None),
            };
        }
    }
}

impl<'a> AlignmentScorer for MultilineGapsScoring<'a> {
    fn score_gaps_between(
        &self,
        file_ids: [usize; 2],
        start_indices: [usize; 2],
        end_indices: [usize; 2],
    ) -> super::TScore {
        [0, 1]
            .map(|side| {
                self.information_between(
                    side,
                    file_ids[side],
                    WordIndex::new(start_indices[side]),
                    WordIndex::new(end_indices[side]),
                ) * Self::SMALL_GAP_FACTOR
            })
            .iter()
            .sum()
    }

    fn score_alignment_steps(
        &self,
        alignment: &[DiffOp],
        start: [usize; 2],
        end: [usize; 2],
        file_ids: [usize; 2],
    ) -> Option<Vec<TScore>> {
        let transition_cost = |position: &[usize; 2], from: usize, to: usize| -> TScore {
            self.transition_cost(file_ids, position.map(WordIndex::new), from, to)
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
                    let file_id = file_ids[side];
                    let gap_state = if op == DiffOp::Delete {
                        Self::BIG_DELETE
                    } else {
                        Self::BIG_INSERT
                    };

                    let mut first_line_break: Option<(usize, LineIndex, TScore)> = None;
                    let mut last_line_break: Option<(usize, LineIndex, TScore)> = None;

                    let start_index = index_in_alignment;
                    let start_position = position;
                    while index_in_alignment < alignment.len() && alignment[index_in_alignment] == op {
                        if let Some(line_index) = self.get_line_index(side, file_id, WordIndex::new(position[side])) {
                            let edge_score = self.line_gap_scoring.gap_edge(side, file_id, line_index);
                            if first_line_break.is_none() {
                                first_line_break = Some((index_in_alignment, line_index, edge_score));
                            }

                            last_line_break = Some((index_in_alignment, line_index, edge_score));
                        }
                        index_in_alignment += 1;
                        position[side] += 1;
                    }
                    if let Some(line_index) = self.get_line_index(side, file_id, WordIndex::new(position[side])) {
                        let bound_score = self.line_gap_scoring.gap_edge(side, file_id, line_index);
                        last_line_break = Some((index_in_alignment, line_index, bound_score));
                    }

                    let score_with_match_state = self.information_between(
                        side,
                        file_id,
                        WordIndex::new(start_position[side]),
                        WordIndex::new(position[side]),
                    ) * Self::SMALL_GAP_FACTOR;
                    let end_position = position;
                    position = start_position;
                    if let Some(first_line_break) = first_line_break {
                        let indel_start_words = self.index_converters[file_id][side].line_to_word(first_line_break.1);
                        let score_before_indel_state = self.information_between(
                            side,
                            file_id,
                            WordIndex::new(start_position[side]),
                            indel_start_words,
                        ) * Self::SMALL_GAP_FACTOR;

                        let last_line_break = last_line_break.unwrap();
                        let indel_end_words = self.index_converters[file_id][side].line_to_word(last_line_break.1);
                        let score_after_indel_state = self.information_between(
                            side,
                            file_id,
                            indel_end_words,
                            WordIndex::new(end_position[side]),
                        ) * Self::SMALL_GAP_FACTOR;
                        let score_with_indel_state =
                            score_before_indel_state + score_after_indel_state + first_line_break.2 + last_line_break.2;
                        if score_with_indel_state > score_with_match_state {
                            for _ in start_index..first_line_break.0 {
                                result.push(transition_cost(&position, current_state, Self::MATCH));
                                current_state = Self::MATCH;
                                result.push(
                                    self.information_values[file_id][side][position[side]] * Self::SMALL_GAP_FACTOR,
                                );
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
                                result.push(
                                    self.information_values[file_id][side][position[side]] * Self::SMALL_GAP_FACTOR,
                                );
                                position[side] += 1;
                            }
                            continue;
                        }
                    }
                    for _ in start_index..index_in_alignment {
                        result.push(transition_cost(&position, current_state, Self::MATCH));
                        current_state = Self::MATCH;
                        result.push(self.information_values[file_id][side][position[side]] * Self::SMALL_GAP_FACTOR);
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
