use super::*;
use string_interner::StringInterner;

pub type TScore = f64;

pub trait ScoreState: Clone {
    const SUBSTATES_COUNT: usize;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DpDirection {
    Backward,
    Forward,
}

pub trait AlignmentScoringMethod {
    type State: ScoreState;

    fn starting_state(&self, starting_score: TScore) -> Self::State;

    fn consider_step(
        &self,
        word_indices: [usize; 2],
        file_ids: [usize; 2],
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
        direction: DpDirection,
    );

    fn is_match(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> bool;

    fn append_gaps(
        &self,
        file_ids: [usize; 2],
        start_indices: [usize; 2],
        end_indices: [usize; 2],
        starting_substate: usize,
    ) -> TScore;

    fn substate_movement(&self, state: &Self::State, substate: usize) -> Option<(DiffOp, usize)>;

    fn substate_score(
        &self,
        state: &Self::State,
        substate: usize,
        file_ids: [usize; 2],
        position: [usize; 2],
        direction: DpDirection,
    ) -> TScore;
}

#[derive(Clone, Copy)]
pub struct InputSliceBounds {
    pub file_ids: [usize; 2],
    pub start: [usize; 2],
    pub direction: DpDirection,
}

impl InputSliceBounds {
    fn global_index(&self, side: usize, local_index: usize) -> usize {
        match self.direction {
            DpDirection::Forward => self.start[side] + local_index,
            DpDirection::Backward => self.start[side] - local_index,
        }
    }

    fn global_indices(&self, local_indices: [usize; 2]) -> [usize; 2] {
        [0, 1].map(|side| self.global_index(side, local_indices[side]))
    }

    fn local_index(&self, side: usize, global_index: usize) -> usize {
        match self.direction {
            DpDirection::Forward => global_index - self.start[side],
            DpDirection::Backward => self.start[side] - global_index,
        }
    }

    fn local_indices(&self, global_indices: [usize; 2]) -> [usize; 2] {
        [0, 1].map(|side| self.local_index(side, global_indices[side]))
    }
}

pub struct AlignmentSliceScoring<'a, Scoring: AlignmentScoringMethod> {
    pub slice: InputSliceBounds,
    pub scoring: &'a Scoring,
}

impl<'a, Scoring: AlignmentScoringMethod> AlignmentSliceScoring<'a, Scoring> {
    pub fn starting_state(&self, starting_score: TScore) -> Scoring::State {
        self.scoring.starting_state(starting_score)
    }

    pub fn consider_step(
        &self,
        word_indices: [usize; 2],
        state_after_move: Scoring::State,
        state: &mut Scoring::State,
        step: DiffOp,
    ) {
        self.scoring.consider_step(
            self.slice.global_indices(word_indices),
            self.slice.file_ids,
            state_after_move,
            state,
            step,
            self.slice.direction,
        )
    }

    pub fn is_match(&self, word_indices: [usize; 2]) -> bool {
        self.scoring
            .is_match(self.slice.global_indices(word_indices), self.slice.file_ids)
    }

    pub fn append_gaps(&self, mut start_indices: [usize; 2], mut end_indices: [usize; 2], substate: usize) -> TScore {
        if self.slice.direction == DpDirection::Backward {
            (start_indices, end_indices) = (end_indices, start_indices);
        }
        self.scoring.append_gaps(
            self.slice.file_ids,
            self.slice.global_indices(start_indices),
            self.slice.global_indices(end_indices),
            substate,
        )
    }

    pub fn substate_movement(&self, state: &Scoring::State, substate: usize) -> Option<(DiffOp, usize)> {
        self.scoring.substate_movement(state, substate)
    }

    pub fn substate_score(&self, state: &Scoring::State, substate: usize, position: [usize; 2]) -> TScore {
        self.scoring.substate_score(
            state,
            substate,
            self.slice.file_ids,
            self.slice.global_indices(position),
            self.slice.direction,
        )
    }
}

#[derive(Clone)]
pub struct SimpleScoreState<const SUBSTATES_COUNT: usize> {
    previous_steps: [Option<(DiffOp, usize)>; SUBSTATES_COUNT],
    scores: [TScore; SUBSTATES_COUNT],
}

impl<const SUBSTATES_COUNT: usize> ScoreState for SimpleScoreState<SUBSTATES_COUNT> {
    const SUBSTATES_COUNT: usize = SUBSTATES_COUNT;
}

fn information_values(texts: &[[PartitionedText; 2]]) -> Vec<[Vec<TScore>; 2]> {
    use std::collections::HashMap;

    let mut char_frequencies: HashMap<char, usize> = HashMap::new();
    let mut total_chars: usize = 0;

    for file_texts in texts {
        for side_text in file_texts {
            for c in side_text.text.chars() {
                char_frequencies.insert(c, char_frequencies.get(&c).unwrap_or(&0) + 1);
                total_chars += 1;
            }
        }
    }

    let mut result = vec![];
    for file_texts in texts {
        let mut file_values = [vec![], vec![]];
        for (side, side_text) in file_texts.iter().enumerate() {
            for i in 0..side_text.word_count() {
                let word = side_text.get_word(i);
                let mut score: f64 = 0.0;
                for c in word.chars() {
                    let char_frequency = (*char_frequencies.get(&c).unwrap() as f64) / total_chars as f64;
                    score += -char_frequency.log2() / 5.0;
                }
                file_values[side].push(score);
            }
        }
        result.push(file_values);
    }
    result
}

fn internalize_words(texts: &[[PartitionedText; 2]]) -> Vec<[Vec<string_interner::symbol::SymbolU32>; 2]> {
    let mut symbols = vec![[vec![], vec![]]; texts.len()];
    let mut interner = StringInterner::default();
    for (file_id, file_texts) in texts.iter().enumerate() {
        for (side, side_text) in file_texts.iter().enumerate() {
            for i in 0..side_text.word_count() {
                let word = side_text.get_word(i);
                symbols[file_id][side].push(interner.get_or_intern(word));
            }
        }
    }
    symbols
}

pub struct SimpleScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    information_values: Vec<[Vec<TScore>; 2]>,
}

impl SimpleScoring {
    const P_MATCH: TScore = 0.97;

    fn gap_score() -> TScore {
        (1.0 - Self::P_MATCH).log2()
    }

    fn match_score(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        let old_symbol = self.symbols[file_ids[0]][0][word_indices[0]];
        let new_symbol = self.symbols[file_ids[1]][1][word_indices[1]];
        if old_symbol != new_symbol {
            return TScore::NEG_INFINITY;
        }
        Self::P_MATCH.log2() + self.information_values[file_ids[0]][0][word_indices[0]]
    }

    #[allow(dead_code)]
    pub fn new(texts: &[[PartitionedText; 2]]) -> SimpleScoring {
        SimpleScoring {
            symbols: internalize_words(texts),
            information_values: information_values(texts),
        }
    }
}

impl AlignmentScoringMethod for SimpleScoring {
    type State = SimpleScoreState<1>;

    fn starting_state(&self, starting_score: TScore) -> Self::State {
        SimpleScoreState {
            previous_steps: [None],
            scores: [starting_score],
        }
    }

    fn consider_step(
        &self,
        word_indices: [usize; 2],
        file_ids: [usize; 2],
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
        direction: DpDirection,
    ) {
        let step_score = if step == DiffOp::Match {
            match direction {
                DpDirection::Backward => self.match_score(word_indices, file_ids),
                DpDirection::Forward => self.match_score([word_indices[0] - 1, word_indices[1] - 1], file_ids),
            }
        } else {
            Self::gap_score()
        };
        let proposed_score = state_after_move.scores[0] + step_score;
        if proposed_score > state.scores[0] {
            state.scores[0] = proposed_score;
            state.previous_steps[0] = Some((step, 0));
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
        _starting_substate: usize,
    ) -> TScore {
        Self::gap_score() * (end_indices[0] - start_indices[0] + end_indices[1] - start_indices[1]) as f64
    }

    fn substate_movement(&self, state: &Self::State, substate: usize) -> Option<(DiffOp, usize)> {
        state.previous_steps[substate]
    }

    fn substate_score(
        &self,
        state: &Self::State,
        substate: usize,
        _file_ids: [usize; 2],
        _position: [usize; 2],
        _direction: DpDirection,
    ) -> TScore {
        state.scores[substate]
    }
}

pub struct AffineScoring {
    // these matrices should be constant, but Rust doesn't allow floating point operations in const expressions
    transition_matrix: [[TScore; 3]; 3],
    transition_matrix_newline: [[TScore; 3]; 3],

    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    pub information_values: Vec<[Vec<TScore>; 2]>,
    is_white: Vec<[Vec<bool>; 2]>,
    line_splits_at: Vec<[Vec<bool>; 2]>,
    nearest_line_split_forward: Vec<[Vec<usize>; 2]>,
    nearest_line_split_backward: Vec<[Vec<usize>; 2]>,
    bound_score: Vec<[Vec<TScore>; 2]>,
}

impl AffineScoring {
    const MATCH: usize = 0;
    const GAP: usize = 1;
    const WHITE_GAP: usize = 2;

    fn compute_transition_matrix(
        p_start_gap: f64,
        p_end_gap: f64,
        p_start_white_gap: f64,
        p_end_white_gap: f64,
    ) -> [[TScore; 3]; 3] {
        let p_stay_match = 1.0 - p_start_gap - p_start_white_gap;
        [
            [p_stay_match.log2(), p_start_gap.log2(), p_start_white_gap.log2()],
            [p_end_gap.log2(), (1.0 - p_end_gap).log2(), TScore::NEG_INFINITY],
            [
                p_end_white_gap.log2(),
                TScore::NEG_INFINITY,
                (1.0 - p_end_white_gap).log2(),
            ],
        ]
    }

    const P_END_GAP: f64 = 0.3;
    const P_START_GAP: f64 = 0.05;
    const P_END_WHITE_GAP: f64 = 0.4;
    const P_START_WHITE_GAP: f64 = 0.2;
    const NEWLINE_STATE_CHANGE_COEF: f64 = 1.1;

    const BASE_BOUND_SCORE: TScore = -1.0;
    const LINE_CONTENT_COEF: TScore = -0.8;

    pub fn new(texts: &[[PartitionedText; 2]]) -> AffineScoring {
        let information_values = information_values(texts);

        let mut is_white = vec![];
        let mut line_splits_at = vec![];
        let mut bound_score = vec![];
        let mut nearest_line_split_forward = vec![];
        let mut nearest_line_split_backward = vec![];
        for (file_id, file_texts) in texts.iter().enumerate() {
            let mut file_is_white = [vec![], vec![]];
            let mut file_line_splits_at = [vec![true], vec![true]];
            let mut file_bound_score = [
                vec![TScore::NEG_INFINITY; file_texts[0].word_count() + 1],
                vec![TScore::NEG_INFINITY; file_texts[1].word_count() + 1],
            ];
            let mut file_nearest_split_forward = [vec![], vec![]];
            let mut file_nearest_split_backward = [vec![], vec![]];
            for side in 0..2 {
                let mut this_line_value = 0.0;
                let mut last_line_value = 0.0;
                let mut last_line_end = 0;
                for i in 0..file_texts[side].word_count() {
                    let word = file_texts[side].get_word(i);
                    let mut white = true;
                    for c in word.chars() {
                        if !c.is_whitespace() {
                            white = false;
                        }
                    }
                    file_is_white[side].push(white);

                    this_line_value += information_values[file_id][side][i];
                    if word == "\n" || i + 1 >= file_texts[side].word_count() {
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
            is_white.push(file_is_white);
            line_splits_at.push(file_line_splits_at);
            bound_score.push(file_bound_score);
            nearest_line_split_backward.push(file_nearest_split_backward);
            nearest_line_split_forward.push(file_nearest_split_forward);
        }

        let transition_matrix = Self::compute_transition_matrix(
            Self::P_START_GAP,
            Self::P_END_GAP,
            Self::P_START_WHITE_GAP,
            Self::P_END_WHITE_GAP,
        );
        let transition_matrix_newline = Self::compute_transition_matrix(
            Self::P_START_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
            Self::P_END_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
            Self::P_START_WHITE_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
            Self::P_END_WHITE_GAP * Self::NEWLINE_STATE_CHANGE_COEF,
        );

        AffineScoring {
            transition_matrix,
            transition_matrix_newline,
            symbols: internalize_words(texts),
            information_values,
            is_white,
            line_splits_at,
            nearest_line_split_backward,
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
        direction: DpDirection,
    ) -> TScore {
        let transition_matrix =
            if self.line_splits_at[file_ids[0]][0][position[0]] && self.line_splits_at[file_ids[1]][1][position[1]] {
                &self.transition_matrix_newline
            } else {
                &self.transition_matrix
            };
        if direction == DpDirection::Forward {
            transition_matrix[from_state][to_state]
        } else {
            transition_matrix[to_state][from_state]
        }
    }

    pub fn alignment_score(&self, alignment: &[DiffOp], file_ids: [usize; 2], start: [usize; 2]) -> Option<TScore> {
        let mut states = vec![];
        let mut gap_started = None;
        let mut gap_is_white = true;
        let mut word_indices = start;
        for (i, &op) in alignment.iter().enumerate() {
            if op == DiffOp::Match {
                if !self.is_match(word_indices, file_ids) {
                    return None;
                }
                if gap_started.is_some() {
                    let gap_type = if gap_is_white { Self::WHITE_GAP } else { Self::GAP };
                    for _ in gap_started.unwrap()..i {
                        states.push(gap_type);
                    }
                    gap_started = None;
                }
                states.push(Self::MATCH);
            } else {
                if gap_started.is_none() {
                    gap_started = Some(i);
                    gap_is_white = true;
                }
                let side = if op == DiffOp::Delete { 0 } else { 1 };
                gap_is_white &= self.is_white[file_ids[side]][side][word_indices[side]];
            }
            for side in 0..2 {
                word_indices[side] += op.movement()[side];
            }
        }
        if gap_started.is_some() {
            let gap_type = if gap_is_white { Self::WHITE_GAP } else { Self::GAP };
            for _ in gap_started.unwrap()..alignment.len() {
                states.push(gap_type);
            }
        }

        for side in 0..2 {
            if word_indices[side] != self.symbols[file_ids[side]][side].len() {
                return None;
            }
        }

        let mut result = 0.0;
        let mut word_indices = start;
        for (i, &op) in alignment.iter().enumerate() {
            if i >= 1 {
                let transition_matrix = if self.line_splits_at[file_ids[0]][0][word_indices[0]]
                    && self.line_splits_at[file_ids[1]][1][word_indices[1]]
                {
                    &self.transition_matrix_newline
                } else {
                    &self.transition_matrix
                };
                result += transition_matrix[states[i - 1]][states[i]];
            }
            if op == DiffOp::Match {
                result += self.information_values[file_ids[0]][0][word_indices[0]];
            }

            for side in 0..2 {
                word_indices[side] += op.movement()[side];
            }
        }
        Some(result)
    }
}

impl AlignmentScoringMethod for AffineScoring {
    type State = SimpleScoreState<3>;

    fn starting_state(&self, starting_score: TScore) -> Self::State {
        Self::State {
            previous_steps: [None, None, None],
            scores: [starting_score, starting_score, starting_score],
        }
    }

    fn consider_step(
        &self,
        dp_position: [usize; 2],
        file_ids: [usize; 2],
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
        direction: DpDirection,
    ) {
        let word_indices = match direction {
            DpDirection::Backward => dp_position,
            DpDirection::Forward => [dp_position[0].wrapping_sub(1), dp_position[1].wrapping_sub(1)],
        };

        let mut improve = |substate: usize, proposed: TScore, proposed_movement: &Option<(DiffOp, usize)>| {
            if state.scores[substate] < proposed {
                state.scores[substate] = proposed;
                state.previous_steps[substate] = *proposed_movement;
            }
        };

        if step == DiffOp::Match {
            let syms = [0, 1].map(|side| self.symbols[file_ids[side]][side][word_indices[side]]);

            if syms[0] != syms[1] {
                return;
            }
            let score_without_transition =
                state_after_move.scores[Self::MATCH] + self.information_values[file_ids[0]][0][word_indices[0]];
            let movement = Some((DiffOp::Match, Self::MATCH));
            for to_state in [Self::MATCH, Self::GAP, Self::WHITE_GAP] {
                improve(
                    to_state,
                    score_without_transition
                        + self.transition_cost(file_ids, dp_position, Self::MATCH, to_state, direction),
                    &movement,
                );
            }
        } else {
            let score_without_transition = state_after_move.scores[Self::GAP];
            let movement = Some((step, Self::GAP));
            for to_state in [Self::MATCH, Self::GAP] {
                improve(
                    to_state,
                    score_without_transition
                        + self.transition_cost(file_ids, dp_position, Self::GAP, to_state, direction),
                    &movement,
                );
            }
            if step == DiffOp::Insert && self.is_white[file_ids[1]][1][word_indices[1]]
                || step == DiffOp::Delete && self.is_white[file_ids[0]][0][word_indices[0]]
            {
                let score_without_transition = state_after_move.scores[Self::WHITE_GAP];
                let movement = Some((step, Self::WHITE_GAP));
                for to_state in [Self::MATCH, Self::WHITE_GAP] {
                    improve(
                        to_state,
                        score_without_transition
                            + self.transition_cost(file_ids, dp_position, Self::WHITE_GAP, to_state, direction),
                        &movement,
                    );
                }
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

    fn substate_movement(&self, state: &Self::State, substate: usize) -> Option<(DiffOp, usize)> {
        state.previous_steps[substate]
    }

    fn substate_score(
        &self,
        state: &Self::State,
        substate: usize,
        file_ids: [usize; 2],
        position: [usize; 2],
        direction: DpDirection,
    ) -> TScore {
        if state.previous_steps[substate].is_none() {
            state.scores[substate]
        } else {
            state.scores[substate]
                - self.transition_cost(
                    file_ids,
                    position,
                    state.previous_steps[substate].unwrap().1,
                    substate,
                    direction,
                )
        }
    }
}

pub trait FragmentBoundsScoringMethod {
    const SUPERSECTION_THRESHOLD: TScore;
    const MOVED_SUPERSECTION_THRESHOLD: TScore;

    fn fragment_bound_penalty(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore;
    fn is_viable_bound(&self, side: usize, index: usize, file_id: usize) -> bool;
    fn nearest_bound(&self, side: usize, index: usize, file_id: usize, direction: DpDirection) -> usize;
}

impl FragmentBoundsScoringMethod for AffineScoring {
    const SUPERSECTION_THRESHOLD: TScore = 5.0;
    const MOVED_SUPERSECTION_THRESHOLD: TScore = 10.0;

    fn fragment_bound_penalty(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        self.bound_score[file_ids[0]][0][word_indices[0]] + self.bound_score[file_ids[1]][1][word_indices[1]]
    }

    fn is_viable_bound(&self, side: usize, index: usize, file_id: usize) -> bool {
        index < self.bound_score[file_id][side].len() && self.bound_score[file_id][side][index] != TScore::NEG_INFINITY
    }

    fn nearest_bound(&self, side: usize, index: usize, file_id: usize, direction: DpDirection) -> usize {
        (match direction {
            DpDirection::Backward => &self.nearest_line_split_backward,
            DpDirection::Forward => &self.nearest_line_split_forward,
        })[file_id][side][index]
    }
}

pub struct BoundsSliceScoring<'a, Scoring: FragmentBoundsScoringMethod> {
    pub slice: InputSliceBounds,
    pub scoring: &'a Scoring,
}

impl<'a, Scoring: FragmentBoundsScoringMethod> BoundsSliceScoring<'a, Scoring> {
    pub fn fragment_bound_penalty(&self, word_indices: [usize; 2]) -> TScore {
        self.scoring
            .fragment_bound_penalty(self.slice.global_indices(word_indices), self.slice.file_ids)
    }

    pub fn nearest_bound_point(&self, word_indices: [usize; 2]) -> [usize; 2] {
        let global_point_indices = [0, 1].map(|side| {
            self.scoring.nearest_bound(
                side,
                self.slice.global_index(side, word_indices[side]),
                self.slice.file_ids[side],
                self.slice.direction,
            )
        });
        self.slice.local_indices(global_point_indices)
    }
}
