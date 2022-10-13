use super::*;
use string_interner::StringInterner;

pub type TScore = f64;

pub trait ScoreState: Clone {
    const SUBSTATES_COUNT: usize;

    fn substate_scores(&self) -> &[TScore];
    fn best_score(&self) -> TScore;
    fn substate_movements(&self) -> &[Option<(DiffOp, usize)>];
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
}

#[derive(Clone)]
pub struct SimpleScoreState<const SUBSTATES_COUNT: usize> {
    next_steps: [Option<(DiffOp, usize)>; SUBSTATES_COUNT],
    scores: [TScore; SUBSTATES_COUNT],
}

impl<const SUBSTATES_COUNT: usize> ScoreState for SimpleScoreState<SUBSTATES_COUNT> {
    const SUBSTATES_COUNT: usize = SUBSTATES_COUNT;

    fn best_score(&self) -> TScore {
        let mut best = TScore::NEG_INFINITY;
        for score in &self.scores {
            best = TScore::max(*score, best);
        }
        best
    }

    fn substate_scores(&self) -> &[TScore] {
        &self.scores
    }

    fn substate_movements(&self) -> &[Option<(DiffOp, usize)>] {
        &self.next_steps
    }
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
            next_steps: [None],
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
            state.next_steps[0] = Some((step, 0));
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
}

pub struct AffineScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    pub information_values: Vec<[Vec<TScore>; 2]>,
    is_white: Vec<[Vec<bool>; 2]>,
    line_splits_at: Vec<[Vec<bool>; 2]>,
    nearest_line_split_forward: Vec<[Vec<usize>; 2]>,
    nearest_line_split_backward: Vec<[Vec<usize>; 2]>,
    bound_score: Vec<[Vec<TScore>; 2]>,
}

impl AffineScoring {
    const P_END_GAP: f64 = 0.3;
    const P_START_GAP: f64 = 0.05;
    const NEWLINE_STATE_CHANGE_COEF: f64 = 1.1;
    const P_END_WHITE_GAP: f64 = 0.4;
    const P_START_WHITE_GAP: f64 = 0.2;

    const BASE_BOUND_SCORE: TScore = -1.0;
    const LINE_CONTENT_COEF: TScore = -0.8;

    const MATCH: usize = 0;
    const GAP: usize = 1;
    const WHITE_GAP: usize = 2;

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

        AffineScoring {
            symbols: internalize_words(texts),
            information_values,
            is_white,
            line_splits_at,
            nearest_line_split_backward,
            nearest_line_split_forward,
            bound_score,
        }
    }
}

impl AlignmentScoringMethod for AffineScoring {
    type State = SimpleScoreState<3>;

    fn starting_state(&self, starting_score: TScore) -> Self::State {
        Self::State {
            next_steps: [None, None, None],
            scores: [starting_score, starting_score, starting_score],
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
        let corrected_indices = match direction {
            DpDirection::Backward => word_indices,
            DpDirection::Forward => [word_indices[0] - 1, word_indices[1] - 1],
        };

        let mut improve = |substate: usize, proposed: TScore, proposed_movement: &Option<(DiffOp, usize)>| {
            if state.scores[substate] < proposed {
                state.scores[substate] = proposed;
                state.next_steps[substate] = *proposed_movement;
            }
        };

        let change_coef = if self.line_splits_at[file_ids[0]][0][word_indices[0]]
            && self.line_splits_at[file_ids[1]][1][word_indices[1]]
        {
            Self::NEWLINE_STATE_CHANGE_COEF
        } else {
            1.0
        };

        if step == DiffOp::Match {
            let syms = [0, 1].map(|side| self.symbols[file_ids[side]][side][corrected_indices[side]]);

            if syms[0] != syms[1] {
                return;
            }
            let score_without_transition =
                state_after_move.scores[Self::MATCH] + self.information_values[file_ids[0]][0][corrected_indices[0]];
            let movement = Some((DiffOp::Match, Self::MATCH));
            let p_stay_match = 1.0 - (Self::P_START_GAP + Self::P_START_WHITE_GAP) * change_coef;
            improve(Self::MATCH, score_without_transition + p_stay_match.log2(), &movement);
            improve(
                Self::GAP,
                score_without_transition + (Self::P_END_GAP * change_coef).log2(),
                &movement,
            );
            improve(
                Self::WHITE_GAP,
                score_without_transition + (Self::P_END_WHITE_GAP * change_coef).log2(),
                &movement,
            );
        } else {
            let score_without_transition = state_after_move.scores[Self::GAP];
            let movement = Some((step, Self::GAP));
            improve(
                Self::MATCH,
                score_without_transition + (Self::P_START_GAP * change_coef).log2(),
                &movement,
            );
            improve(
                Self::GAP,
                score_without_transition + (1.0 - Self::P_END_GAP * change_coef).log2(),
                &movement,
            );
            if step == DiffOp::Insert && self.is_white[file_ids[1]][1][corrected_indices[1]]
                || step == DiffOp::Delete && self.is_white[file_ids[0]][0][corrected_indices[0]]
            {
                let score_without_transition = state_after_move.scores[Self::WHITE_GAP];
                let movement = Some((step, Self::WHITE_GAP));
                improve(
                    Self::MATCH,
                    score_without_transition + (Self::P_START_WHITE_GAP * change_coef).log2(),
                    &movement,
                );
                improve(
                    Self::WHITE_GAP,
                    score_without_transition + (1.0 - Self::P_END_GAP * change_coef).log2(),
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
        let start_score = if starting_substate == Self::MATCH {
            Self::P_START_GAP.log2()
        } else if starting_substate == Self::GAP {
            (1.0 - Self::P_END_GAP).log2()
        } else {
            TScore::NEG_INFINITY
        };
        start_score + (1.0 - Self::P_END_GAP).log2() * gap_length as f64
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

    pub fn is_viable_bound(&self, side: usize, index: usize) -> bool {
        self.scoring
            .is_viable_bound(side, self.slice.global_index(side, index), self.slice.file_ids[side])
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
