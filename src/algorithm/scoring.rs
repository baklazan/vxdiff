use super::*;
use string_interner::StringInterner;

pub type TScore = f64;

pub trait ScoreState: Clone {
    const SUBSTATES_COUNT: usize;

    fn substate_scores(&self) -> &[TScore];
    fn best_score(&self) -> TScore;
    fn substate_movements(&self) -> &[Option<(DiffOp, usize)>];
}

pub trait AlignmentScoringMethod {
    type State: ScoreState;

    fn starting_state(&self, starting_score: TScore) -> Self::State;
    fn consider_step(
        &self,
        old_index: usize,
        new_index: usize,
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
    );
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

fn information_values(texts: &[PartitionedText; 2]) -> [Vec<TScore>; 2] {
    use std::collections::HashMap;

    let mut char_frequencies: HashMap<char, usize> = HashMap::new();
    let mut total_chars: usize = 0;

    for side in 0..2 {
        for c in texts[side].text.chars() {
            char_frequencies.insert(c, char_frequencies.get(&c).unwrap_or(&0) + 1);
            total_chars += 1;
        }
    }
    let mut values = [vec![], vec![]];
    for side in 0..2 {
        for i in 0..texts[side].word_count() {
            let word = texts[side].get_word(i);
            let mut frequency: f64 = 1.0;
            for c in word.chars() {
                frequency *= (*char_frequencies.get(&c).unwrap() as f64) / total_chars as f64;
            }
            values[side].push(-frequency.log2() / 5.0);
        }
    }
    values
}

fn internalize_words(texts: &[PartitionedText; 2]) -> [Vec<string_interner::symbol::SymbolU32>; 2] {
    let mut symbols = [vec![], vec![]];
    let mut interner = StringInterner::default();
    for side in 0..2 {
        for i in 0..texts[side].word_count() {
            let word = texts[side].get_word(i);
            symbols[side].push(interner.get_or_intern(word));
        }
    }
    symbols
}

pub struct SimpleScoring {
    symbols: [Vec<string_interner::symbol::SymbolU32>; 2],
    information_values: [Vec<TScore>; 2],
}

impl SimpleScoring {
    const P_MATCH: TScore = 0.97;

    fn gap_score() -> TScore {
        (1.0 - Self::P_MATCH).log2()
    }

    fn match_score(&self, old_index: usize, new_index: usize) -> TScore {
        if self.symbols[0][old_index] != self.symbols[1][new_index] {
            return TScore::NEG_INFINITY;
        }
        Self::P_MATCH.log2() + self.information_values[0][old_index]
    }

    #[allow(dead_code)]
    pub fn new(texts: &[PartitionedText; 2]) -> SimpleScoring {
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
        old_index: usize,
        new_index: usize,
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
    ) {
        let step_score = if step == DiffOp::Match {
            self.match_score(old_index, new_index)
        } else {
            Self::gap_score()
        };
        let proposed_score = state_after_move.scores[0] + step_score;
        if proposed_score > state.scores[0] {
            state.scores[0] = proposed_score;
            state.next_steps[0] = Some((step, 0));
        }
    }
}

pub struct AffineScoring {
    symbols: [Vec<string_interner::symbol::SymbolU32>; 2],
    information_values: [Vec<TScore>; 2],
    is_white: [Vec<bool>; 2],
    line_splits_before: [Vec<bool>; 2],
    bound_score: [Vec<TScore>; 2],
}

impl AffineScoring {
    const P_END_GAP: f64 = 0.2;
    const P_START_GAP: f64 = 0.01;
    const NEWLINE_STATE_CHANGE_COEF: f64 = 1.1;
    const P_END_WHITE_GAP: f64 = 0.4;
    const P_START_WHITE_GAP: f64 = 0.2;

    const BASE_BOUND_SCORE: TScore = -1.0;
    const LINE_CONTENT_COEF: TScore = -0.5;

    const MATCH: usize = 0;
    const GAP: usize = 1;
    const WHITE_GAP: usize = 2;

    pub fn new(texts: &[PartitionedText; 2]) -> AffineScoring {
        let information_values = information_values(texts);

        let mut is_white = [vec![], vec![]];
        let mut line_splits_before = [vec![true], vec![true]];
        let mut bound_score = [
            vec![TScore::NEG_INFINITY; texts[0].word_count() + 1],
            vec![TScore::NEG_INFINITY; texts[1].word_count() + 1],
        ];
        for side in 0..2 {
            let mut this_line_value = 0.0;
            let mut last_line_value = 0.0;
            let mut last_line_end = 0;
            for i in 0..texts[side].word_count() {
                let word = texts[side].get_word(i);
                let mut white = true;
                for c in word.chars() {
                    if !c.is_whitespace() {
                        white = false;
                    }
                }
                is_white[side].push(white);

                this_line_value += information_values[side][i];
                if word == "\n" || i + 1 >= texts[side].word_count() {
                    bound_score[side][last_line_end] = Self::BASE_BOUND_SCORE
                        + TScore::min(this_line_value, last_line_value) * Self::LINE_CONTENT_COEF;
                    last_line_value = this_line_value;
                    this_line_value = 0.0;
                    last_line_end = i + 1;
                    line_splits_before[side].push(true);
                } else {
                    line_splits_before[side].push(false);
                }
            }
            bound_score[side][last_line_end] = Self::BASE_BOUND_SCORE + last_line_value * Self::LINE_CONTENT_COEF;
        }

        AffineScoring {
            symbols: internalize_words(texts),
            information_values,
            is_white,
            line_splits_before,
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
        old_index: usize,
        new_index: usize,
        state_after_move: Self::State,
        state: &mut Self::State,
        step: DiffOp,
    ) {
        let mut improve = |substate: usize, proposed: TScore, proposed_movement: &Option<(DiffOp, usize)>| {
            if state.scores[substate] < proposed {
                state.scores[substate] = proposed;
                state.next_steps[substate] = *proposed_movement;
            }
        };

        let change_coef = if self.line_splits_before[0][old_index] && self.line_splits_before[1][new_index] {
            Self::NEWLINE_STATE_CHANGE_COEF
        } else {
            1.0
        };

        if step == DiffOp::Match {
            if self.symbols[0][old_index] != self.symbols[1][new_index] {
                return;
            }
            let score_without_transition = state_after_move.scores[Self::MATCH] + self.information_values[0][old_index];
            let movement = Some((DiffOp::Match, Self::MATCH));
            let p_stay_match = 1.0 - (Self::P_START_GAP/*+ Self::P_START_WHITE_GAP*/) * change_coef;
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
            if step == DiffOp::Insert && self.is_white[1][new_index]
                || step == DiffOp::Delete && self.is_white[0][old_index]
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
}

pub trait SupersectionBoundsScoringMethod {
    const SUPERSECTION_THRESHOLD: TScore;
    const MOVED_SUPERSECTION_THRESHOLD: TScore;

    fn supersection_bound_penalty(&self, old_index: usize, new_index: usize) -> TScore;
}

impl SupersectionBoundsScoringMethod for AffineScoring {
    const SUPERSECTION_THRESHOLD: TScore = 5.0;
    const MOVED_SUPERSECTION_THRESHOLD: TScore = 10.0;

    fn supersection_bound_penalty(&self, old_index: usize, new_index: usize) -> TScore {
        self.bound_score[0][old_index] + self.bound_score[1][new_index]
    }
}
