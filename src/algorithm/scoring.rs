use super::*;
use string_interner::StringInterner;

pub type TScore = f64;

pub trait ScoreState: Clone {
    const SUBSTATES_COUNT: usize;

    fn substate_scores(&self) -> Vec<TScore>;
    fn best_score(&self) -> TScore;
    fn substate_movements(&self) -> Vec<Option<(DiffOp, usize)>>;
}

pub trait ScoringMethod {
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
pub struct SimpleScoreState {
    next_step: Option<DiffOp>,
    value: TScore,
}

impl ScoreState for SimpleScoreState {
    const SUBSTATES_COUNT: usize = 1;

    fn best_score(&self) -> TScore {
        self.value
    }

    fn substate_scores(&self) -> Vec<TScore> {
        vec![self.value]
    }

    fn substate_movements(&self) -> Vec<Option<(DiffOp, usize)>> {
        let value = match self.next_step {
            None => None,
            Some(op) => Some((op, 0)),
        };
        vec![value]
    }
}

pub struct HistogramScoring {
    symbols: [Vec<string_interner::symbol::SymbolU32>; 2],
    scores: [Vec<TScore>; 2],
}

impl HistogramScoring {
    const P_MATCH: TScore = 0.97;

    fn gap_score() -> TScore {
        (1.0 - HistogramScoring::P_MATCH).log2()
    }

    fn match_score(&self, old_index: usize, new_index: usize) -> TScore {
        if self.symbols[0][old_index] != self.symbols[1][new_index] {
            return TScore::NEG_INFINITY;
        }
        self.scores[0][old_index]
    }

    pub fn new(texts: &[PartitionedText; 2]) -> HistogramScoring {
        use std::collections::HashMap;

        let mut char_frequencies: HashMap<char, usize> = HashMap::new();
        let mut total_chars: usize = 0;

        for side in 0..2 {
            for c in texts[side].text.chars() {
                char_frequencies.insert(c, char_frequencies.get(&c).unwrap_or(&0) + 1);
                total_chars += 1;
            }
        }

        let mut symbols = [vec![], vec![]];
        let mut interner = StringInterner::default();
        let mut scores = [vec![], vec![]];
        for side in 0..2 {
            for i in 0..texts[side].word_count() {
                let word = texts[side].get_word(i);
                symbols[side].push(interner.get_or_intern(word));
                let mut frequency: f64 = 1.0;
                for c in word.chars() {
                    frequency *= (*char_frequencies.get(&c).unwrap() as f64) / total_chars as f64;
                }
                scores[side].push(HistogramScoring::P_MATCH.log2() - frequency.log2());
            }
        }

        HistogramScoring { symbols, scores }
    }
}

impl ScoringMethod for HistogramScoring {
    type State = SimpleScoreState;

    fn starting_state(&self, starting_score: TScore) -> Self::State {
        SimpleScoreState {
            next_step: None,
            value: starting_score,
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
        let proposed_score = state_after_move.value + step_score;
        if proposed_score > state.value {
            state.value = proposed_score;
            state.next_step = Some(step);
        }
    }
}
