use super::*;
use string_interner::StringInterner;

pub type TScore = f64;
pub mod affine_scoring;

#[derive(Clone, Default)]
pub struct DpSubstate {
    pub score: TScore,
    pub previous_step: Option<(DiffOp, usize)>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DpDirection {
    Backward,
    Forward,
}

pub trait AlignmentScoringMethod {
    fn substates_count(&self) -> usize;

    fn set_starting_state(&self, starting_score: TScore, state: &mut [DpSubstate]);

    fn consider_step(
        &self,
        word_indices: [usize; 2],
        file_ids: [usize; 2],
        state_before_step: &[DpSubstate],
        state: &mut [DpSubstate],
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

    fn substate_score(
        &self,
        state: &[DpSubstate],
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

pub struct AlignmentSliceScoring<'a> {
    pub slice: InputSliceBounds,
    pub scoring: &'a dyn AlignmentScoringMethod,
}

impl<'a> AlignmentSliceScoring<'a> {
    pub fn substates_count(&self) -> usize {
        self.scoring.substates_count()
    }

    pub fn set_starting_state(&self, starting_score: TScore, state: &mut [DpSubstate]) {
        self.scoring.set_starting_state(starting_score, state);
    }

    pub fn consider_step(
        &self,
        word_indices: [usize; 2],
        state_after_move: &[DpSubstate],
        state: &mut [DpSubstate],
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

    pub fn substate_score(&self, state: &[DpSubstate], substate: usize, position: [usize; 2]) -> TScore {
        self.scoring.substate_score(
            state,
            substate,
            self.slice.file_ids,
            self.slice.global_indices(position),
            self.slice.direction,
        )
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

pub trait FragmentBoundsScoringMethod {
    const SUPERSECTION_THRESHOLD: TScore;
    const MOVED_SUPERSECTION_THRESHOLD: TScore;

    fn fragment_bound_penalty(&self, word_indices: [usize; 2], file_ids: [usize; 2]) -> TScore;
    fn is_viable_bound(&self, side: usize, index: usize, file_id: usize) -> bool;
    fn nearest_bound(&self, side: usize, index: usize, file_id: usize, direction: DpDirection) -> usize;
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
