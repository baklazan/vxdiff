use std::collections::HashMap;

use string_interner::StringInterner;

use super::{scoring::TScore, PartitionedText};

pub fn partition_into_words(text: &str) -> Vec<usize> {
    let mut word_bounds = vec![];
    let mut was_last_alphabetic = false;
    let mut was_last_numeric = false;
    for (i, c) in text.char_indices() {
        if c.is_alphabetic() && was_last_alphabetic {
            continue;
        }
        if c.is_numeric() && was_last_numeric {
            continue;
        }
        was_last_alphabetic = c.is_alphabetic();
        was_last_numeric = c.is_numeric();
        word_bounds.push(i);
    }
    word_bounds.push(text.len());
    word_bounds
}

pub fn partition_into_lines(text: &str) -> Vec<usize> {
    let mut line_bounds: Vec<usize> = vec![0];
    for (i, c) in text.char_indices() {
        if c == '\n' {
            line_bounds.push(i + 1);
        }
    }
    if *line_bounds.last().unwrap() < text.len() {
        line_bounds.push(text.len());
    }
    line_bounds
}

pub(super) struct CharScorer {
    frequencies: HashMap<char, usize>,
    total: usize,
}

impl CharScorer {
    const WHITESPACE_INFORMATION_FACTOR: TScore = 0.01;

    pub fn from_texts(text_parts: &[[PartitionedText; 2]]) -> CharScorer {
        let mut result = CharScorer {
            frequencies: HashMap::new(),
            total: 0,
        };

        for file_text_parts in text_parts {
            for side_text in file_text_parts {
                for c in side_text.text.chars() {
                    result
                        .frequencies
                        .insert(c, result.frequencies.get(&c).unwrap_or(&0) + 1);
                    result.total += 1;
                }
            }
        }
        result
    }

    pub fn score(&self, c: char) -> TScore {
        let frequency = *self.frequencies.get(&c).unwrap() as f64 / self.total as f64;
        let score = -frequency.log2() / 5.0;
        if c.is_whitespace() {
            score * Self::WHITESPACE_INFORMATION_FACTOR
        } else {
            score
        }
    }
}

pub(super) fn part_values(text_parts: &[[PartitionedText; 2]], char_scorer: &CharScorer) -> Vec<[Vec<TScore>; 2]> {
    let mut result = vec![];
    for file_text_parts in text_parts {
        let mut file_values = [vec![], vec![]];
        for (side, side_text) in file_text_parts.iter().enumerate() {
            for i in 0..side_text.part_count() {
                let word = side_text.get_part(i);
                let mut score: f64 = 0.0;
                for c in word.chars() {
                    score += char_scorer.score(c);
                }
                file_values[side].push(score);
            }
        }
        result.push(file_values);
    }
    result
}

pub(super) fn information_values(text_parts: &[[PartitionedText; 2]]) -> Vec<[Vec<TScore>; 2]> {
    let char_scorer = CharScorer::from_texts(text_parts);
    part_values(text_parts, &char_scorer)
}

pub(super) fn internalize_parts(
    text_parts: &[[PartitionedText; 2]],
) -> Vec<[Vec<string_interner::symbol::SymbolU32>; 2]> {
    let mut symbols = vec![[vec![], vec![]]; text_parts.len()];
    let mut interner = StringInterner::default();
    for (file_id, file_text_parts) in text_parts.iter().enumerate() {
        for (side, side_text) in file_text_parts.iter().enumerate() {
            for i in 0..side_text.part_count() {
                let word = side_text.get_part(i);
                symbols[file_id][side].push(interner.get_or_intern(word));
            }
        }
    }
    symbols
}
