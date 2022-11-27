use crate::algorithm::{
    preprocess::{internalize_parts, part_values, CharFrequencyCounter},
    scoring::TScore,
    PartitionedText,
};

use super::MatchScoring;

pub struct WhitespaceAwareScoring {
    full_symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    non_white_symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    full_values: Vec<[Vec<TScore>; 2]>,
    non_white_values: Vec<[Vec<TScore>; 2]>,
}

struct FilteredInput {
    texts: Vec<[String; 2]>,
    part_bounds: Vec<[Vec<usize>; 2]>,
}

fn remove_whitespace(text_parts: &[[PartitionedText; 2]]) -> FilteredInput {
    let mut texts = vec![];
    let mut part_bounds = vec![];
    for file_text_parts in text_parts {
        let mut file_texts = [String::new(), String::new()];
        let mut file_part_bounds = [vec![], vec![]];
        for side in 0..2 {
            let mut bound_index = 0;
            let my_partitioned = &file_text_parts[side];
            for (i, c) in my_partitioned.text.char_indices() {
                if i == my_partitioned.part_bounds[bound_index] {
                    file_part_bounds[side].push(file_texts[side].len());
                    bound_index += 1;
                }
                if !c.is_whitespace() {
                    file_texts[side].push(c);
                }
            }
            file_part_bounds[side].push(file_texts[side].len());
        }
        texts.push(file_texts);
        part_bounds.push(file_part_bounds);
    }
    FilteredInput { texts, part_bounds }
}

impl WhitespaceAwareScoring {
    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]]) -> WhitespaceAwareScoring {
        let non_white_input = remove_whitespace(text_parts);
        let mut non_white_parts = vec![];
        for file_id in 0..text_parts.len() {
            non_white_parts.push([0, 1].map(|side| PartitionedText {
                text: &non_white_input.texts[file_id][side],
                part_bounds: &non_white_input.part_bounds[file_id][side],
            }));
        }

        let frequencies = CharFrequencyCounter::collect_from_texts(text_parts);

        WhitespaceAwareScoring {
            full_symbols: internalize_parts(text_parts),
            non_white_symbols: internalize_parts(&non_white_parts),
            full_values: part_values(text_parts, &frequencies),
            non_white_values: part_values(&non_white_parts, &frequencies),
        }
    }
}

impl MatchScoring for WhitespaceAwareScoring {
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.full_symbols[file_ids[0]][0][part_indices[0]] == self.full_symbols[file_ids[1]][1][part_indices[1]]
    }

    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        if self.is_match(part_indices, file_ids) {
            self.full_values[file_ids[0]][0][part_indices[0]]
        } else {
            let non_white_match = self.non_white_symbols[file_ids[0]][0][part_indices[0]]
                == self.non_white_symbols[file_ids[1]][1][part_indices[1]];
            if non_white_match {
                self.non_white_values[file_ids[0]][0][part_indices[0]]
            } else {
                0.0
            }
        }
    }
}
