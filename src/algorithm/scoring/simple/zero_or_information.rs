use super::MatchScoring;
use crate::algorithm::{
    scoring::{information_values, internalize_parts, TScore},
    PartitionedText,
};

pub struct ZeroOrInformationScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    information_values: Vec<[Vec<TScore>; 2]>,
}

impl ZeroOrInformationScoring {
    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]]) -> ZeroOrInformationScoring {
        ZeroOrInformationScoring {
            symbols: internalize_parts(text_parts),
            information_values: information_values(text_parts),
        }
    }
}

impl MatchScoring for ZeroOrInformationScoring {
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
    }

    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        if self.is_match(part_indices, file_ids) {
            self.information_values[file_ids[0]][0][part_indices[0]]
        } else {
            0.0
        }
    }
}
