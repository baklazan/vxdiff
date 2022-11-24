use crate::algorithm::{PartitionedText, scoring::{TScore, internalize_parts}};

use super::MatchScoring;
pub struct ZeroOneScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
}

impl ZeroOneScoring {
    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]]) -> ZeroOneScoring {
        ZeroOneScoring {
            symbols: internalize_parts(text_parts),
        }
    }
}

impl MatchScoring for ZeroOneScoring {
    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
    }
    
    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        if self.is_match(part_indices, file_ids) { 1.0 } else { 0.0 }
    }
}
