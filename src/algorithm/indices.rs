use std::ops::Range;

use index_vec::{index_vec, IndexVec};

pub(super) trait UsizeConvertible {
    fn from_usize(val: usize) -> Self;
    fn to_usize(&self) -> usize;
}

macro_rules! extend_index_type {
    (
        $type:ident
    ) => {
        impl UsizeConvertible for $type {
            fn from_usize(val: usize) -> Self {
                Self::new(val)
            }

            fn to_usize(&self) -> usize {
                self.raw()
            }
        }

        #[allow(dead_code)]
        impl $type {
            pub fn saturating_sub(&self, other: Self) -> Self {
                Self::new(self.raw().saturating_sub(other.raw()))
            }
        }
    };
}

index_vec::define_index_type! {
    pub(super) struct LineIndex = usize;
}
extend_index_type!(LineIndex);

index_vec::define_index_type! {
    pub(super) struct WordIndex = usize;
}
extend_index_type!(WordIndex);

pub(super) fn range_iter<Index: UsizeConvertible>(range: Range<Index>) -> impl Iterator<Item = Index> {
    (range.start.to_usize()..range.end.to_usize()).map(Index::from_usize)
}

pub(super) struct IndexConverter {
    line_to_word: IndexVec<LineIndex, WordIndex>,
    word_to_line_before: IndexVec<WordIndex, LineIndex>,
}

impl IndexConverter {
    pub fn new(word_bounds: &[usize], line_bounds: &[usize]) -> Self {
        let mut line_to_word = index_vec![WordIndex::new(0)];
        let mut word_to_line_before = index_vec![];
        let mut current_line_index = LineIndex::new(0);
        for (word_index, &word_bound) in word_bounds.iter().enumerate() {
            if current_line_index + 1 < line_bounds.len() && line_bounds[current_line_index.raw() + 1] == word_bound {
                current_line_index += 1;
                line_to_word.push(WordIndex::new(word_index));
            }
            word_to_line_before.push(current_line_index);
        }

        IndexConverter {
            line_to_word,
            word_to_line_before,
        }
    }

    pub fn line_to_word(&self, line_index: LineIndex) -> WordIndex {
        self.line_to_word[line_index]
    }

    pub fn word_to_line_before(&self, word_index: WordIndex) -> LineIndex {
        self.word_to_line_before[word_index]
    }

    pub fn word_to_line_after(&self, word_index: WordIndex) -> LineIndex {
        let line_before = self.word_to_line_before[word_index];
        if self.line_to_word[line_before] == word_index {
            line_before
        } else {
            line_before + 1
        }
    }

    pub fn lines_count(&self) -> LineIndex {
        self.line_to_word.last_idx()
    }

    pub fn words_count(&self) -> WordIndex {
        self.word_to_line_before.last_idx()
    }
}

#[cfg(test)]
mod test {
    use super::IndexConverter;
    use super::LineIndex;
    use super::WordIndex;

    #[test]
    fn line_to_word() {
        let word_bounds = [100, 103, 105, 107, 108, 110, 200];
        let line_bounds = [100, 105, 110, 200];
        let converter = IndexConverter::new(&word_bounds, &line_bounds);
        assert_eq!(converter.line_to_word(LineIndex::new(0)), 0);
        assert_eq!(converter.line_to_word(LineIndex::new(1)), 2);
        assert_eq!(converter.line_to_word(LineIndex::new(2)), 5);
        assert_eq!(converter.line_to_word(LineIndex::new(3)), 6);
    }

    #[test]
    fn word_to_line_before() {
        let word_bounds = [100, 103, 105, 107, 108, 110, 200];
        let line_bounds = [100, 105, 110, 200];
        let converter = IndexConverter::new(&word_bounds, &line_bounds);
        assert_eq!(converter.word_to_line_before(WordIndex::new(0)), 0);
        assert_eq!(converter.word_to_line_before(WordIndex::new(1)), 0);
        assert_eq!(converter.word_to_line_before(WordIndex::new(2)), 1);
        assert_eq!(converter.word_to_line_before(WordIndex::new(4)), 1);
        assert_eq!(converter.word_to_line_before(WordIndex::new(5)), 2);
        assert_eq!(converter.word_to_line_before(WordIndex::new(6)), 3);
    }

    #[test]
    fn word_to_line_after() {
        let word_bounds = [100, 103, 105, 107, 108, 110, 200];
        let line_bounds = [100, 105, 110, 200];
        let converter = IndexConverter::new(&word_bounds, &line_bounds);
        assert_eq!(converter.word_to_line_after(WordIndex::new(0)), 0);
        assert_eq!(converter.word_to_line_after(WordIndex::new(1)), 1);
        assert_eq!(converter.word_to_line_after(WordIndex::new(2)), 1);
        assert_eq!(converter.word_to_line_after(WordIndex::new(3)), 2);
        assert_eq!(converter.word_to_line_after(WordIndex::new(5)), 2);
        assert_eq!(converter.word_to_line_after(WordIndex::new(6)), 3);
    }
}
