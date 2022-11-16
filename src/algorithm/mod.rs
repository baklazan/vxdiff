pub mod benchmark;
mod dp_substate_vec;
mod dynamic_programming;
mod fragment_selection;
mod main_sequence;
mod postprocess;
mod preprocess;
mod scoring;
mod seed_selection;

use self::{
    fragment_selection::greedy_fragments, main_sequence::main_sequence_fragments, preprocess::partition_into_words,
};
use std::ops::Range;

#[derive(Debug, Default)]
pub struct Diff {
    pub sections: Vec<Section>,
    pub files: Vec<FileDiff>,
}

#[derive(Debug, PartialEq)]
pub struct FileDiff {
    pub ops: Vec<(DiffOp, usize)>,
}

#[derive(Debug, PartialEq)]
pub struct Section {
    pub sides: [SectionSide; 2],
    pub equal: bool,
}

#[derive(Debug, PartialEq)]
pub struct SectionSide {
    pub byte_range: Range<usize>,
    pub highlight_ranges: Vec<Range<usize>>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DiffOp {
    Match,
    Insert,
    Delete,
}

impl DiffOp {
    pub fn movement(&self) -> [usize; 2] {
        match self {
            DiffOp::Delete => [1, 0],
            DiffOp::Insert => [0, 1],
            DiffOp::Match => [1, 1],
        }
    }
}

pub enum DiffAlgorithm {
    MainSequence(MainSequenceAlgorithm),
    MovingSeeds,
}

pub enum MainSequenceAlgorithm {
    Naive,
    Seeds,
}

pub fn compute_diff(files: &[[&str; 2]], algorithm: DiffAlgorithm) -> Diff {
    let mut word_bounds = vec![];
    let mut texts = vec![];

    for file in files {
        let bounds = [partition_into_words(file[0]), partition_into_words(file[1])];
        word_bounds.push(bounds);
    }
    for (file_id, file) in files.iter().enumerate() {
        let file_texts: [PartitionedText; 2] = [
            PartitionedText {
                text: file[0],
                word_bounds: &word_bounds[file_id][0],
            },
            PartitionedText {
                text: file[1],
                word_bounds: &word_bounds[file_id][1],
            },
        ];
        texts.push(file_texts);
    }

    let aligned_fragments = compute_fragments(&texts, algorithm);
    postprocess::build_diff(&texts, aligned_fragments)
}

struct AlignedFragment {
    starts: [usize; 2],
    ends: [usize; 2],
    file_ids: [usize; 2],
    alignment: Vec<DiffOp>,
}

#[derive(Default, Clone)]
struct PartitionedText<'a> {
    pub text: &'a str,
    pub word_bounds: &'a [usize],
}

impl<'a> PartitionedText<'a> {
    pub fn word_count(&self) -> usize {
        if self.word_bounds.is_empty() {
            0
        } else {
            self.word_bounds.len() - 1
        }
    }
    pub fn get_word(&self, index: usize) -> &'a str {
        &self.text[self.word_bounds[index]..self.word_bounds[index + 1]]
    }
}

fn get_partitioned_subtext<'a>(text: &PartitionedText<'a>, word_range: Range<usize>) -> PartitionedText<'a> {
    PartitionedText {
        text: text.text,
        word_bounds: &text.word_bounds[word_range.start..=word_range.end],
    }
}

fn compute_fragments(texts: &[[PartitionedText; 2]], algorithm: DiffAlgorithm) -> Vec<(AlignedFragment, bool)> {
    match algorithm {
        DiffAlgorithm::MovingSeeds => greedy_fragments(texts),
        DiffAlgorithm::MainSequence(main_sequence_algorithm) => main_sequence_fragments(texts, main_sequence_algorithm),
    }
}
