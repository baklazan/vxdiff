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
    let mut text_words = vec![];

    for file in files {
        let bounds = [partition_into_words(file[0]), partition_into_words(file[1])];
        word_bounds.push(bounds);
    }
    for (file_id, file) in files.iter().enumerate() {
        let file_text_words: [PartitionedText; 2] = [
            PartitionedText {
                text: file[0],
                part_bounds: &word_bounds[file_id][0],
            },
            PartitionedText {
                text: file[1],
                part_bounds: &word_bounds[file_id][1],
            },
        ];
        text_words.push(file_text_words);
    }

    let aligned_fragments = compute_fragments(&text_words, algorithm);
    postprocess::build_diff(&text_words, aligned_fragments)
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
    pub part_bounds: &'a [usize],
}

impl<'a> PartitionedText<'a> {
    pub fn part_count(&self) -> usize {
        if self.part_bounds.is_empty() {
            0
        } else {
            self.part_bounds.len() - 1
        }
    }
    pub fn get_part(&self, index: usize) -> &'a str {
        &self.text[self.part_bounds[index]..self.part_bounds[index + 1]]
    }
}

fn get_partitioned_subtext<'a>(text: &PartitionedText<'a>, part_range: Range<usize>) -> PartitionedText<'a> {
    PartitionedText {
        text: text.text,
        part_bounds: &text.part_bounds[part_range.start..=part_range.end],
    }
}

fn compute_fragments(text_words: &[[PartitionedText; 2]], algorithm: DiffAlgorithm) -> Vec<(AlignedFragment, bool)> {
    match algorithm {
        DiffAlgorithm::MovingSeeds => greedy_fragments(text_words),
        DiffAlgorithm::MainSequence(main_sequence_algorithm) => {
            main_sequence_fragments(text_words, main_sequence_algorithm)
        }
    }
}
