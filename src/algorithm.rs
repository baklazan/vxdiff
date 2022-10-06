mod dynamic_programming;
mod fragment_selection;
mod postprocess;
mod preprocess;
mod scoring;
mod seed_selection;

use self::{fragment_selection::greedy_fragments, preprocess::partition_into_words};

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
    pub highlight_bounds: Vec<usize>,
    pub highlight_first: bool,
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

pub fn compute_diff(files: &[[&str; 2]]) -> Diff {
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

    let aligned_fragments = greedy_fragments(&texts);
    postprocess::build_diff(&texts, aligned_fragments)
}

pub struct AlignedFragment {
    starts: [usize; 2],
    ends: [usize; 2],
    file_ids: [usize; 2],
    alignment: Vec<DiffOp>,
}

#[derive(Default)]
pub struct PartitionedText<'a> {
    pub text: &'a str,
    pub word_bounds: &'a [usize],
}

impl<'a> PartitionedText<'a> {
    fn word_count(&self) -> usize {
        self.word_bounds.len() - 1
    }
    fn get_word(&self, index: usize) -> &'a str {
        &self.text[self.word_bounds[index]..self.word_bounds[index + 1]]
    }
}
