mod dynamic_programming;
mod fragment_selection;
mod postprocess;
mod preprocess;
mod scoring;
mod seed_selection;

use self::{fragment_selection::greedy_fragments, preprocess::partition_into_words};
use std::convert::identity;

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

pub fn diff_file(old: &str, new: &str) -> Diff {
    let word_bounds = [partition_into_words(old), partition_into_words(new)];
    let texts = [
        PartitionedText {
            text: old,
            word_bounds: &word_bounds[0],
        },
        PartitionedText {
            text: new,
            word_bounds: &word_bounds[1],
        },
    ];

    let aligned_fragments = greedy_fragments(&texts);
    postprocess::build_diff(&texts, aligned_fragments)
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

pub fn merge_diffs(one: &mut Diff, two: Diff) {
    let old_section_count = one.sections.len();
    one.sections.append(&mut identity(two.sections));
    for FileDiff { ops } in two.files {
        let map_op = |(op, section_id)| (op, section_id + old_section_count);
        let mapped_ops = ops.into_iter().map(map_op).collect();
        one.files.push(FileDiff { ops: mapped_ops });
    }
}

pub struct AlignedFragment {
    starts: [usize; 2],
    ends: [usize; 2],
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
