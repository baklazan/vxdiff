pub mod benchmark;
mod dp_substate_vec;
mod indices;
mod main_sequence;
mod moved_detection;
mod postprocess;
mod preprocess;
mod scoring;
mod suffix_array;

use self::{
    main_sequence::main_sequence_fragments,
    moved_detection::main_then_moved,
    preprocess::{partition_into_lines, partition_into_words},
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
    MainThenMoved(MainSequenceAlgorithm),
}

pub enum MainSequenceAlgorithm {
    Naive,
    LinesThenWords(LineScoringStrategy),
}

#[derive(Clone, Copy)]
pub enum LineScoringStrategy {
    ZeroOne,
    ZeroInformation,
    WhitespaceIgnoring,
    KGram,
}

pub fn compute_diff_with_hints(files: &[[&str; 2]], hints: &[Vec<[usize; 2]>], algorithm: DiffAlgorithm) -> Diff {
    assert_eq!(files.len(), hints.len());

    let mut split_files = vec![];
    for (file, file_hints) in files.iter().zip(hints.iter()) {
        let mut last = [0, 0];
        let end = [file[0].len(), file[1].len()];
        for hint in file_hints.iter().copied().chain(std::iter::once(end)) {
            assert!(last[0] <= hint[0]);
            assert!(last[1] <= hint[1]);
            assert!(hint != last);

            split_files.push([0, 1].map(|side| &file[side][last[side]..hint[side]]));
            last = hint;
        }
    }

    let split_diff = compute_diff(&split_files, algorithm);

    let mut joined_diff = Diff {
        sections: split_diff.sections,
        files: vec![],
    };

    let mut split_index = 0;
    for file_hints in hints {
        let mut ops = vec![];
        for last in std::iter::once([0, 0]).chain(file_hints.iter().copied()) {
            for &(op, section_id) in &split_diff.files[split_index].ops {
                ops.push((op, section_id));
                for side in 0..2 {
                    if op.movement()[side] != 0 {
                        let section_side = &mut joined_diff.sections[section_id].sides[side];
                        let adjust = |range: &mut Range<usize>| {
                            range.start += last[side];
                            range.end += last[side];
                        };
                        adjust(&mut section_side.byte_range);
                        for highlight_range in section_side.highlight_ranges.iter_mut() {
                            adjust(highlight_range);
                        }
                    }
                }
            }
            split_index += 1;
        }
        joined_diff.files.push(FileDiff { ops });
    }

    joined_diff
}

pub fn compute_diff(files: &[[&str; 2]], algorithm: DiffAlgorithm) -> Diff {
    let word_bounds: Vec<[Vec<usize>; 2]> = files.iter().map(|file| file.map(partition_into_words)).collect();
    let line_bounds: Vec<[Vec<usize>; 2]> = files.iter().map(|file| file.map(partition_into_lines)).collect();

    fn wrap_in_partitioned_text<'a>(
        bounds: &'a [[Vec<usize>; 2]],
        files: &'a [[&str; 2]],
    ) -> Vec<[PartitionedText<'a>; 2]> {
        let mut result = vec![];
        for (file_id, file) in files.iter().enumerate() {
            let file_parts: [PartitionedText; 2] = [
                PartitionedText {
                    text: file[0],
                    part_bounds: &bounds[file_id][0],
                },
                PartitionedText {
                    text: file[1],
                    part_bounds: &bounds[file_id][1],
                },
            ];
            result.push(file_parts);
        }
        result
    }

    let text_words = wrap_in_partitioned_text(&word_bounds, files);
    let text_lines = wrap_in_partitioned_text(&line_bounds, files);

    let aligned_fragments = compute_fragments(&text_words, &text_lines, algorithm);
    postprocess::build_diff(&text_words, aligned_fragments)
}

struct AlignedFragment {
    // TODO: refactor, use LineIndex (instead of word index stored in usize)
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

fn compute_fragments(
    text_words: &[[PartitionedText; 2]],
    text_lines: &[[PartitionedText; 2]],
    algorithm: DiffAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    match algorithm {
        DiffAlgorithm::MainThenMoved(main_sequence_algorithm) => {
            main_then_moved(text_words, text_lines, main_sequence_algorithm)
        }
        DiffAlgorithm::MainSequence(main_sequence_algorithm) => {
            main_sequence_fragments(text_words, main_sequence_algorithm)
        }
    }
}
