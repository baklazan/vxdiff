use super::{
    main_sequence::{greedy_seeds, naive_dp},
    preprocess::partition_into_words,
    scoring::{AlignmentSliceScoring, DpDirection, InputSliceBounds},
    DiffOp, MainSequenceAlgorithm, PartitionedText,
};

type AlignmentScoring = super::scoring::affine_scoring::AffineWordScoring;

pub struct PreprocessedTestcase<'a> {
    text_strings: [&'a str; 2],
    bounds: [Vec<usize>; 2],
    scoring: AlignmentScoring,
}

impl<'a> PreprocessedTestcase<'a> {
    pub fn new(left: &'a str, right: &'a str) -> PreprocessedTestcase<'a> {
        let text_strings = [left, right];
        let bounds = [0, 1].map(|side| partition_into_words(text_strings[side]));

        let partitioned_texts = [[0, 1].map(|side| PartitionedText {
            text: text_strings[side],
            part_bounds: &bounds[side],
        })];
        let scoring = AlignmentScoring::new(&partitioned_texts);

        PreprocessedTestcase {
            bounds,
            text_strings,
            scoring,
        }
    }
}

type Algorithm = fn(&[[PartitionedText; 2]], &AlignmentScoring) -> Vec<Vec<DiffOp>>;

fn get_algorithm(algorithm: MainSequenceAlgorithm) -> Algorithm {
    match algorithm {
        MainSequenceAlgorithm::Seeds => greedy_seeds::greedy_seeds,
        MainSequenceAlgorithm::Naive => |text_words, scoring| naive_dp::naive_dp_all_files(text_words, scoring),
    }
}

pub fn run_algorithm(input: &PreprocessedTestcase, algorithm: MainSequenceAlgorithm) -> f64 {
    let partitioned_texts = [[0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        part_bounds: &input.bounds[side],
    })];
    let alignment = get_algorithm(algorithm)(&partitioned_texts, &input.scoring).remove(0);
    input.scoring.alignment_score(&alignment, [0, 0]).unwrap()
}

pub fn compute_optimal_score(input: &PreprocessedTestcase) -> f64 {
    let partitioned_texts = [0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        part_bounds: &input.bounds[side],
    });
    let slice = InputSliceBounds {
        file_ids: [0, 0],
        start: [0, 0],
        direction: DpDirection::Forward,
        size: [0, 1].map(|side| partitioned_texts[side].part_count()),
    };
    naive_dp::compute_score(&AlignmentSliceScoring {
        slice,
        scoring: &input.scoring,
    })
}
