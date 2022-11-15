use super::{
    main_sequence::{greedy_seeds, naive_dp},
    preprocess::partition_into_words,
    scoring::{AlignmentSliceScoring, DpDirection, InputSliceBounds},
    DiffOp, MainSequenceAlgorithm, PartitionedText,
};

type AlignmentScoring = super::scoring::AffineScoring;

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
            word_bounds: &bounds[side],
        })];
        let scoring = AlignmentScoring::new(&partitioned_texts);

        PreprocessedTestcase {
            bounds,
            text_strings,
            scoring,
        }
    }
}

type Algorithm = fn(&[PartitionedText; 2], &AlignmentScoring) -> Vec<DiffOp>;

fn get_algorithm(algorithm: MainSequenceAlgorithm) -> Algorithm {
    match algorithm {
        MainSequenceAlgorithm::Seeds => |texts, scoring| greedy_seeds::greedy_seeds(texts, scoring, [0, 0]),
        MainSequenceAlgorithm::Naive => |texts, scoring| {
            let sizes = [0, 1].map(|side| texts[side].word_count());
            let slice = InputSliceBounds {
                file_ids: [0, 0],
                start: [0, 0],
                direction: DpDirection::Forward,
            };
            naive_dp::naive_dp(&AlignmentSliceScoring { slice, scoring }, sizes)
        },
    }
}

pub fn run_algorithm(input: &PreprocessedTestcase, algorithm: MainSequenceAlgorithm) -> f64 {
    let partitioned_texts = [0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        word_bounds: &input.bounds[side],
    });
    let alignment = get_algorithm(algorithm)(&partitioned_texts, &input.scoring);
    input.scoring.alignment_score(&alignment, [0, 0]).unwrap()
}

pub fn compute_optimal_score(input: &PreprocessedTestcase) -> f64 {
    let partitioned_texts = [0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        word_bounds: &input.bounds[side],
    });
    let sizes = [0, 1].map(|side| partitioned_texts[side].word_count());
    let slice = InputSliceBounds {
        file_ids: [0, 0],
        start: [0, 0],
        direction: DpDirection::Forward,
    };
    naive_dp::compute_score(
        &AlignmentSliceScoring {
            slice,
            scoring: &input.scoring,
        },
        sizes,
    )
}