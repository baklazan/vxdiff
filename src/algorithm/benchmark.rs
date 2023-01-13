use super::{
    indices::WordIndex,
    main_sequence::{get_aligner, naive_dp},
    preprocess::partition_into_words,
    scoring::{AlignmentSliceScoring, DpDirection, InputSliceBounds},
    MainSequenceAlgorithm, PartitionedText,
};

type AlignmentScoring = super::scoring::affine_scoring::AffineWordScoring;

pub struct PreprocessedTestcase<'a> {
    text_strings: [&'a str; 2],
    word_bounds: [Vec<usize>; 2],
    scoring: AlignmentScoring,
}

impl<'a> PreprocessedTestcase<'a> {
    pub fn new(left: &'a str, right: &'a str) -> PreprocessedTestcase<'a> {
        let text_strings = [left, right];
        let word_bounds = [0, 1].map(|side| partition_into_words(text_strings[side]));

        let text_words = [[0, 1].map(|side| PartitionedText {
            text: text_strings[side],
            part_bounds: &word_bounds[side],
        })];
        let scoring = AlignmentScoring::new(&text_words);

        PreprocessedTestcase {
            word_bounds,
            text_strings,
            scoring,
        }
    }
}

pub fn run_algorithm(input: &PreprocessedTestcase, algorithm: MainSequenceAlgorithm) -> f64 {
    let text_words = [[0, 1].map(|side| PartitionedText {
        text: input.text_strings[side],
        part_bounds: &input.word_bounds[side],
    })];
    let aligner = get_aligner(&text_words, &input.scoring, algorithm);
    let alignment = aligner.align(
        [0, 0],
        [WordIndex::new(0), WordIndex::new(0)],
        [
            WordIndex::new(input.word_bounds[0].len() - 1),
            WordIndex::new(input.word_bounds[1].len() - 1),
        ],
    );
    input.scoring.alignment_score(&alignment, [0, 0]).unwrap()
}

pub fn compute_optimal_score(input: &PreprocessedTestcase) -> f64 {
    let partitioned_texts = [0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        part_bounds: &input.word_bounds[side],
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
