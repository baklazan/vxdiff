use super::{
    indices::{IndexConverter, WordIndex},
    main_sequence::{get_aligner, naive_dp},
    preprocess::{partition_into_lines, partition_into_words},
    scoring::{line_bounds_scoring::LineBoundsScoring, AlignmentScorer, InputSliceBounds, SliceAlignmentPrioritizer},
    MainSequenceAlgorithm, PartitionedText,
};

pub struct PreprocessedTestcase<'a> {
    text_strings: [&'a str; 2],
    word_bounds: [Vec<usize>; 2],
    index_converters: [[IndexConverter; 2]; 1],
    line_bounds_scoring: LineBoundsScoring,
}

impl<'a> PreprocessedTestcase<'a> {
    pub fn new(left: &'a str, right: &'a str) -> PreprocessedTestcase<'a> {
        let text_strings = [left, right];
        let word_bounds = [0, 1].map(|side| partition_into_words(text_strings[side]));
        let line_bounds = [0, 1].map(|side| partition_into_lines(text_strings[side]));

        let index_converters = [[0, 1].map(|side| IndexConverter::new(&word_bounds[side], &line_bounds[side]))];

        let text_lines = [[0, 1].map(|side| PartitionedText {
            text: text_strings[side],
            part_bounds: &line_bounds[side],
        })];

        let line_bounds_scoring = LineBoundsScoring::new(&text_lines);
        PreprocessedTestcase {
            word_bounds,
            text_strings,
            index_converters,
            line_bounds_scoring,
        }
    }
}

pub type AlignmentScoring<'a> = super::scoring::multiline_gaps_scoring::MultilineGapsScoring<'a>;

pub fn make_scorer<'a>(input: &'a PreprocessedTestcase) -> AlignmentScoring<'a> {
    let text_words = [[0, 1].map(|side| PartitionedText {
        text: input.text_strings[side],
        part_bounds: &input.word_bounds[side],
    })];
    AlignmentScoring::new(&text_words, &input.index_converters, &input.line_bounds_scoring)
}

pub fn run_algorithm(
    input: &PreprocessedTestcase,
    scoring: &AlignmentScoring,
    algorithm: MainSequenceAlgorithm,
) -> f64 {
    let text_words = [[0, 1].map(|side| PartitionedText {
        text: input.text_strings[side],
        part_bounds: &input.word_bounds[side],
    })];

    let aligner = get_aligner(&text_words, scoring, &input.line_bounds_scoring, algorithm);

    let start = [0, 0];
    let file_ids = [0, 0];
    let end = [0, 1].map(|side| input.word_bounds[side].len() - 1);
    let alignment = aligner.align(file_ids, start.map(WordIndex::new), end.map(WordIndex::new));
    scoring.score_alignment(&alignment, start, end, file_ids).unwrap()
}

pub fn compute_optimal_score(input: &PreprocessedTestcase, scoring: &AlignmentScoring) -> f64 {
    let partitioned_texts = [0, 1].map(|side| PartitionedText {
        text: &input.text_strings[side],
        part_bounds: &input.word_bounds[side],
    });
    let slice = InputSliceBounds {
        file_ids: [0, 0],
        start: [0, 0],
        size: [0, 1].map(|side| partitioned_texts[side].part_count()),
    };
    naive_dp::compute_score(&SliceAlignmentPrioritizer { slice, scoring })
}
