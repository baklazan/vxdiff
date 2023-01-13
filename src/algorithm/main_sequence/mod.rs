use super::{
    indices::WordIndex,
    scoring::{affine_scoring::AffineWordScoring, AlignmentScoringMethod},
    AlignedFragment, DiffOp, MainSequenceAlgorithm, PartitionedText,
};

pub mod multi_level;
pub mod naive_dp;

pub(super) trait Aligner {
    fn align(&self, file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> Vec<DiffOp>;
    /*fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore>;
    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore>;*/
}

pub(super) fn get_aligner<'a>(
    text_words: &[[PartitionedText; 2]],
    word_scoring: &'a dyn AlignmentScoringMethod,
    algorithm: MainSequenceAlgorithm,
) -> Box<dyn Aligner + 'a> {
    match algorithm {
        MainSequenceAlgorithm::Naive => Box::new(naive_dp::NaiveAligner::new(word_scoring)),
        MainSequenceAlgorithm::LinesThenWords(line_scoring_algorithm) => Box::new(
            multi_level::lines_then_words_aligner(text_words, word_scoring, line_scoring_algorithm),
        ),
    }
}

pub(super) fn main_sequence_fragments(
    text_words: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let scoring = AffineWordScoring::new(text_words);

    let aligner = get_aligner(text_words, &scoring, algorithm);

    let mut alignments = vec![];
    for file_id in 0..text_words.len() {
        alignments.push(aligner.align(
            [file_id, file_id],
            [WordIndex::new(0), WordIndex::new(0)],
            [
                WordIndex::new(text_words[file_id][0].part_count()),
                WordIndex::new(text_words[file_id][1].part_count()),
            ],
        ));
    }

    let mut result: Vec<(AlignedFragment, bool)> = vec![];
    for (file_id, file_texts) in text_words.iter().enumerate() {
        result.push((
            AlignedFragment {
                starts: [0, 0],
                ends: [0, 1].map(|side| file_texts[side].part_count()),
                alignment: std::mem::take(&mut alignments[file_id]),
                file_ids: [file_id, file_id],
            },
            true,
        ));
    }
    result
}
