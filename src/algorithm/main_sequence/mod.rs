use super::{
    scoring::{affine_scoring::AffineWordScoring, DpDirection, InputSliceBounds},
    AlignedFragment, MainSequenceAlgorithm, PartitionedText,
};

pub mod greedy_seeds;
pub mod multi_level;
pub mod naive_dp;

fn slices_for_files(text_words: &[[PartitionedText; 2]]) -> Vec<InputSliceBounds> {
    let mut result = vec![];
    for (file_id, file_text_words) in text_words.iter().enumerate() {
        result.push(InputSliceBounds {
            start: [0, 0],
            file_ids: [file_id, file_id],
            direction: DpDirection::Forward,
            size: [0, 1].map(|side| file_text_words[side].part_count()),
        });
    }
    result
}

pub(super) fn main_sequence_fragments(
    text_words: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let scoring = AffineWordScoring::new(text_words);

    let mut alignments = match algorithm {
        MainSequenceAlgorithm::Naive => naive_dp::naive_dp_all_files(text_words, &scoring),
        MainSequenceAlgorithm::Seeds => greedy_seeds::greedy_seeds(text_words, &scoring),
        MainSequenceAlgorithm::LinesThenWords(line_scoring) => {
            multi_level::lines_then_words(text_words, &scoring, line_scoring)
        }
    };

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
