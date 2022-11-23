use super::{
    scoring::{affine_scoring::AffineWordScoring, DpDirection, InputSliceBounds},
    AlignedFragment, MainSequenceAlgorithm, PartitionedText,
};

pub mod greedy_seeds;
pub mod naive_dp;

fn slices_for_files(texts: &[[PartitionedText; 2]]) -> Vec<InputSliceBounds> {
    let mut result = vec![];
    for (file_id, file_texts) in texts.iter().enumerate() {
        result.push(InputSliceBounds {
            start: [0, 0],
            file_ids: [file_id, file_id],
            direction: DpDirection::Forward,
            size: [0, 1].map(|side| file_texts[side].word_count()),
        });
    }
    result
}

pub(super) fn main_sequence_fragments(
    texts: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let scoring = AffineWordScoring::new(texts);

    let mut alignments = match algorithm {
        MainSequenceAlgorithm::Naive => naive_dp::naive_dp_all_files(texts, &scoring),
        MainSequenceAlgorithm::Seeds => greedy_seeds::greedy_seeds(texts, &scoring),
    };

    let mut result: Vec<(AlignedFragment, bool)> = vec![];
    for (file_id, file_texts) in texts.iter().enumerate() {
        result.push((
            AlignedFragment {
                starts: [0, 0],
                ends: [0, 1].map(|side| file_texts[side].word_count()),
                alignment: std::mem::take(&mut alignments[file_id]),
                file_ids: [file_id, file_id],
            },
            true,
        ));
    }
    result
}
