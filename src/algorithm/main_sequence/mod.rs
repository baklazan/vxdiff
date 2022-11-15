use super::{
    scoring::{AffineScoring, AlignmentSliceScoring, DpDirection, InputSliceBounds},
    AlignedFragment, MainSequenceAlgorithm, PartitionedText,
};

pub mod greedy_seeds;
pub mod naive_dp;

pub(super) fn main_sequence_fragments(
    texts: &[[PartitionedText; 2]],
    algorithm: MainSequenceAlgorithm,
) -> Vec<(AlignedFragment, bool)> {
    let scoring = AffineScoring::new(texts);
    let mut result: Vec<(AlignedFragment, bool)> = vec![];
    for (file_id, file_texts) in texts.iter().enumerate() {
        let slice = InputSliceBounds {
            start: [0, 0],
            file_ids: [file_id, file_id],
            direction: DpDirection::Forward,
        };
        let slice_scoring = AlignmentSliceScoring {
            slice,
            scoring: &scoring,
        };
        let sizes = [0, 1].map(|side| file_texts[side].word_count());
        let alignment = match algorithm {
            MainSequenceAlgorithm::Naive => naive_dp::naive_dp(&slice_scoring, sizes),
            MainSequenceAlgorithm::Seeds => greedy_seeds::greedy_seeds(file_texts, &scoring, [file_id, file_id]),
        };
        result.push((
            AlignedFragment {
                starts: [0, 0],
                ends: sizes,
                alignment,
                file_ids: [file_id, file_id],
            },
            true,
        ));
    }
    result
}
