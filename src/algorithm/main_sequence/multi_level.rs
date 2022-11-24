use crate::algorithm::{
    partition::partition_into_lines,
    scoring::{AlignmentScoringMethod, AlignmentSliceScoring, simple::{SimpleScoring, zero_one::ZeroOneScoring, zero_or_information::ZeroOrInformationScoring} },
    DiffOp, PartitionedText,
};

use super::{naive_dp, slices_for_files};

struct AlgorithmLevel<'a> {
    scoring: AlignmentSliceScoring<'a>,
    part_bounds_finer: [Vec<usize>; 2],
    max_bruteforced_width: usize,
}

fn refine_alignment(
    scoring: &AlignmentSliceScoring,
    parent_alignment: &[DiffOp],
    parent_bounds: &[Vec<usize>; 2],
    max_bruteforce: usize,
) -> Vec<DiffOp> {
    let mut processed_till = [0, 0];
    let mut parent_indices = [0, 0];
    let mut child_indices = [0, 0];
    let mut result: Vec<DiffOp> = vec![];

    let process_until = |until_indices: [usize; 2], result: &mut Vec<DiffOp>, processed_till: &mut [usize; 2]| {
        if until_indices != *processed_till {
            let sizes = [0, 1].map(|side| until_indices[side] - processed_till[side]);
            if sizes[0] <= max_bruteforce || sizes[1] <= max_bruteforce {
                let mut slice = scoring.slice;
                slice.start = *processed_till;
                slice.size = sizes;
                let slice_scoring = AlignmentSliceScoring {
                    slice,
                    scoring: scoring.scoring,
                };
                let mut dp_result = naive_dp::naive_dp(&slice_scoring);
                result.append(&mut dp_result);
            } else {
                for _ in 0..sizes[0] {
                    result.push(DiffOp::Delete);
                }
                for _ in 0..sizes[1] {
                    result.push(DiffOp::Insert);
                }
            }
        }
        *processed_till = until_indices;
    };

    for &op in parent_alignment {
        if op == DiffOp::Match {
            process_until(child_indices, &mut result, &mut processed_till);
            let covered_child_parts = parent_bounds[0][parent_indices[0] + 1] - parent_bounds[0][parent_indices[0]];
            assert!(
                covered_child_parts == parent_bounds[1][parent_indices[1] + 1] - parent_bounds[1][parent_indices[1]]
            );
            for _ in 0..covered_child_parts {
                result.push(DiffOp::Match);
            }
            for side in 0..2 {
                processed_till[side] += covered_child_parts;
            }
        }

        for side in 0..2 {
            parent_indices[side] += op.movement()[side];
            child_indices[side] = parent_bounds[side][parent_indices[side]];
        }
    }
    process_until(child_indices, &mut result, &mut processed_till);
    result
}

fn align_multi_level(levels: &[AlgorithmLevel]) -> Vec<DiffOp> {
    let parts_in_top_level = [0, 1].map(|side| levels[0].part_bounds_finer[side].len() - 1);
    let mut previous_bounds = &[0, 1].map(|side| vec![0, parts_in_top_level[side]]);
    let mut alignment = vec![DiffOp::Insert, DiffOp::Delete];
    for level in levels {
        alignment = refine_alignment(
            &level.scoring,
            &alignment,
            &previous_bounds,
            level.max_bruteforced_width,
        );
        previous_bounds = &level.part_bounds_finer;
    }
    alignment
}

fn partition_correspondence(coarse: &[usize], fine: &[usize]) -> Vec<usize> {
    let mut result = vec![];
    let mut coarse_index = 0;
    for (fine_index, &fine_value) in fine.iter().enumerate() {
        if fine_value == coarse[coarse_index] {
            result.push(fine_index);
            coarse_index += 1;
        }
    }
    assert!(result.len() == coarse.len());
    result
}

pub(in crate::algorithm) fn lines_then_words(
    text_words: &[[PartitionedText; 2]],
    word_scoring: &dyn AlignmentScoringMethod,
) -> Vec<Vec<DiffOp>> {
    let mut line_bounds = vec![];
    for file_text_words in text_words {
        let bounds = [0, 1].map(|side| partition_into_lines(file_text_words[side].text));
        line_bounds.push(bounds);
    }
    let mut text_lines = vec![];
    for (file_id, file_text_words) in text_words.iter().enumerate() {
        let file_text_lines: [PartitionedText; 2] = [0, 1].map(|side| PartitionedText {
            text: file_text_words[side].text,
            part_bounds: &line_bounds[file_id][side],
        });
        text_lines.push(file_text_lines);
    }

    let line_scoring = SimpleScoring{ match_scoring: ZeroOrInformationScoring::new(&text_lines) };

    let word_slices = slices_for_files(text_words);
    let line_slices = slices_for_files(&text_lines);

    let mut result = vec![];
    for file_id in 0..text_words.len() {
        let word_level = AlgorithmLevel {
            scoring: AlignmentSliceScoring {
                slice: word_slices[file_id],
                scoring: word_scoring,
            },
            max_bruteforced_width: 100,
            part_bounds_finer: [0, 1].map(|side| Vec::from(text_words[file_id][side].part_bounds)),
        };
        let line_level = AlgorithmLevel {
            scoring: AlignmentSliceScoring {
                slice: line_slices[file_id],
                scoring: &line_scoring,
            },
            max_bruteforced_width: usize::MAX,
            part_bounds_finer: [0, 1].map(|side| {
                partition_correspondence(
                    text_lines[file_id][side].part_bounds,
                    text_words[file_id][side].part_bounds,
                )
            }),
        };
        result.push(align_multi_level(&[line_level, word_level]));
    }

    result
}
