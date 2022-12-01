use crate::algorithm::{
    dp_substate_vec::DpStateVec,
    preprocess::partition_into_lines,
    scoring::{
        simple::{
            k_gram_sampling::KGramSamplingScoring, whitespace_ignoring::WhitespaceIgnoringScoring,
            zero_one::ZeroOneScoring, zero_or_information::ZeroOrInformationScoring, SimpleScoring,
        },
        AlignmentScoringMethod, AlignmentSliceScoring, TScore,
    },
    DiffOp, LineScoringStrategy, PartitionedText,
};

use super::slices_for_files;

struct AlgorithmLevel<'a> {
    scoring: AlignmentSliceScoring<'a>,
    part_bounds_finer: [Vec<usize>; 2],
    max_bruteforced_width: usize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum ApxDiffOp {
    ExactMatch,
    ApproximateMatch,
    Insert,
    Delete,
}

impl ApxDiffOp {
    pub fn movement(&self) -> [usize; 2] {
        match self {
            ApxDiffOp::Delete => [1, 0],
            ApxDiffOp::Insert => [0, 1],
            ApxDiffOp::ExactMatch => [1, 1],
            ApxDiffOp::ApproximateMatch => [1, 1],
        }
    }

    pub fn to_diff_op(&self) -> DiffOp {
        match self {
            ApxDiffOp::Delete => DiffOp::Delete,
            ApxDiffOp::Insert => DiffOp::Insert,
            ApxDiffOp::ExactMatch => DiffOp::Match,
            ApxDiffOp::ApproximateMatch => panic!("Can't convert ApproximateMatch to DiffOp"),
        }
    }
}

struct DpArea {
    start: [usize; 2],
    row_ranges_local_inclusive: Vec<(usize, usize)>,
}

impl DpArea {
    fn new(start: [usize; 2]) -> DpArea {
        DpArea {
            start,
            row_ranges_local_inclusive: vec![(0, 0)],
        }
    }

    fn add_rectangle(&mut self, low_global: [usize; 2], high_global: [usize; 2]) {
        let row_range_local = (low_global[1] - self.start[1], high_global[1] - self.start[1]);
        for row_global in low_global[0]..=high_global[0] {
            let row_local = row_global - self.start[0];
            if row_local < self.row_ranges_local_inclusive.len() {
                self.row_ranges_local_inclusive[row_local].0 =
                    std::cmp::min(self.row_ranges_local_inclusive[row_local].0, row_range_local.0);
                self.row_ranges_local_inclusive[row_local].1 =
                    std::cmp::max(self.row_ranges_local_inclusive[row_local].1, row_range_local.1);
            } else {
                self.row_ranges_local_inclusive.push(row_range_local);
            }
        }
    }
}

fn dp_in_area(scoring: &AlignmentSliceScoring, area: &DpArea) -> Vec<DiffOp> {
    let mut dp: Vec<DpStateVec> = area
        .row_ranges_local_inclusive
        .iter()
        .map(|(start, end)| DpStateVec::new_with_offset(end + 1 - start, scoring.substates_count(), *start))
        .collect();

    for old_index in 0..=scoring.slice.size[0] {
        for new_index in area.row_ranges_local_inclusive[old_index].0..=area.row_ranges_local_inclusive[old_index].1 {
            let default_score = if old_index == 0 && new_index == 0 {
                0.0
            } else {
                TScore::NEG_INFINITY
            };
            scoring.set_starting_state(default_score, &mut dp[old_index][new_index]);
            for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                let previos_old = old_index.wrapping_sub(op.movement()[0]);
                let previos_new = new_index.wrapping_sub(op.movement()[1]);
                if previos_old <= scoring.slice.size[0]
                    && previos_new >= area.row_ranges_local_inclusive[previos_old].0
                    && previos_new <= area.row_ranges_local_inclusive[previos_old].1
                {
                    scoring.consider_step(
                        [old_index, new_index],
                        &dp[previos_old][previos_new],
                        &dp[old_index][new_index],
                        op,
                    );
                }
            }
        }
    }

    let last_cell = &dp[scoring.slice.size[0]][scoring.slice.size[1]];
    let mut best_substate = 0;
    let mut best_score = scoring.substate_score(last_cell, 0, scoring.slice.size);
    for substate in 1..scoring.substates_count() {
        let score = scoring.substate_score(last_cell, substate, scoring.slice.size);
        if score > best_score {
            best_score = score;
            best_substate = substate;
        }
    }

    let mut result = vec![];
    let mut old_index = scoring.slice.size[0];
    let mut new_index = scoring.slice.size[1];
    let mut substate = best_substate;
    while old_index > 0 || new_index > 0 {
        let (op, previous_substate) = dp[old_index][new_index][substate].previous_step.get().unwrap();
        result.push(op);
        substate = previous_substate;
        old_index -= op.movement()[0];
        new_index -= op.movement()[1];
    }
    result.reverse();
    result
}

fn refine_alignment(
    scoring: &AlignmentSliceScoring,
    parent_alignment: &[ApxDiffOp],
    parent_bounds: &[Vec<usize>; 2],
    max_bruteforce: usize,
) -> Vec<ApxDiffOp> {
    const EXACT_RUN_THRESHOLD: usize = 4;
    const FUZZY_RUN_THRESHOLD: usize = 1;

    let mut is_exact_run = vec![false; parent_alignment.len()];
    let mut is_fuzzy_run = vec![false; parent_alignment.len()];

    let mut match_streak = 0;
    let mut exact_match_streak = 0;
    for (i, &op) in parent_alignment.iter().enumerate() {
        if op == ApxDiffOp::ApproximateMatch || op == ApxDiffOp::ExactMatch {
            match_streak += 1;
            if match_streak == FUZZY_RUN_THRESHOLD {
                for j in i - (FUZZY_RUN_THRESHOLD - 1)..i {
                    is_fuzzy_run[j] = true;
                }
            }
            if match_streak >= FUZZY_RUN_THRESHOLD {
                is_fuzzy_run[i] = true;
            }
            if op == ApxDiffOp::ExactMatch {
                exact_match_streak += 1;
                if exact_match_streak == EXACT_RUN_THRESHOLD {
                    for j in i - (EXACT_RUN_THRESHOLD - 1)..i {
                        is_exact_run[j] = true;
                    }
                }
                if exact_match_streak >= EXACT_RUN_THRESHOLD {
                    is_exact_run[i] = true;
                }
            }
        } else {
            match_streak = 0;
        }
        if op != ApxDiffOp::ExactMatch {
            exact_match_streak = 0;
        }
    }

    struct PreparedDp {
        area: DpArea,
        covers_till: [usize; 2],
        start_rect_at: [usize; 2],
    }
    let mut prepared_dp = PreparedDp {
        area: DpArea::new([0, 0]),
        covers_till: [0, 0],
        start_rect_at: [0, 0],
    };
    let mut parent_indices = [0, 0];
    let mut child_indices = [0, 0];
    let mut result: Vec<ApxDiffOp> = vec![];

    let close_dp = |next_start: [usize; 2], prepared_dp: &mut PreparedDp, result: &mut Vec<ApxDiffOp>| {
        let slice_size = [0, 1].map(|side| prepared_dp.covers_till[side] - prepared_dp.area.start[side]);
        let slice = scoring.slice.subslice(prepared_dp.area.start, slice_size);
        let slice_scoring = AlignmentSliceScoring {
            slice,
            scoring: scoring.scoring,
        };

        let dp_result = dp_in_area(&slice_scoring, &prepared_dp.area);
        let mut position_in_slice = [0, 0];
        for op in dp_result {
            let apx_op = match op {
                DiffOp::Match => {
                    if slice_scoring.is_match(position_in_slice) {
                        ApxDiffOp::ExactMatch
                    } else {
                        ApxDiffOp::ApproximateMatch
                    }
                }
                DiffOp::Insert => ApxDiffOp::Insert,
                DiffOp::Delete => ApxDiffOp::Delete,
            };
            result.push(apx_op);
            for side in 0..2 {
                position_in_slice[side] += op.movement()[side];
            }
        }
        *prepared_dp = PreparedDp {
            area: DpArea::new(next_start),
            covers_till: next_start,
            start_rect_at: next_start,
        };
    };

    let rectangle_until = |rect_until: [usize; 2],
                           next_rect_start: [usize; 2],
                           prepared_dp: &mut PreparedDp,
                           result: &mut Vec<ApxDiffOp>| {
        if rect_until[0] - prepared_dp.start_rect_at[0] > max_bruteforce
            && rect_until[1] - prepared_dp.start_rect_at[1] > max_bruteforce
        {
            let old_covered = prepared_dp.covers_till;

            close_dp(next_rect_start, prepared_dp, result);

            for _ in old_covered[0]..next_rect_start[0] {
                result.push(ApxDiffOp::Delete);
            }
            for _ in old_covered[1]..next_rect_start[1] {
                result.push(ApxDiffOp::Insert);
            }

            prepared_dp.start_rect_at = next_rect_start;
            prepared_dp.area.add_rectangle(prepared_dp.start_rect_at, rect_until);
            prepared_dp.covers_till = rect_until;
        } else {
            prepared_dp.area.add_rectangle(prepared_dp.start_rect_at, rect_until);
            prepared_dp.start_rect_at = next_rect_start;
            prepared_dp.covers_till = rect_until;
        }
    };

    for (i, &op) in parent_alignment.iter().enumerate() {
        let parent_indices_after = [0, 1].map(|side| parent_indices[side] + op.movement()[side]);
        let child_indices_after = [0, 1].map(|side| parent_bounds[side][parent_indices_after[side]]);

        if is_exact_run[i] {
            rectangle_until(child_indices, child_indices, &mut prepared_dp, &mut result);
            close_dp(child_indices_after, &mut prepared_dp, &mut result);

            let covered_child_parts = child_indices_after[0] - child_indices[0];
            assert!(covered_child_parts == child_indices_after[1] - child_indices[1]);
            for _ in 0..covered_child_parts {
                result.push(ApxDiffOp::ExactMatch);
            }
        } else if is_fuzzy_run[i] {
            if child_indices_after[0] - child_indices[0] > max_bruteforce
                && child_indices_after[1] - child_indices[1] > max_bruteforce
            {
                rectangle_until(child_indices, child_indices, &mut prepared_dp, &mut result);
                close_dp(child_indices_after, &mut prepared_dp, &mut result);
                for _ in child_indices[0]..child_indices_after[0] {
                    result.push(ApxDiffOp::Delete);
                }
                for _ in child_indices[1]..child_indices_after[1] {
                    result.push(ApxDiffOp::Insert);
                }
            } else {
                rectangle_until(child_indices_after, child_indices, &mut prepared_dp, &mut result);
            }
        }

        parent_indices = parent_indices_after;
        child_indices = child_indices_after;
    }
    rectangle_until(child_indices, child_indices, &mut prepared_dp, &mut result);
    close_dp(child_indices, &mut prepared_dp, &mut result);
    result
}

fn align_multi_level(levels: &[AlgorithmLevel]) -> Vec<DiffOp> {
    let parts_in_top_level = [0, 1].map(|side| levels[0].part_bounds_finer[side].len() - 1);
    let mut previous_bounds = &[0, 1].map(|side| vec![0, parts_in_top_level[side]]);
    let mut alignment = vec![ApxDiffOp::Insert, ApxDiffOp::Delete];
    for level in levels {
        alignment = refine_alignment(
            &level.scoring,
            &alignment,
            &previous_bounds,
            level.max_bruteforced_width,
        );
        previous_bounds = &level.part_bounds_finer;
    }
    alignment.iter().map(ApxDiffOp::to_diff_op).collect()
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
    line_scoring_strategy: LineScoringStrategy,
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

    let line_scoring: Box<dyn AlignmentScoringMethod> = match line_scoring_strategy {
        LineScoringStrategy::ZeroOne => Box::new(SimpleScoring {
            match_scoring: ZeroOneScoring::new(&text_lines),
        }),
        LineScoringStrategy::ZeroInformation => Box::new(SimpleScoring {
            match_scoring: ZeroOrInformationScoring::new(&text_lines),
        }),
        LineScoringStrategy::WhitespaceIgnoring => Box::new(SimpleScoring {
            match_scoring: WhitespaceIgnoringScoring::new(&text_lines),
        }),
        LineScoringStrategy::KGram => Box::new(SimpleScoring {
            match_scoring: KGramSamplingScoring::new(&text_lines),
        }),
    };

    let word_slices = slices_for_files(text_words);
    let line_slices = slices_for_files(&text_lines);

    let mut result = vec![];
    for file_id in 0..text_words.len() {
        let word_level = AlgorithmLevel {
            scoring: AlignmentSliceScoring {
                slice: word_slices[file_id],
                scoring: word_scoring,
            },
            max_bruteforced_width: 200,
            part_bounds_finer: [0, 1].map(|side| Vec::from(text_words[file_id][side].part_bounds)),
        };
        let line_level = AlgorithmLevel {
            scoring: AlignmentSliceScoring {
                slice: line_slices[file_id],
                scoring: line_scoring.as_ref(),
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
