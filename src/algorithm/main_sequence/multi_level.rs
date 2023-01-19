use crate::algorithm::{
    dp_substate_vec::DpStateVec,
    indices::WordIndex,
    preprocess::partition_into_lines,
    scoring::{
        simple::{
            k_gram_sampling::KGramSamplingScoring, whitespace_ignoring::WhitespaceIgnoringScoring,
            zero_one::ZeroOneScoring, zero_or_information::ZeroOrInformationScoring, SimpleScoring,
        },
        AlignmentScoringMethod, AlignmentSliceScoring, InputSliceBounds, TScore,
    },
    DiffOp, LineScoringStrategy, PartitionedText,
};

use super::Aligner;

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

#[derive(Debug)]
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

fn dp_in_area(scoring: &AlignmentSliceScoring, area: &DpArea) -> Vec<ApxDiffOp> {
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
            scoring.set_starting_state([old_index, new_index], default_score, &mut dp[old_index][new_index]);
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

    let mut result = vec![];
    let mut old_index = scoring.slice.size[0];
    let mut new_index = scoring.slice.size[1];
    let mut substate = scoring.final_substate();
    while old_index > 0 || new_index > 0 {
        let (op, previous_substate) = dp[old_index][new_index][substate].previous_step.get().unwrap();
        substate = previous_substate;
        old_index -= op.movement()[0];
        new_index -= op.movement()[1];
        let apx_op = match op {
            DiffOp::Match => {
                if scoring.is_match([old_index, new_index]) {
                    ApxDiffOp::ExactMatch
                } else {
                    ApxDiffOp::ApproximateMatch
                }
            }
            DiffOp::Delete => ApxDiffOp::Delete,
            DiffOp::Insert => ApxDiffOp::Insert,
        };
        result.push(apx_op);
    }
    result.reverse();
    result
}

fn refine_alignment(
    fine_scoring: &AlignmentSliceScoring,
    fine_start: [usize; 2],
    fine_end: [usize; 2],
    coarse_alignment: &[ApxDiffOp],
    coarse_start: [usize; 2],
    coarse_to_fine_index: [&[usize]; 2],
    max_bruteforce: usize,
) -> Vec<ApxDiffOp> {
    const EXACT_RUN_THRESHOLD: usize = 4;
    const FUZZY_RUN_THRESHOLD: usize = 1;

    let mut is_exact_run = vec![false; coarse_alignment.len()];
    let mut is_fuzzy_run = vec![false; coarse_alignment.len()];

    let mut match_streak = 0;
    let mut exact_match_streak = 0;
    for (i, &op) in coarse_alignment.iter().enumerate() {
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

    let mut result: Vec<ApxDiffOp> = vec![];

    struct PreparedDp {
        area: DpArea,
        covers_till: [usize; 2],
        start_rect_at: [usize; 2],
    }
    let mut prepared_dp = PreparedDp {
        area: DpArea::new(fine_start),
        covers_till: fine_start,
        start_rect_at: fine_start,
    };

    let close_dp = |next_start: [usize; 2], prepared_dp: &mut PreparedDp, result: &mut Vec<ApxDiffOp>| {
        let slice = fine_scoring
            .slice
            .subslice(prepared_dp.area.start, prepared_dp.covers_till);
        let slice_scoring = AlignmentSliceScoring {
            slice,
            scoring: fine_scoring.scoring,
        };

        let mut dp_result = dp_in_area(&slice_scoring, &prepared_dp.area);
        result.append(&mut dp_result);
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

    let add_approximate_match =
        |match_from: [usize; 2], match_until: [usize; 2], prepared_dp: &mut PreparedDp, result: &mut Vec<ApxDiffOp>| {
            if match_until[0] - match_from[0] > max_bruteforce && match_until[1] - match_from[1] > max_bruteforce {
                rectangle_until(match_from, match_from, prepared_dp, result);
                close_dp(match_until, prepared_dp, result);
                for _ in match_from[0]..match_until[0] {
                    result.push(ApxDiffOp::Delete);
                }
                for _ in match_from[1]..match_until[1] {
                    result.push(ApxDiffOp::Insert);
                }
            } else {
                rectangle_until(match_until, match_from, prepared_dp, result);
            }
        };

    let mut coarse_indices = coarse_start;
    let mut fine_indices = [0, 1].map(|side| coarse_to_fine_index[side][coarse_indices[side]]);
    add_approximate_match(fine_start, fine_indices, &mut prepared_dp, &mut result);

    for (i, &op) in coarse_alignment.iter().enumerate() {
        let coarse_indices_after = [0, 1].map(|side| coarse_indices[side] + op.movement()[side]);
        let fine_indices_after = [0, 1].map(|side| coarse_to_fine_index[side][coarse_indices_after[side]]);

        if is_exact_run[i] {
            rectangle_until(fine_indices, fine_indices, &mut prepared_dp, &mut result);
            close_dp(fine_indices_after, &mut prepared_dp, &mut result);

            let covered_child_parts = fine_indices_after[0] - fine_indices[0];
            assert!(covered_child_parts == fine_indices_after[1] - fine_indices[1]);
            for _ in 0..covered_child_parts {
                result.push(ApxDiffOp::ExactMatch);
            }
        } else if is_fuzzy_run[i] {
            add_approximate_match(fine_indices, fine_indices_after, &mut prepared_dp, &mut result);
        }

        coarse_indices = coarse_indices_after;
        fine_indices = fine_indices_after;
    }
    add_approximate_match(fine_indices, fine_end, &mut prepared_dp, &mut result);
    rectangle_until(fine_end, fine_end, &mut prepared_dp, &mut result);
    close_dp(fine_end, &mut prepared_dp, &mut result);
    result
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

// Binary search. Array must be in ascending order.
// Result will be a number from 0..=array.len(), such that
// array[i] < value for i in 0..result
// and
// array[i] >= value for i in result..array.len()
fn lower_bound(array: &[usize], value: usize) -> usize {
    let mut low = 0;
    let mut high = array.len() + 1;
    while low + 1 < high {
        let mid = (low + high) / 2;
        if array[mid - 1] >= value {
            high = mid;
        } else {
            low = mid;
        }
    }
    low
}

// Binary search. Array must be in ascending order.
// Result will be a number from 0..=array.len(), such that
// array[i] <= value for i in 0..result
// and
// array[i] > value for i in result..array.len()
fn upper_bound(array: &[usize], value: usize) -> usize {
    let mut low = 0;
    let mut high = array.len() + 1;
    while low + 1 < high {
        let mid = (low + high) / 2;
        if array[mid - 1] > value {
            high = mid;
        } else {
            low = mid;
        }
    }
    low
}

pub(in crate::algorithm) struct MultiLevelAligner<'a> {
    bottom_level_scoring: &'a dyn AlignmentScoringMethod,
    other_scorings: Vec<Box<dyn AlignmentScoringMethod>>,
    coarse_to_fine_index: Vec<Vec<[Vec<usize>; 2]>>,
    max_bruteforce_on_level: Vec<usize>,
}

pub(in crate::algorithm) fn lines_then_words_aligner<'a>(
    text_words: &[[PartitionedText; 2]],
    word_scoring: &'a dyn AlignmentScoringMethod,
    line_scoring_strategy: LineScoringStrategy,
) -> MultiLevelAligner<'a> {
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

    let mut line_index_to_word_index = vec![];
    for file_id in 0..text_words.len() {
        line_index_to_word_index.push(
            [0, 1].map(|side| {
                partition_correspondence(&line_bounds[file_id][side], text_words[file_id][side].part_bounds)
            }),
        );
    }

    MultiLevelAligner {
        bottom_level_scoring: word_scoring,
        other_scorings: vec![line_scoring],
        coarse_to_fine_index: vec![line_index_to_word_index],
        max_bruteforce_on_level: vec![200, usize::MAX],
    }
}

impl<'a> Aligner for MultiLevelAligner<'a> {
    fn align(&self, file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> Vec<DiffOp> {
        let mut start = start.map(WordIndex::raw);
        let mut end = end.map(WordIndex::raw);

        let mut start_indices = vec![start];
        let mut end_indices = vec![end];

        for level_coarse_to_fine in self.coarse_to_fine_index.iter() {
            start = [0, 1].map(|side| lower_bound(&level_coarse_to_fine[file_ids[side]][side], start[side]));
            end = [0, 1].map(|side| upper_bound(&level_coarse_to_fine[file_ids[side]][side], end[side]) - 1);
            if start[0] > end[0] || start[1] > end[1] {
                break;
            }
            start_indices.push(start);
            end_indices.push(end);
        }
        let levels_count = start_indices.len();

        let mut scorings = vec![];
        for level in 0..levels_count {
            let slice = InputSliceBounds {
                file_ids,
                start: [0, 0],
                size: [0, 1].map(|side| {
                    if level == 0 {
                        *self.coarse_to_fine_index[0][file_ids[side]][side].last().unwrap()
                    } else {
                        self.coarse_to_fine_index[level - 1][file_ids[side]][side].len() - 1
                    }
                }),
            };
            scorings.push(AlignmentSliceScoring {
                slice,
                scoring: if level == 0 {
                    self.bottom_level_scoring
                } else {
                    self.other_scorings[level - 1].as_ref()
                },
            });
        }

        let top_level = levels_count - 1;
        let mut alignment = if end_indices[top_level][0] - start_indices[top_level][0]
            > self.max_bruteforce_on_level[top_level]
            && end_indices[top_level][1] - start_indices[top_level][1] > self.max_bruteforce_on_level[top_level]
        {
            let mut alignment = vec![ApxDiffOp::Delete; end_indices[top_level][0] - start_indices[top_level][0]];
            for _ in start_indices[top_level][1]..end_indices[top_level][1] {
                alignment.push(ApxDiffOp::Insert);
            }
            alignment
        } else {
            let mut area = DpArea::new(start_indices[top_level]);
            area.add_rectangle(start_indices[top_level], end_indices[top_level]);
            let slice = scorings[top_level]
                .slice
                .subslice(start_indices[top_level], end_indices[top_level]);
            let slice_scoring = AlignmentSliceScoring {
                slice,
                scoring: scorings[top_level].scoring,
            };
            dp_in_area(&slice_scoring, &area)
        };

        for level in (0..top_level).rev() {
            alignment = refine_alignment(
                &scorings[level],
                start_indices[level],
                end_indices[level],
                &alignment,
                start_indices[level + 1],
                [0, 1].map(|side| self.coarse_to_fine_index[level][file_ids[side]][side].as_slice()),
                self.max_bruteforce_on_level[level],
            );
        }
        alignment.iter().map(ApxDiffOp::to_diff_op).collect()
    }

    fn prefix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        self.bottom_level_scoring
            .prefix_scores(file_ids, start.map(WordIndex::raw), end.map(WordIndex::raw), alignment)
    }

    fn suffix_scores(
        &self,
        file_ids: [usize; 2],
        start: [WordIndex; 2],
        end: [WordIndex; 2],
        alignment: &[DiffOp],
    ) -> Vec<TScore> {
        self.bottom_level_scoring
            .suffix_scores(file_ids, start.map(WordIndex::raw), end.map(WordIndex::raw), alignment)
    }

    fn score_gaps_between(&self, _file_ids: [usize; 2], start: [WordIndex; 2], end: [WordIndex; 2]) -> TScore {
        self.bottom_level_scoring
            .score_gaps_between(start.map(WordIndex::raw), end.map(WordIndex::raw))
    }
}
