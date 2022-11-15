use super::seed_selection::*;

use super::scoring::*;
use super::*;

pub fn adaptive_dp<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    alignment_scoring: &AlignmentSliceScoring<AlignmentScoring>,
    bounds_scoring: &BoundsSliceScoring<FragmentScoring>,
    bound: [usize; 2],
) -> (Vec<DiffOp>, [usize; 2]) {
    const BAND_SIZE_WORDS: usize = 100;

    let mut dp: Vec<Vec<AlignmentScoring::State>> = vec![vec![alignment_scoring.starting_state(0.0)]];
    let mut diag_start_new = vec![0];

    let mut best_start = ([0, 0], 0);
    let mut best_score = bounds_scoring.fragment_bound_penalty([0, 0]);

    let mut best_new_in_previous_diag = 0;
    let mut in_heuristic_until: usize = 0;

    let skip_matching_lines = |position: [usize; 2]| -> [usize; 2] {
        let mut result = position;
        while result[0] < bound[0] && result[1] < bound[1] {
            let next_bound = bounds_scoring.nearest_bound_point([result[0] + 1, result[1] + 1]);
            if next_bound[0] - result[0] != next_bound[1] - result[1] {
                return result;
            }
            for i in 0..(next_bound[0] - result[0]) {
                if !alignment_scoring.is_match([result[0] + i, result[1] + i]) {
                    return result;
                }
            }
            result = next_bound;
        }
        result
    };

    let mut compute_diagonal_bounds = |index_sum: usize, previous_best_new: usize| -> (usize, usize) {
        let previous_best = [index_sum - 1 - previous_best_new, previous_best_new];
        if index_sum > in_heuristic_until && bounds_scoring.nearest_bound_point(previous_best) == previous_best {
            let after_match = skip_matching_lines(previous_best);
            if after_match != previous_best {
                in_heuristic_until = after_match[0] + after_match[1];
            }
        }

        if index_sum < in_heuristic_until {
            if index_sum % 2 == in_heuristic_until % 2 {
                return (previous_best_new + 1, previous_best_new + 2);
            }
            return (previous_best_new + 1, previous_best_new + 1);
        }

        let mut starting_new = 0;
        if index_sum > bound[0] {
            starting_new = index_sum - bound[0];
        }
        if previous_best_new + 1 > starting_new + BAND_SIZE_WORDS / 2 {
            starting_new = previous_best_new + 1 - BAND_SIZE_WORDS / 2;
        }
        let mut ending_new_inclusive = std::cmp::min(index_sum, bound[1]);
        ending_new_inclusive = std::cmp::min(ending_new_inclusive, previous_best_new + BAND_SIZE_WORDS / 2);
        (starting_new, ending_new_inclusive + 1)
    };

    for index_sum in 1..=(bound[0] + bound[1]) {
        let (starting_new, ending_new) = compute_diagonal_bounds(index_sum, best_new_in_previous_diag);
        diag_start_new.push(starting_new);
        let diag_length = ending_new - starting_new;
        dp.push(vec![
            alignment_scoring.starting_state(TScore::NEG_INFINITY);
            diag_length
        ]);
        if diag_length == 0 {
            continue;
        }

        let mut best_score_in_diag = TScore::NEG_INFINITY;
        for new_index in starting_new..ending_new {
            let old_index = index_sum - new_index;
            let index_in_diag = new_index - starting_new;

            let mut valid_steps = vec![];
            for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                if old_index >= op.movement()[0] && new_index >= op.movement()[1] {
                    let old_before = old_index - op.movement()[0];
                    let new_before = new_index - op.movement()[1];
                    let sum_before = old_before + new_before;
                    if new_before >= diag_start_new[sum_before]
                        && new_before < diag_start_new[sum_before] + dp[sum_before].len()
                    {
                        let index_in_diag_before = new_before - diag_start_new[sum_before];
                        valid_steps.push((sum_before, index_in_diag_before, op));
                    }
                }
            }

            for (sum_before, index_in_diag_before, op) in valid_steps {
                alignment_scoring.consider_step(
                    [old_index, new_index],
                    dp[sum_before][index_in_diag_before].clone(),
                    &mut dp[index_sum][index_in_diag],
                    op,
                );
            }

            let nearest_bound_point = bounds_scoring.nearest_bound_point([old_index, new_index]);
            let change_state_penalty = bounds_scoring.fragment_bound_penalty(nearest_bound_point);

            for substate in 0..AlignmentScoring::State::SUBSTATES_COUNT {
                let score =
                    alignment_scoring.substate_score(&dp[index_sum][index_in_diag], substate, [old_index, new_index]);
                if score > best_score_in_diag {
                    best_score_in_diag = score;
                    best_new_in_previous_diag = new_index;
                }

                let proposed_score = score
                    + alignment_scoring.append_gaps([old_index, new_index], nearest_bound_point, substate)
                    + change_state_penalty;
                if proposed_score > best_score {
                    best_score = proposed_score;
                    best_start = ([old_index, new_index], substate);
                }
            }
        }
        const SCORE_SLACK: TScore = 20.0;
        if best_score_in_diag < best_score - SCORE_SLACK {
            break;
        }
    }

    let mut alignment = vec![];
    let actual_start = bounds_scoring.nearest_bound_point(best_start.0);
    for _ in best_start.0[0]..actual_start[0] {
        alignment.push(DiffOp::Delete);
    }
    for _ in best_start.0[1]..actual_start[1] {
        alignment.push(DiffOp::Insert);
    }

    let mut old_index = best_start.0[0];
    let mut new_index = best_start.0[1];
    let mut substate = best_start.1;
    while old_index != 0 || new_index != 0 {
        let index_sum = old_index + new_index;
        let index_in_diag = new_index - diag_start_new[index_sum];
        let movement = alignment_scoring.substate_movement(&dp[index_sum][index_in_diag], substate);
        assert!(movement.is_some());
        let (op, next_substate) = movement.unwrap();
        alignment.push(op);
        substate = next_substate;
        old_index = old_index - op.movement()[0];
        new_index = new_index - op.movement()[1];
    }
    alignment.reverse();
    (alignment, actual_start)
}

pub(super) fn extend_seed<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &FragmentScoring,
    seed: Seed,
    bounds_before: [usize; 2],
    bounds_after: [usize; 2],
) -> Option<AlignedFragment> {
    let mut seed_contains_viable_start = false;
    for (old_index, new_index) in (seed.start[0]..=seed.end[0]).zip(seed.start[1]..=seed.end[1]) {
        if bounds_scoring.is_viable_bound(0, old_index, seed.file_ids[0])
            && bounds_scoring.is_viable_bound(1, new_index, seed.file_ids[1])
        {
            seed_contains_viable_start = true;
            break;
        }
    }

    let mut dp_backward_start = seed.start;
    let mut dp_forward_start = seed.end;
    if seed_contains_viable_start {
        while !bounds_scoring.is_viable_bound(0, dp_backward_start[0], seed.file_ids[0])
            || !bounds_scoring.is_viable_bound(1, dp_backward_start[1], seed.file_ids[1])
        {
            dp_backward_start[0] += 1;
            dp_backward_start[1] += 1;
        }
        while !bounds_scoring.is_viable_bound(0, dp_forward_start[0], seed.file_ids[0])
            || !bounds_scoring.is_viable_bound(1, dp_forward_start[1], seed.file_ids[1])
        {
            dp_forward_start[0] -= 1;
            dp_forward_start[1] -= 1;
        }
    } else {
        for side in 0..2 {
            while !bounds_scoring.is_viable_bound(side, dp_forward_start[side], seed.file_ids[side]) {
                dp_forward_start[side] += 1;
            }
        }
        dp_backward_start = dp_forward_start;
    }

    let backward_slice = InputSliceBounds {
        file_ids: seed.file_ids,
        start: dp_backward_start,
        direction: DpDirection::Backward,
    };
    let backward_max = [0, 1].map(|side| dp_backward_start[side] - bounds_before[side]);
    let alignment_scoring_backward = AlignmentSliceScoring {
        slice: backward_slice,
        scoring: alignment_scoring,
    };
    let bounds_scoring_backward = BoundsSliceScoring {
        slice: backward_slice,
        scoring: bounds_scoring,
    };
    let (pre_seed_alignment, pre_seed_aligned_size) =
        adaptive_dp(&alignment_scoring_backward, &bounds_scoring_backward, backward_max);
    let fragment_start = [0, 1].map(|side| dp_backward_start[side] - pre_seed_aligned_size[side]);

    let forward_slice = InputSliceBounds {
        file_ids: seed.file_ids,
        start: dp_forward_start,
        direction: DpDirection::Forward,
    };
    let forward_max = [0, 1].map(|side| bounds_after[side] - dp_forward_start[side]);
    let alignment_scoring_forward = AlignmentSliceScoring {
        slice: forward_slice,
        scoring: alignment_scoring,
    };
    let bounds_scoring_forward = BoundsSliceScoring {
        slice: forward_slice,
        scoring: bounds_scoring,
    };
    let (mut post_seed_alignment, post_seed_aligned_size) =
        adaptive_dp(&alignment_scoring_forward, &bounds_scoring_forward, forward_max);
    let fragment_end = [0, 1].map(|side| dp_forward_start[side] + post_seed_aligned_size[side]);

    if fragment_start[0] >= fragment_end[0] || fragment_start[1] >= fragment_end[1] {
        return None;
    }

    let mut alignment = pre_seed_alignment;
    alignment.reverse();
    alignment.append(&mut vec![DiffOp::Match; dp_forward_start[0] - dp_backward_start[0]]);
    alignment.append(&mut post_seed_alignment);

    Some(AlignedFragment {
        starts: fragment_start,
        ends: fragment_end,
        file_ids: seed.file_ids,
        alignment,
    })
}
