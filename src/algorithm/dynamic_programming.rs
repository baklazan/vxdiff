use super::seed_selection::*;

use super::scoring::*;
use super::*;

pub fn adaptive_dp<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    alignment_scoring: &AlignmentSliceScoring<AlignmentScoring>,
    bounds_scoring: &BoundsSliceScoring<FragmentScoring>,
    bound: [usize; 2],
) -> (Vec<DiffOp>, [usize; 2]) {
    const BAND_SIZE_LINES: usize = 3;
    const BAND_EOLS_BEFOREQ_BEST: usize = (BAND_SIZE_LINES / 2) + 1;
    const BAND_EOLS_AFTER_BEST: usize = BAND_SIZE_LINES + 1 - BAND_EOLS_BEFOREQ_BEST;

    let mut dp: Vec<Vec<AlignmentScoring::State>> = vec![vec![alignment_scoring.starting_state(TScore::NEG_INFINITY)]];
    let mut row_starts = vec![0];

    let mut best_start = (0, 0, 0);
    let mut best_score = TScore::NEG_INFINITY;

    let mut best_new_in_previous_row = 0;

    for old_index in 0..=bound[0] {
        let central_new = if old_index == 0 {
            0
        } else {
            if best_new_in_previous_row == bound[1] {
                best_new_in_previous_row
            } else {
                best_new_in_previous_row + 1
            }
        };

        if bounds_scoring.is_viable_bound(0, old_index) && old_index != bound[0] {
            // compute matrix row starts for the next row of old text
            let mut used_match_heuristic = false;
            if bounds_scoring.is_viable_bound(1, central_new) && central_new != bound[1] {
                // check if the following text lines are matching
                let mut next_line_end_old = old_index + 1;
                while !bounds_scoring.is_viable_bound(0, next_line_end_old) {
                    next_line_end_old = next_line_end_old + 1;
                }
                let line_start_index_old = std::cmp::min(old_index, next_line_end_old);
                let line_length = next_line_end_old - old_index;
                let next_line_end_new = central_new + line_length;
                let mut lines_matching = bounds_scoring.is_viable_bound(1, next_line_end_new);
                let line_start_index_new = std::cmp::min(central_new, next_line_end_new);

                for i in 0..line_length {
                    lines_matching &= alignment_scoring.is_match([line_start_index_old + i, line_start_index_new + i]);
                }

                if lines_matching {
                    for i in 1..=line_length {
                        row_starts.push(central_new + i);
                        dp.push(vec![alignment_scoring.starting_state(TScore::NEG_INFINITY); 1]);
                    }
                    used_match_heuristic = true;
                }
            }

            if !used_match_heuristic {
                let mut row_start = central_new;
                let current_row_start = row_starts[old_index];
                let mut row_end = central_new;

                let mut seen_eols_beforeq = if bounds_scoring.is_viable_bound(1, row_start) {
                    1
                } else {
                    0
                };
                while row_start != current_row_start && seen_eols_beforeq < BAND_EOLS_BEFOREQ_BEST {
                    row_start = row_start - 1;
                    if bounds_scoring.is_viable_bound(1, row_start) {
                        seen_eols_beforeq += 1;
                    }
                }

                let mut seen_eols_after = 0;
                while row_end != bound[1] && seen_eols_after < BAND_EOLS_AFTER_BEST {
                    row_end = row_end + 1;
                    if bounds_scoring.is_viable_bound(1, row_end) {
                        seen_eols_after += 1;
                    }
                }

                let current_row_size = row_end - current_row_start + 1;
                while dp[old_index].len() < current_row_size {
                    dp[old_index].push(alignment_scoring.starting_state(TScore::NEG_INFINITY));
                }
                let row_size = row_end - row_start + 1;
                let mut i = old_index + 1;
                loop {
                    row_starts.push(row_start);
                    dp.push(vec![alignment_scoring.starting_state(TScore::NEG_INFINITY); row_size]);
                    if bounds_scoring.is_viable_bound(0, i) {
                        break;
                    }
                    i = i + 1;
                }
            }
        }
        let mut best_score_in_previous_row = TScore::NEG_INFINITY;
        for col_index in 0..dp[old_index].len() {
            let new_index = row_starts[old_index] + col_index;

            let mut valid_steps = vec![];
            for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                if old_index >= op.movement()[0] {
                    let old_after = old_index - op.movement()[0];
                    let row_index_after = old_after;
                    if new_index - row_starts[row_index_after] >= op.movement()[1] {
                        let new_after = new_index - op.movement()[1];
                        let col_index_after = new_after - row_starts[row_index_after];
                        if col_index_after < dp[row_index_after].len() {
                            valid_steps.push((row_index_after, col_index_after, op));
                        }
                    }
                }
            }

            dp[old_index][col_index] = alignment_scoring.starting_state(TScore::NEG_INFINITY);
            if old_index == 0 && new_index == 0 {
                dp[old_index][col_index] = alignment_scoring.starting_state(0.0);
            }

            for step in valid_steps {
                alignment_scoring.consider_step(
                    [old_index, new_index],
                    dp[step.0][step.1].clone(),
                    &mut dp[old_index][col_index],
                    step.2,
                );
            }

            let change_state_score = bounds_scoring.fragment_bound_penalty([old_index, new_index]);

            for (substate, &score) in dp[old_index][col_index].substate_scores().iter().enumerate() {
                if score > best_score_in_previous_row {
                    best_score_in_previous_row = score;
                    best_new_in_previous_row = new_index;
                }

                let proposed_score = score + change_state_score;
                if proposed_score > best_score {
                    best_score = proposed_score;
                    best_start = (old_index, new_index, substate);
                }
            }
        }
        const SCORE_SLACK: TScore = 20.0;
        if best_score_in_previous_row < best_score - SCORE_SLACK {
            break;
        }
    }
    let mut alignment = vec![];
    let mut old_index = best_start.0;
    let mut new_index = best_start.1;
    let mut substate = best_start.2;
    while old_index != 0 || new_index != 0 {
        let row_index = old_index;
        let col_index = new_index - row_starts[row_index];
        let movement = dp[row_index][col_index].substate_movements()[substate];
        assert!(movement.is_some());
        let (op, next_substate) = movement.unwrap();
        alignment.push(op);
        substate = next_substate;
        old_index = old_index - op.movement()[0];
        new_index = new_index - op.movement()[1];
    }
    (alignment, [best_start.0, best_start.1])
}

pub fn extend_seed<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
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
    let (pre_seed_alignment, pre_seed_aligned_size) = adaptive_dp(
        &alignment_scoring_backward,
        &bounds_scoring_backward,
        backward_max,
    );
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
    alignment.append(&mut vec![DiffOp::Match; dp_forward_start[0] - dp_backward_start[0]]);
    post_seed_alignment.reverse();
    alignment.append(&mut post_seed_alignment);

    Some(AlignedFragment {
        starts: fragment_start,
        ends: fragment_end,
        file_ids: seed.file_ids,
        alignment,
    })
}
