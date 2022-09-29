use super::seed_selection::*;

use super::scoring::*;
use super::*;

pub fn extend_seed<AlignmentScoring: AlignmentScoringMethod, FragmentScoring: FragmentBoundsScoringMethod>(
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &FragmentScoring,
    seed: Seed,
    bounds_before: [usize; 2],
    bounds_after: [usize; 2],
) -> Option<AlignedFragment> {
    let mut seed_contains_viable_start = false;
    for (old_index, new_index) in (seed.start[0]..=seed.end[0]).zip(seed.start[1]..=seed.end[1]) {
        if bounds_scoring.is_viable_bound(0, old_index) && bounds_scoring.is_viable_bound(1, new_index) {
            seed_contains_viable_start = true;
            break;
        }
    }

    let mut dp_backward_start = seed.start;
    let mut dp_forward_start = seed.end;
    if seed_contains_viable_start {
        while !bounds_scoring.is_viable_bound(0, dp_backward_start[0])
            || !bounds_scoring.is_viable_bound(1, dp_backward_start[1])
        {
            dp_backward_start[0] += 1;
            dp_backward_start[1] += 1;
        }
        while !bounds_scoring.is_viable_bound(0, dp_forward_start[0])
            || !bounds_scoring.is_viable_bound(1, dp_forward_start[1])
        {
            dp_forward_start[0] -= 1;
            dp_forward_start[1] -= 1;
        }
    } else {
        for side in 0..2 {
            while !bounds_scoring.is_viable_bound(side, dp_forward_start[side]) {
                dp_forward_start[side] += 1;
            }
        }
        dp_backward_start = dp_forward_start;
    }

    const BAND_SIZE_LINES: usize = 3;
    const BAND_EOLS_BEFOREQ_BEST: usize = (BAND_SIZE_LINES / 2) + 1;
    const BAND_EOLS_AFTER_BEST: usize = BAND_SIZE_LINES + 1 - BAND_EOLS_BEFOREQ_BEST;

    let mut extension_alignments = vec![];
    let mut extension_ends = vec![];

    for (start, bound, direction) in [
        (dp_backward_start, bounds_before, DpDirection::Backward),
        (dp_forward_start, bounds_after, DpDirection::Forward),
    ] {
        let directed_sub = |left: usize, right: usize| match direction {
            DpDirection::Forward => left - right,
            DpDirection::Backward => right - left,
        };

        let directed_le = |left: usize, right: usize| match direction {
            DpDirection::Forward => left <= right,
            DpDirection::Backward => left >= right,
        };

        let mut dp: Vec<Vec<AlignmentScoring::State>> =
            vec![vec![alignment_scoring.starting_state(TScore::NEG_INFINITY)]];
        let mut row_starts = vec![start[1]];
        let mut mismatch_score: Vec<Vec<TScore>> = vec![vec![TScore::NEG_INFINITY]];
        let mut is_alive: Vec<Vec<Vec<bool>>> = vec![vec![vec![true; AlignmentScoring::State::SUBSTATES_COUNT]]];

        let mut best_start = (0, 0, 0);
        let mut best_score = TScore::NEG_INFINITY;

        let mut best_new_in_previous_row = start[1];

        let dp_step = match direction {
            DpDirection::Backward => usize::wrapping_sub(0, 1),
            DpDirection::Forward => 1,
        };

        for row_index in 0..=directed_sub(bound[0], start[0]) {
            let old_index = start[0].wrapping_add(row_index.wrapping_mul(dp_step));

            let central_new = if row_index == 0 {
                start[1]
            } else {
                if best_new_in_previous_row == bound[1] {
                    best_new_in_previous_row
                } else {
                    best_new_in_previous_row.wrapping_add(dp_step)
                }
            };

            if bounds_scoring.is_viable_bound(0, old_index) && old_index != bound[0] {
                // compute matrix row starts for the next row of old text
                let mut used_match_heuristic = false;
                if bounds_scoring.is_viable_bound(1, central_new) && central_new != bound[1] {
                    // check if the following text lines are matching
                    let mut next_line_end_old = old_index.wrapping_add(dp_step);
                    while !bounds_scoring.is_viable_bound(0, next_line_end_old) {
                        next_line_end_old = next_line_end_old.wrapping_add(dp_step);
                    }
                    let line_start_index_old = std::cmp::min(old_index, next_line_end_old);
                    let line_length = directed_sub(next_line_end_old, old_index);
                    let next_line_end_new = central_new.wrapping_add(line_length.wrapping_mul(dp_step));
                    let mut lines_matching = bounds_scoring.is_viable_bound(1, next_line_end_new);
                    let line_start_index_new = std::cmp::min(central_new, next_line_end_new);

                    for i in 0..line_length {
                        lines_matching &=
                            alignment_scoring.is_match(line_start_index_old + i, line_start_index_new + i);
                    }

                    if lines_matching {
                        for i in 1..=line_length {
                            row_starts.push(central_new.wrapping_add(i.wrapping_mul(dp_step)));
                            dp.push(vec![alignment_scoring.starting_state(TScore::NEG_INFINITY); 1]);
                            mismatch_score.push(vec![TScore::NEG_INFINITY; 1]);
                            is_alive.push(vec![vec![false; AlignmentScoring::State::SUBSTATES_COUNT]; 1]);
                        }
                        used_match_heuristic = true;
                    }
                }

                if !used_match_heuristic {
                    let mut row_start = central_new;
                    let current_row_start = row_starts[row_index];
                    let mut row_end = central_new;

                    let mut seen_eols_beforeq = if bounds_scoring.is_viable_bound(1, row_start) {
                        1
                    } else {
                        0
                    };
                    while row_start != current_row_start && seen_eols_beforeq < BAND_EOLS_BEFOREQ_BEST {
                        row_start = row_start.wrapping_sub(dp_step);
                        if bounds_scoring.is_viable_bound(1, row_start) {
                            seen_eols_beforeq += 1;
                        }
                    }

                    let mut seen_eols_after = 0;
                    while row_end != bound[1] && seen_eols_after < BAND_EOLS_AFTER_BEST {
                        row_end = row_end.wrapping_add(dp_step);
                        if bounds_scoring.is_viable_bound(1, row_end) {
                            seen_eols_after += 1;
                        }
                    }

                    let current_row_size = directed_sub(row_end, current_row_start) + 1;
                    while dp[row_index].len() < current_row_size {
                        dp[row_index].push(alignment_scoring.starting_state(TScore::NEG_INFINITY));
                        mismatch_score[row_index].push(TScore::NEG_INFINITY);
                        is_alive[row_index].push(vec![false; AlignmentScoring::State::SUBSTATES_COUNT]);
                    }
                    let row_size = directed_sub(row_end, row_start) + 1;
                    let mut i = old_index.wrapping_add(dp_step);
                    loop {
                        row_starts.push(row_start);
                        dp.push(vec![alignment_scoring.starting_state(TScore::NEG_INFINITY); row_size]);
                        mismatch_score.push(vec![TScore::NEG_INFINITY; row_size]);
                        is_alive.push(vec![vec![false; AlignmentScoring::State::SUBSTATES_COUNT]; row_size]);
                        if bounds_scoring.is_viable_bound(0, i) {
                            break;
                        }
                        i = i.wrapping_add(dp_step);
                    }
                }
            }
            let mut best_alive_score_in_previous_row = TScore::NEG_INFINITY;
            for col_index in 0..dp[row_index].len() {
                let new_index = row_starts[row_index].wrapping_add(col_index.wrapping_mul(dp_step));

                let mut valid_steps = vec![];
                for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                    let old_after = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
                    let new_after = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
                    if directed_le(start[0], old_after) {
                        let row_index_after = directed_sub(old_after, start[0]);
                        if directed_le(row_starts[row_index_after], new_after) {
                            let col_index_after = directed_sub(new_after, row_starts[row_index_after]);
                            if col_index_after < dp[row_index_after].len() {
                                valid_steps.push((row_index_after, col_index_after, op));
                            }
                        }
                    }
                }

                for step in valid_steps.iter() {
                    mismatch_score[row_index][col_index] =
                        TScore::max(mismatch_score[row_index][col_index], mismatch_score[step.0][step.1]);
                }
                let change_state_score = bounds_scoring.fragment_bound_penalty(old_index, new_index);
                dp[row_index][col_index] =
                    alignment_scoring.starting_state(mismatch_score[row_index][col_index] + change_state_score);
                if old_index == start[0] && new_index == start[1] {
                    dp[row_index][col_index] = alignment_scoring.starting_state(0.0);
                }

                for step in valid_steps {
                    alignment_scoring.consider_step(
                        old_index,
                        new_index,
                        dp[step.0][step.1].clone(),
                        &mut dp[row_index][col_index],
                        step.2,
                        direction.clone(),
                    );
                }

                for (substate, movement) in dp[row_index][col_index].substate_movements().iter().enumerate() {
                    if movement.is_none() {
                        is_alive[row_index][col_index][substate] = old_index == start[0] && new_index == start[1];
                    } else {
                        let (op, next_substate) = movement.unwrap();
                        let old_after = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
                        let row_index_after = directed_sub(old_after, start[0]);
                        let new_after = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
                        let col_index_after = directed_sub(new_after, row_starts[row_index_after]);
                        is_alive[row_index][col_index][substate] =
                            is_alive[row_index_after][col_index_after][next_substate];
                    }
                }

                for (substate, &score) in dp[row_index][col_index].substate_scores().iter().enumerate() {
                    if !is_alive[row_index][col_index][substate] {
                        continue;
                    }

                    if score > best_alive_score_in_previous_row {
                        best_alive_score_in_previous_row = score;
                        best_new_in_previous_row = new_index;
                    }

                    let proposed_score = score + change_state_score;
                    if proposed_score > best_score {
                        best_score = proposed_score;
                        best_start = (old_index, new_index, substate);
                    }

                    mismatch_score[row_index][col_index] =
                        TScore::max(mismatch_score[row_index][col_index], proposed_score);
                }
            }
            if best_alive_score_in_previous_row == TScore::NEG_INFINITY {
                break;
            }
        }
        let mut alignment = vec![];
        let mut old_index = best_start.0;
        let mut new_index = best_start.1;
        let mut substate = best_start.2;
        while old_index != start[0] || new_index != start[1] {
            let row_index = directed_sub(old_index, start[0]);
            let col_index = directed_sub(new_index, row_starts[row_index]);
            let movement = dp[row_index][col_index].substate_movements()[substate];
            assert!(movement.is_some());
            let (op, next_substate) = movement.unwrap();
            alignment.push(op);
            substate = next_substate;
            old_index = old_index.wrapping_sub(op.movement()[0].wrapping_mul(dp_step));
            new_index = new_index.wrapping_sub(op.movement()[1].wrapping_mul(dp_step));
        }
        extension_alignments.push(alignment);
        extension_ends.push([best_start.0, best_start.1]);
    }

    if extension_ends[0][0] >= extension_ends[1][0] || extension_ends[0][1] >= extension_ends[1][1] {
        return None;
    }

    let mut alignment = std::mem::take(&mut extension_alignments[0]);
    alignment.append(&mut vec![DiffOp::Match; dp_forward_start[0] - dp_backward_start[0]]);
    extension_alignments[1].reverse();
    alignment.append(&mut extension_alignments[1]);

    Some(AlignedFragment {
        starts: extension_ends[0],
        ends: extension_ends[1],
        alignment,
    })
}
