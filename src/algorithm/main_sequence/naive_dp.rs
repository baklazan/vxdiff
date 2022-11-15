use crate::algorithm::{
    scoring::{AlignmentScoringMethod, AlignmentSliceScoring, ScoreState, TScore},
    DiffOp,
};

fn compute_dp_matrix<AlignmentScoring: AlignmentScoringMethod>(
    alignment_scoring: &AlignmentSliceScoring<AlignmentScoring>,
    sizes: [usize; 2],
    row_range: usize,
) -> Vec<Vec<AlignmentScoring::State>> {
    let mut result = vec![vec![alignment_scoring.starting_state(TScore::NEG_INFINITY); sizes[1] + 1]; row_range];

    for old_index in 0..=sizes[0] {
        for new_index in 0..=sizes[1] {
            let default_score = if old_index == 0 && new_index == 0 {
                0.0
            } else {
                TScore::NEG_INFINITY
            };
            result[old_index % row_range][new_index] = alignment_scoring.starting_state(default_score);
            for op in [DiffOp::Delete, DiffOp::Insert, DiffOp::Match] {
                if old_index >= op.movement()[0] && new_index >= op.movement()[1] {
                    alignment_scoring.consider_step(
                        [old_index, new_index],
                        result[(old_index - op.movement()[0]) % row_range][new_index - op.movement()[1]].clone(),
                        &mut result[old_index % row_range][new_index],
                        op,
                    )
                }
            }
        }
    }

    result
}

pub fn naive_dp<AlignmentScoring: AlignmentScoringMethod>(
    alignment_scoring: &AlignmentSliceScoring<AlignmentScoring>,
    sizes: [usize; 2],
) -> Vec<DiffOp> {
    let matrix = compute_dp_matrix(alignment_scoring, sizes, sizes[0] + 1);

    let mut best_substate = 0;
    let final_scores: Vec<TScore> = (0..AlignmentScoring::State::SUBSTATES_COUNT)
        .map(|substate| alignment_scoring.substate_score(&matrix[sizes[0]][sizes[1]], substate, [sizes[0], sizes[1]]))
        .collect();
    for substate in 1..AlignmentScoring::State::SUBSTATES_COUNT {
        if final_scores[substate] > final_scores[best_substate] {
            best_substate = substate;
        }
    }

    let mut result = vec![];
    let mut substate = best_substate;
    let mut indices = sizes;
    while indices[0] > 0 || indices[1] > 0 {
        let (op, next_substate) = alignment_scoring
            .substate_movement(&matrix[indices[0]][indices[1]], substate)
            .unwrap();
        substate = next_substate;
        result.push(op);
        for side in 0..2 {
            indices[side] -= op.movement()[side];
        }
    }
    result.reverse();
    result
}

pub fn compute_score<AlignmentScoring: AlignmentScoringMethod>(
    alignment_scoring: &AlignmentSliceScoring<AlignmentScoring>,
    sizes: [usize; 2],
) -> TScore {
    let matrix = compute_dp_matrix(alignment_scoring, sizes, 2);
    let scores: Vec<TScore> = (0..AlignmentScoring::State::SUBSTATES_COUNT)
        .map(|substate| {
            alignment_scoring.substate_score(&matrix[sizes[0] % 2][sizes[1]], substate, [sizes[0], sizes[1]])
        })
        .collect();

    let mut result = scores[0];
    for substate in 1..AlignmentScoring::State::SUBSTATES_COUNT {
        if scores[substate] > result {
            result = scores[substate];
        }
    }
    result
}
