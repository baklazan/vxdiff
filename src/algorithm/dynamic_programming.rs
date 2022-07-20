use super::scoring::*;
use super::*;

#[derive(Debug)]
pub struct OriginalCandidate {
    pub starts: [usize; 2],
    pub ends: [usize; 2],
    pub alignment: Vec<DiffOp>,
    pub prefix_scores: Vec<TScore>,
    pub word_to_alignment: [Vec<usize>; 2], // words map to alignment steps
    pub alignment_to_word: [Vec<usize>; 2], // points between alignment steps map to points between words
}

impl OriginalCandidate {
    pub fn word_interval_to_alignment_interval(&self, interval: (usize, usize), side: usize) -> (usize, usize) {
        let interval = clamp_interval(interval, (self.starts[side], self.ends[side]));
        if interval.0 >= self.ends[side] {
            return (self.alignment.len(), self.alignment.len());
        }
        if interval.1 <= self.starts[side] {
            return (0, 0);
        }
        let first_word_index = interval.0 - self.starts[side];
        let last_word_index = interval.1 - 1 - self.starts[side];
        (
            self.word_to_alignment[side][first_word_index],
            self.word_to_alignment[side][last_word_index] + 1,
        )
    }
}

pub fn supersection_candidates<
    AlignmentScoring: AlignmentScoringMethod,
    SupersectionScoring: SupersectionBoundsScoringMethod,
>(
    old_len: usize,
    new_len: usize,
    alignment_scoring: &AlignmentScoring,
    bounds_scoring: &SupersectionScoring,
) -> Vec<OriginalCandidate> {
    type Position = (usize, usize);

    let mut match_state: Vec<Vec<AlignmentScoring::State>> =
        vec![vec![alignment_scoring.starting_state(f64::NEG_INFINITY); new_len + 1]; old_len + 1];
    let mut mismatch_score: Vec<Vec<TScore>> = vec![vec![0.0; new_len + 1]; old_len + 1];
    let mut root_position: Vec<Vec<Vec<Position>>> =
        vec![vec![vec![(old_len, new_len); AlignmentScoring::State::SUBSTATES_COUNT]; new_len + 1]; old_len + 1];

    let mut best_leaf_for_root: std::collections::HashMap<Position, (TScore, Position, usize)> =
        std::collections::HashMap::new();

    match_state[old_len][new_len] = alignment_scoring.starting_state(0.0);
    for old_index in (0..=old_len).rev() {
        for new_index in (0..=new_len).rev() {
            let change_state_score = bounds_scoring.supersection_bound_penalty(old_index, new_index);

            let ops = [DiffOp::Delete, DiffOp::Insert, DiffOp::Match];
            for op in ops {
                let step = op.movement();
                if old_index + step.0 <= old_len && new_index + step.1 <= new_len {
                    mismatch_score[old_index][new_index] = TScore::max(
                        mismatch_score[old_index][new_index],
                        mismatch_score[old_index + step.0][new_index + step.1],
                    );
                }
            }
            match_state[old_index][new_index] =
                alignment_scoring.starting_state(mismatch_score[old_index][new_index] + change_state_score);

            for op in ops {
                let step = op.movement();
                if old_index + step.0 <= old_len && new_index + step.1 <= new_len {
                    alignment_scoring.consider_step(
                        old_index,
                        new_index,
                        match_state[old_index + step.0][new_index + step.1].clone(),
                        &mut match_state[old_index][new_index],
                        op,
                    );
                }
            }
            let match_score = match_state[old_index][new_index].best_score();
            mismatch_score[old_index][new_index] =
                TScore::max(mismatch_score[old_index][new_index], match_score + change_state_score);

            let can_change_state = change_state_score > TScore::NEG_INFINITY;
            let match_scores = match_state[old_index][new_index].substate_scores();
            let position = (old_index, new_index);
            for (substate, movement) in match_state[old_index][new_index]
                .substate_movements()
                .iter()
                .enumerate()
            {
                match movement {
                    Some((op, next_substate)) => {
                        let step = op.movement();
                        let root = root_position[old_index + step.0][new_index + step.1][*next_substate];
                        root_position[old_index][new_index][substate] = root;
                        if can_change_state {
                            let old_value = best_leaf_for_root.get(&root).unwrap().0;
                            if match_scores[substate] > old_value {
                                best_leaf_for_root.insert(root, (match_scores[substate], position, substate));
                            }
                        }
                    }
                    None => {
                        root_position[old_index][new_index][substate] = position;
                        if can_change_state {
                            best_leaf_for_root.insert(position, (match_scores[substate], position, substate));
                        }
                    }
                }
            }
        }
    }

    let mut result = vec![];
    for (root, (score, position, substate)) in best_leaf_for_root.iter() {
        if *score - match_state[root.0][root.1].best_score() < SupersectionScoring::SUPERSECTION_THRESHOLD {
            continue;
        }
        let mut alignment = Vec::<DiffOp>::new();
        let mut prefix_scores = vec![0.0];
        let mut word_to_alignment = [vec![], vec![]];
        let (mut old_index, mut new_index) = position;
        let mut substate = *substate;
        let mut alignment_to_word = [vec![old_index], vec![new_index]];
        while old_index < root.0 || new_index < root.1 {
            let (op, next_substate) = match_state[old_index][new_index].substate_movements()[substate].unwrap();
            let old_score = match_state[old_index][new_index].substate_scores()[substate];
            let step = op.movement();
            old_index += step.0;
            new_index += step.1;
            substate = next_substate;
            let new_score = match_state[old_index][new_index].substate_scores()[substate];

            if step.0 == 1 {
                word_to_alignment[0].push(alignment.len());
            }
            if step.1 == 1 {
                word_to_alignment[1].push(alignment.len());
            }
            prefix_scores.push(prefix_scores.last().unwrap() + old_score - new_score);
            alignment.push(op);
            alignment_to_word[0].push(old_index);
            alignment_to_word[1].push(new_index);
        }
        result.push(OriginalCandidate {
            starts: [position.0, position.1],
            ends: [root.0, root.1],
            alignment,
            prefix_scores,
            word_to_alignment,
            alignment_to_word,
        });
    }
    result
}
