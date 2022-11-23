use float_ord::FloatOrd;

use crate::algorithm::{dynamic_programming::extend_seed, scoring::affine_scoring::AffineWordScoring};

use super::{
    scoring::TScore,
    seed_selection::{select_seeds, Seed},
    AlignedFragment, PartitionedText,
};
use std::collections::BTreeSet;

fn remove_covered_parts(seed: Seed, covered: [&BTreeSet<(usize, usize)>; 2]) -> Vec<Seed> {
    let mut intervals = vec![];
    use std::ops::Bound;
    for side in 0..2 {
        intervals.push(
            covered[side]
                .range((Bound::Unbounded, Bound::Excluded((seed.end[side], 0))))
                .rev()
                .peekable(),
        );
    }
    let mut current_end_index: usize = seed.end[0] - seed.start[0];
    let mut result = vec![];
    'main_loop: while current_end_index > 0 {
        for side in 0..2 {
            let interval = intervals[side].peek();
            if interval.is_none() {
                continue;
            }
            let interval = *interval.unwrap();
            let interval_end_index = interval.1.saturating_sub(seed.start[side]);
            if interval_end_index >= current_end_index {
                intervals[side].next();
                current_end_index = usize::min(current_end_index, interval.0.saturating_sub(seed.start[side]));
                continue 'main_loop;
            }
        }
        let mut start_index = 0;
        for side in 0..2 {
            let interval = intervals[side].peek();
            if interval.is_none() {
                continue;
            }
            let interval = interval.unwrap();
            let interval_end_index = interval.1.saturating_sub(seed.start[side]);
            start_index = usize::max(start_index, interval_end_index);
        }
        assert!(start_index < current_end_index);
        result.push(Seed {
            start: [seed.start[0] + start_index, seed.start[1] + start_index],
            end: [seed.start[0] + current_end_index, seed.start[1] + current_end_index],
            file_ids: seed.file_ids,
        });
        current_end_index = start_index;
    }
    result
}

pub(super) fn greedy_fragments(texts: &[[PartitionedText; 2]]) -> Vec<(AlignedFragment, bool)> {
    let seeds = select_seeds(texts);
    let scoring = AffineWordScoring::new(&texts);
    let mut prefix_scores = vec![];
    for file_id in 0..texts.len() {
        let mut file_prefix_scores = vec![0.0];
        let mut score = 0.0;
        for value in scoring.information_values[file_id][0].iter() {
            score += value;
            file_prefix_scores.push(score);
        }
        prefix_scores.push(file_prefix_scores);
    }

    const SEED_INFORMATION_THRESHOLD: TScore = 20.0;
    let mut seeds_by_score: BTreeSet<(FloatOrd<TScore>, Seed)> = BTreeSet::new();
    for seed in seeds {
        let score = prefix_scores[seed.file_ids[0]][seed.end[0]] - prefix_scores[seed.file_ids[0]][seed.start[0]];
        if score > SEED_INFORMATION_THRESHOLD {
            seeds_by_score.insert((FloatOrd(score), seed));
        }
    }

    let files_count = texts.len();
    let mut covered: Vec<[BTreeSet<(usize, usize)>; 2]> = vec![[BTreeSet::new(), BTreeSet::new()]; files_count];
    let mut main_sequence_indices: Vec<BTreeSet<(usize, usize)>> = vec![BTreeSet::new(); files_count];
    let mut result = vec![];

    while !seeds_by_score.is_empty() {
        let current = *seeds_by_score.iter().next_back().unwrap();
        seeds_by_score.remove(&current);

        let seed = current.1;

        let covered_for_seed = [&covered[seed.file_ids[0]][0], &covered[seed.file_ids[1]][1]];
        let after_removal = remove_covered_parts(seed, covered_for_seed);
        if after_removal.len() != 1 || after_removal[0] != seed {
            // seed was invalid, insert its subseeds instead
            for seed in after_removal {
                let score =
                    prefix_scores[seed.file_ids[0]][seed.end[0]] - prefix_scores[seed.file_ids[0]][seed.start[0]];
                if score > SEED_INFORMATION_THRESHOLD {
                    seeds_by_score.insert((FloatOrd(score), seed));
                }
            }
            continue;
        }

        let seed = after_removal[0];
        let mut bounds_before = [0, 0];
        let mut bounds_after = [
            texts[seed.file_ids[0]][0].word_count(),
            texts[seed.file_ids[1]][1].word_count(),
        ];
        use std::ops::Bound;
        for side in 0..2 {
            let covered_before = covered_for_seed[side]
                .range((Bound::Unbounded, Bound::Excluded((seed.start[side], 0))))
                .next_back();
            if covered_before.is_some() {
                bounds_before[side] = covered_before.unwrap().1;
            }
            let covered_after = covered_for_seed[side]
                .range((Bound::Included((seed.start[side], 0)), Bound::Unbounded))
                .next();
            if covered_after.is_some() {
                bounds_after[side] = covered_after.unwrap().0;
            }
        }

        let fragment = extend_seed(&scoring, &scoring, seed, bounds_before, bounds_after);
        if fragment.is_none() {
            continue;
        }
        let fragment = fragment.unwrap();

        let mut is_main_sequence = false;
        if fragment.file_ids[0] == fragment.file_ids[1] {
            let file_id = fragment.file_ids[0];
            let start_indices = (fragment.starts[0], fragment.starts[1]);
            let main_point_before = main_sequence_indices[file_id]
                .range((Bound::Unbounded, Bound::Excluded(start_indices)))
                .next_back();
            let before_good = main_point_before.is_none() || main_point_before.unwrap().1 < fragment.starts[1];
            let main_point_after = main_sequence_indices[file_id]
                .range((Bound::Included(start_indices), Bound::Unbounded))
                .next();
            let after_good = main_point_after.is_none() || main_point_after.unwrap().1 >= fragment.starts[1];
            is_main_sequence = before_good && after_good;

            if is_main_sequence {
                main_sequence_indices[file_id].insert(start_indices);
            }
        }
        for side in 0..2 {
            covered[seed.file_ids[side]][side].insert((fragment.starts[side], fragment.ends[side]));
        }
        result.push((fragment, is_main_sequence));
    }

    result
}
