use float_ord::FloatOrd;

use crate::algorithm::dynamic_programming::extend_seed;

use super::{
    scoring::{AffineScoring, TScore},
    seed_selection::{select_seeds, Seed},
    AlignedFragment, PartitionedText,
};
use std::collections::BTreeSet;

fn remove_covered_parts(seed: Seed, covered: &[BTreeSet<(usize, usize)>; 2]) -> Vec<Seed> {
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
        });
        current_end_index = start_index;
    }
    result
}

pub fn greedy_fragments(texts: &[PartitionedText; 2]) -> Vec<(AlignedFragment, bool)> {
    let seeds = select_seeds(texts);
    let scoring = AffineScoring::new(&texts);
    let mut prefix_scores = vec![0.0];
    let mut score = 0.0;
    for value in scoring.information_values[0].iter() {
        score += value;
        prefix_scores.push(score);
    }
    impl Ord for Seed {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            return usize::cmp(&self.start[0], &other.start[0]);
        }
    }

    impl PartialOrd for Seed {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    const SEED_INFORMATION_THRESHOLD: TScore = 20.0;
    let mut seeds_by_score: BTreeSet<(FloatOrd<TScore>, Seed)> = BTreeSet::new();
    for seed in seeds {
        let score = prefix_scores[seed.end[0]] - prefix_scores[seed.start[0]];
        if score > SEED_INFORMATION_THRESHOLD {
            seeds_by_score.insert((FloatOrd(score), seed));
        }
    }

    let mut covered: [BTreeSet<(usize, usize)>; 2] = [BTreeSet::new(), BTreeSet::new()];
    let mut main_sequence_indices: BTreeSet<(usize, usize)> = BTreeSet::new();
    let mut result = vec![];

    while !seeds_by_score.is_empty() {
        let current = *seeds_by_score.iter().next_back().unwrap();
        seeds_by_score.remove(&current);

        let after_removal = remove_covered_parts(current.1, &covered);
        if after_removal.len() != 1 || after_removal[0] != current.1 {
            // seed was invalid, insert its subseeds instead
            for seed in after_removal {
                let score = prefix_scores[seed.end[0]] - prefix_scores[seed.start[0]];
                if score > SEED_INFORMATION_THRESHOLD {
                    seeds_by_score.insert((FloatOrd(score), seed));
                }
            }
            continue;
        }

        let seed = after_removal[0];
        let mut bounds_before = [0, 0];
        let mut bounds_after = [texts[0].word_count(), texts[1].word_count()];
        use std::ops::Bound;
        for side in 0..2 {
            let covered_before = covered[side]
                .range((Bound::Unbounded, Bound::Excluded((seed.start[side], 0))))
                .next_back();
            if covered_before.is_some() {
                bounds_before[side] = covered_before.unwrap().1;
            }
            let covered_after = covered[side]
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

        let start_indices = (fragment.starts[0], fragment.starts[1]);
        let main_point_before = main_sequence_indices
            .range((Bound::Unbounded, Bound::Excluded(start_indices)))
            .next_back();
        let before_good = main_point_before.is_none() || main_point_before.unwrap().1 < fragment.starts[1];
        let main_point_after = main_sequence_indices
            .range((Bound::Included(start_indices), Bound::Unbounded))
            .next();
        let after_good = main_point_after.is_none() || main_point_after.unwrap().1 >= fragment.starts[1];
        let is_main_sequence = before_good && after_good;
        if is_main_sequence {
            main_sequence_indices.insert(start_indices);
        }
        for side in 0..2 {
            covered[side].insert((fragment.starts[side], fragment.ends[side]));
        }
        result.push((fragment, is_main_sequence));
    }

    result
}
