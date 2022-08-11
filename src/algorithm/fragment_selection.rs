use super::{dynamic_programming::OriginalCandidate, scoring::TScore, AlignedFragment, DiffOp};

#[derive(Copy, Clone, Debug)]
struct CandidateFragment<'a> {
    starts: [usize; 2],
    ends: [usize; 2],
    original: &'a OriginalCandidate,
    alignment: &'a [DiffOp],
    alignment_interval_in_original: (usize, usize),
    score: TScore,
}

impl<'a> CandidateFragment<'a> {
    fn from(original: &'a OriginalCandidate) -> CandidateFragment<'a> {
        CandidateFragment {
            original,
            starts: original.starts,
            ends: original.ends,
            alignment: &original.alignment,
            score: *original.prefix_scores.last().unwrap(),
            alignment_interval_in_original: (0, original.alignment.len()),
        }
    }

    fn as_subinterval(original: &'a OriginalCandidate, alignment_subinterval: (usize, usize)) -> CandidateFragment<'a> {
        let mut starts = [0; 2];
        let mut ends = [0; 2];
        for side in 0..2 {
            starts[side] = original.alignment_to_word[side][alignment_subinterval.0];
            ends[side] = original.alignment_to_word[side][alignment_subinterval.1];
        }
        let alignment = &original.alignment[alignment_subinterval.0..alignment_subinterval.1];
        let score = original.prefix_scores[alignment_subinterval.1] - original.prefix_scores[alignment_subinterval.0];
        CandidateFragment {
            starts,
            ends,
            original,
            alignment,
            alignment_interval_in_original: alignment_subinterval,
            score,
        }
    }

    fn overlaps(&self, other: &CandidateFragment) -> bool {
        for side in 0..2 {
            if self.starts[side] < other.ends[side] && other.starts[side] < self.ends[side] {
                return true;
            }
        }
        false
    }

    fn remove_overlapping(&self, other: &CandidateFragment) -> Vec<CandidateFragment<'a>> {
        let mut removed_intervals = [(0, 0); 2];
        for side in 0..2 {
            removed_intervals[side] = self
                .original
                .word_interval_to_alignment_interval((other.starts[side], other.ends[side]), side);
        }
        let mut result_alignment_intervals = vec![];
        let left_removed_start = std::cmp::min(removed_intervals[0].0, removed_intervals[1].0);
        if left_removed_start > self.alignment_interval_in_original.0 {
            result_alignment_intervals.push((self.alignment_interval_in_original.0, left_removed_start));
        }
        let between_removed_start = std::cmp::max(
            self.alignment_interval_in_original.0,
            std::cmp::min(removed_intervals[0].1, removed_intervals[1].1),
        );
        let between_removed_end = std::cmp::min(
            self.alignment_interval_in_original.1,
            std::cmp::max(removed_intervals[0].0, removed_intervals[1].0),
        );
        if between_removed_start < between_removed_end {
            result_alignment_intervals.push((between_removed_start, between_removed_end));
        }
        let right_removed_end = std::cmp::max(removed_intervals[0].1, removed_intervals[1].1);
        if right_removed_end < self.alignment_interval_in_original.1 {
            result_alignment_intervals.push((right_removed_end, self.alignment_interval_in_original.1));
        }
        let mut result = vec![];
        for alignment_interval in result_alignment_intervals {
            result.push(CandidateFragment::as_subinterval(self.original, alignment_interval));
        }
        result
    }
}

impl<'a> PartialEq for CandidateFragment<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.starts == other.starts && self.ends == other.ends
    }
}

impl<'a> Eq for CandidateFragment<'a> {}
impl<'a> Ord for CandidateFragment<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        for side in 0..2 {
            let order = usize::cmp(&self.starts[side], &other.starts[side]);
            if order != std::cmp::Ordering::Equal {
                return order;
            }
            let order = usize::cmp(&self.ends[side], &other.ends[side]);
            if order != std::cmp::Ordering::Equal {
                return order;
            }
        }
        std::cmp::Ordering::Equal
    }
}

impl<'a> PartialOrd for CandidateFragment<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> AlignedFragment<'a> {
    fn from(candidate: &CandidateFragment<'a>) -> AlignedFragment<'a> {
        AlignedFragment {
            starts: candidate.starts,
            ends: candidate.ends,
            alignment: candidate.alignment,
        }
    }
}

pub fn select_candidates(
    original_candidates: &Vec<OriginalCandidate>,
    text_lengths: [usize; 2],
    supersection_threshold: TScore,
    moved_threshold: TScore,
) -> Vec<(AlignedFragment, bool)> {
    #[derive(Clone, Copy, Eq, PartialEq)]
    struct CandidateByScore<'a> {
        candidate: CandidateFragment<'a>,
    }

    impl<'a> Ord for CandidateByScore<'a> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            let ord = f64::partial_cmp(&self.candidate.score, &other.candidate.score).unwrap();
            if ord != std::cmp::Ordering::Equal {
                return ord;
            }
            CandidateFragment::cmp(&self.candidate, &other.candidate)
        }
    }

    impl<'a> PartialOrd for CandidateByScore<'a> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    use std::collections::BTreeSet;

    let mut original_candidate_fragments: Vec<Vec<CandidateFragment>> = vec![vec![]; original_candidates.len()];
    let mut candidates_by_score: BTreeSet<CandidateByScore> = BTreeSet::new();
    let mut original_covering_ids: [Vec<Vec<usize>>; 2] =
        [vec![vec![]; text_lengths[0]], vec![vec![]; text_lengths[1]]];

    for (id, original) in original_candidates.iter().enumerate() {
        for side in 0..2 {
            for position in original.starts[side]..original.ends[side] {
                original_covering_ids[side][position].push(id);
            }
        }

        let candidate = CandidateFragment::from(original);
        candidates_by_score.insert(CandidateByScore { candidate });
        original_candidate_fragments[id].push(candidate);
    }

    let mut selected_candidates = vec![];
    let mut matching_indices: BTreeSet<(usize, usize)> = BTreeSet::new();
    let mut overlaping_set = vec![false; original_candidates.len()];

    while !candidates_by_score.is_empty() {
        let current = *candidates_by_score.iter().next_back().unwrap();
        candidates_by_score.remove(&current);
        let current = current.candidate;
        if current.score < supersection_threshold {
            break;
        }
        let start_indices = (current.starts[0], current.starts[1]);
        let match_before = matching_indices
            .range((std::ops::Bound::Unbounded, std::ops::Bound::Excluded(start_indices)))
            .next_back();
        let match_after = matching_indices
            .range((std::ops::Bound::Excluded(start_indices), std::ops::Bound::Unbounded))
            .next();
        let before_good = match_before == None || match_before.unwrap().1 <= start_indices.1;
        let after_good = match_after == None || match_after.unwrap().1 >= start_indices.1;
        let is_in_main_sequence = before_good && after_good;
        if is_in_main_sequence {
            matching_indices.insert(start_indices);
        } else {
            if current.score < moved_threshold {
                continue;
            }
        }
        selected_candidates.push((AlignedFragment::from(&current), is_in_main_sequence));

        for side in 0..2 {
            let mut influenced_original_ids = vec![];
            for position in current.starts[side]..current.ends[side] {
                for id in &original_covering_ids[side][position] {
                    if !overlaping_set[*id] {
                        overlaping_set[*id] = true;
                        influenced_original_ids.push(*id);
                    }
                }
            }

            for id in influenced_original_ids.iter() {
                let mut new_fragments = vec![];
                for fragment in std::mem::take(&mut original_candidate_fragments[*id]) {
                    if fragment.overlaps(&current) {
                        let mut subfragments = fragment.remove_overlapping(&current);
                        candidates_by_score.remove(&CandidateByScore { candidate: fragment });
                        for subfragment in subfragments.iter() {
                            candidates_by_score.insert(CandidateByScore {
                                candidate: *subfragment,
                            });
                        }
                        new_fragments.append(&mut subfragments);
                    } else {
                        new_fragments.push(fragment);
                    }
                }
                original_candidate_fragments[*id] = new_fragments;
            }

            for id in influenced_original_ids {
                overlaping_set[id] = false;
            }
        }
    }
    selected_candidates
}
