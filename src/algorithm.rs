mod dynamic_programming;
mod scoring;
#[cfg(test)]
mod test;

use dynamic_programming::*;
use scoring::*;

#[derive(Debug)]
pub struct Diff<'a> {
    pub sections: Vec<Section<'a>>,
    pub files: Vec<FileDiff>,
}

#[derive(Debug, PartialEq)]
pub struct FileDiff {
    pub sides: [Vec<usize>; 2],
    pub alignment: Vec<DiffOp>,
}

#[derive(Debug, PartialEq)]
pub struct Section<'a> {
    pub sides: [SectionSide<'a>; 2],
    pub equal: bool,
}

#[derive(Debug, PartialEq)]
pub struct SectionSide<'a> {
    pub text_with_words: Vec<(bool, &'a str)>,
}

fn partition_into_words<'a>(text: &'a str) -> PartitionedText<'a> {
    let mut word_bounds = Vec::new();
    let mut was_last_alphabetic = false;
    let mut was_last_numeric = false;
    for (i, c) in text.char_indices() {
        if c.is_alphabetic() && was_last_alphabetic {
            continue;
        }
        if c.is_numeric() && was_last_numeric {
            continue;
        }
        was_last_alphabetic = c.is_alphabetic();
        was_last_numeric = c.is_numeric();
        word_bounds.push(i);
    }
    word_bounds.push(text.len());
    PartitionedText { text, word_bounds }
}

pub struct PartitionedText<'a> {
    pub text: &'a str,
    pub word_bounds: Vec<usize>,
}

impl<'a> PartitionedText<'a> {
    fn word_count(&self) -> usize {
        self.word_bounds.len() - 1
    }
    fn get_word(&self, index: usize) -> &'a str {
        &self.text[self.word_bounds[index]..self.word_bounds[index + 1]]
    }
}

fn highlighted_subsegments<'a>(
    text: &PartitionedText<'a>,
    word_indices_and_highlight: &[(bool, usize)],
) -> Vec<(bool, &'a str)> {
    if word_indices_and_highlight.is_empty() {
        return Vec::new();
    }
    let mut result = Vec::new();

    let mut subsegment_starting_word_index = word_indices_and_highlight[0].1;
    for (i, (highlight, word_index)) in word_indices_and_highlight.iter().enumerate() {
        if i + 1 >= word_indices_and_highlight.len() || (*highlight != word_indices_and_highlight[i + 1].0) {
            let word_start_offset = text.word_bounds[subsegment_starting_word_index];
            let word_end_offset = text.word_bounds[word_index + 1];
            let word = &text.text[word_start_offset..word_end_offset];
            result.push((*highlight, word));
            subsegment_starting_word_index = word_index + 1;
        }
    }
    result
}

fn make_sections<'a>(texts: &[PartitionedText<'a>; 2], alignment: &[DiffOp]) -> Vec<Section<'a>> {
    let mut result = vec![];

    let mut section_contents: [Vec<(bool, usize)>; 2] = [vec![], vec![]];
    let mut word_indices = [0, 0];
    let mut section_contains_match = false;
    let mut current_line_start = [0, 0]; // indices into section_contents

    for (i, op) in alignment.iter().enumerate() {
        let is_last = i + 1 >= alignment.len();

        let (used_words, is_match) = match op {
            DiffOp::Delete => ([1, 0], false),
            DiffOp::Insert => ([0, 1], false),
            DiffOp::Match => ([1, 1], true),
        };

        let mut is_newline = false;

        for side in 0..2 {
            for _i in 0..used_words[side] {
                is_newline = texts[side].get_word(word_indices[side]) == "\n";
                section_contents[side].push((!is_match, word_indices[side]));
                word_indices[side] += 1;
                if is_newline {
                    current_line_start[side] = section_contents[side].len();
                }
            }
        }

        if is_match && !section_contains_match && !is_newline {
            if current_line_start[0] != 0 || current_line_start[1] != 0 {
                let sides = [0, 1].map(|side| SectionSide {
                    text_with_words: highlighted_subsegments(
                        &texts[side],
                        &section_contents[side][0..current_line_start[side]],
                    ),
                });
                result.push(Section { sides, equal: false });
                for side in 0..2 {
                    section_contents[side].drain(0..current_line_start[side]);
                    current_line_start[side] = 0;
                }
            }
        }

        section_contains_match |= is_match;

        if (is_match && is_newline) || is_last {
            let sides = [0, 1].map(|side| SectionSide {
                text_with_words: highlighted_subsegments(&texts[side], &section_contents[side]),
            });
            let equal = sides
                .iter()
                .all(|side| side.text_with_words.iter().all(|(highlight, _)| *highlight == false));
            result.push(Section { sides, equal });
            section_contents = [vec![], vec![]];
            current_line_start = [0, 0];
            section_contains_match = false;
        }
    }
    result
}

fn get_partitioned_subtext<'a>(text: &PartitionedText<'a>, from_word: usize, to_word: usize) -> PartitionedText<'a> {
    let mut word_bounds = vec![];
    for i in from_word..=to_word {
        word_bounds.push(text.word_bounds[i] - text.word_bounds[from_word]);
    }
    let subtext = &text.text[text.word_bounds[from_word]..text.word_bounds[to_word]];
    PartitionedText {
        text: subtext,
        word_bounds,
    }
}

pub fn diff_file<'a>(old: &'a str, new: &'a str) -> Diff<'a> {
    let texts = [partition_into_words(old), partition_into_words(new)];
    let mut can_change_state = [vec![true], vec![true]];
    for side in 0..2 {
        for i in 0..texts[side].word_count() {
            let word = texts[side].get_word(i);
            can_change_state[side].push(word == "\n" || i + 1 == texts[side].word_count());
        }
    }

    let supersection_candidates = supersection_candidates(
        texts[0].word_count(),
        texts[1].word_count(),
        &HistogramScoring::new(&texts),
        &can_change_state[0],
        &can_change_state[1],
    );

    let supersection_alignment =
        select_candidates(&supersection_candidates, [texts[0].word_count(), texts[1].word_count()]);
    let mut first_section_from_supersection: Vec<usize> = vec![];
    let mut sections_count_from_supersection: Vec<usize> = vec![];
    let mut sections = vec![];

    for supersection in supersection_alignment.supersections.iter() {
        let parts = [
            get_partitioned_subtext(&texts[0], supersection.starts[0], supersection.ends[0]),
            get_partitioned_subtext(&texts[1], supersection.starts[1], supersection.ends[1]),
        ];
        first_section_from_supersection.push(sections.len());
        let mut sections_from_supersection = make_sections(&parts, &supersection.alignment);
        sections_count_from_supersection.push(sections_from_supersection.len());
        sections.append(&mut sections_from_supersection);
    }

    let mut file_sides = [vec![], vec![]];
    let mut file_alignment = vec![];

    let mut super_indices = [0, 0];
    let mut word_indices = [0, 0];
    for op in supersection_alignment.alignment {
        let do_side = match op {
            DiffOp::Insert => [false, true],
            DiffOp::Delete => [true, false],
            DiffOp::Match => [true, true],
        };
        let mut sections_count = 0;
        for side in 0..2 {
            if !do_side[side] {
                continue;
            }
            let supersection_id = supersection_alignment.sides[side][super_indices[side]];
            let supersection = &supersection_alignment.supersections[supersection_id];
            if word_indices[side] < supersection.starts[side] {
                let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
                let indel_alignment = vec![indel_op; supersection.starts[side] - word_indices[side]];
                let mut parts = [
                    PartitionedText {
                        text: "",
                        word_bounds: vec![],
                    },
                    PartitionedText {
                        text: "",
                        word_bounds: vec![],
                    },
                ];
                parts[side] = get_partitioned_subtext(&texts[side], word_indices[side], supersection.starts[side]);
                let mut indel_sections = make_sections(&parts, &indel_alignment);
                for section_id in sections.len()..sections.len() + indel_sections.len() {
                    file_sides[side].push(section_id);
                    file_alignment.push(indel_op);
                }
                sections.append(&mut indel_sections);
            }

            sections_count = sections_count_from_supersection[supersection_id];
            for section_id in first_section_from_supersection[supersection_id]
                ..first_section_from_supersection[supersection_id] + sections_count
            {
                file_sides[side].push(section_id);
            }
            super_indices[side] += 1;
            word_indices[side] = supersection.ends[side];
        }
        for _ in 0..sections_count {
            file_alignment.push(op);
        }
    }

    for side in 0..2 {
        if word_indices[side] < texts[side].word_count() {
            let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
            let indel_alignment = vec![indel_op; texts[side].word_count() - word_indices[side]];
            let mut parts = [
                PartitionedText {
                    text: "",
                    word_bounds: vec![],
                },
                PartitionedText {
                    text: "",
                    word_bounds: vec![],
                },
            ];
            parts[side] = get_partitioned_subtext(&texts[side], word_indices[side], texts[side].word_count());
            let mut indel_sections = make_sections(&parts, &indel_alignment);
            for section_id in sections.len()..sections.len() + indel_sections.len() {
                file_sides[side].push(section_id);
                file_alignment.push(indel_op);
            }
            sections.append(&mut indel_sections);
        }
    }

    let file_diff = FileDiff {
        sides: file_sides,
        alignment: file_alignment,
    };

    Diff {
        sections,
        files: vec![file_diff],
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DiffOp {
    Match,
    Insert,
    Delete,
}

impl DiffOp {
    fn movement(&self) -> (usize, usize) {
        match self {
            DiffOp::Delete => (1, 0),
            DiffOp::Insert => (0, 1),
            DiffOp::Match => (1, 1),
        }
    }
}

fn clamp_interval(interval: (usize, usize), bounds: (usize, usize)) -> (usize, usize) {
    (
        interval.0.clamp(bounds.0, bounds.1),
        interval.1.clamp(bounds.0, bounds.1),
    )
}

#[derive(Copy, Clone, Debug)]
struct Candidate<'a> {
    starts: [usize; 2],
    ends: [usize; 2],
    original: &'a OriginalCandidate,
    alignment: &'a [DiffOp],
    alignment_interval_in_original: (usize, usize),
    score: TScore,
}

impl<'a> Candidate<'a> {
    fn from(original: &'a OriginalCandidate) -> Candidate<'a> {
        Candidate {
            original,
            starts: original.starts,
            ends: original.ends,
            alignment: &original.alignment,
            score: *original.prefix_scores.last().unwrap(),
            alignment_interval_in_original: (0, original.alignment.len()),
        }
    }

    fn as_subinterval(original: &'a OriginalCandidate, alignment_subinterval: (usize, usize)) -> Candidate<'a> {
        let mut starts = [0; 2];
        let mut ends = [0; 2];
        for side in 0..2 {
            starts[side] = original.alignment_to_word[side][alignment_subinterval.0];
            ends[side] = original.alignment_to_word[side][alignment_subinterval.1];
        }
        let alignment = &original.alignment[alignment_subinterval.0..alignment_subinterval.1];
        let score = original.prefix_scores[alignment_subinterval.1] - original.prefix_scores[alignment_subinterval.0];
        Candidate {
            starts,
            ends,
            original,
            alignment,
            alignment_interval_in_original: alignment_subinterval,
            score,
        }
    }

    fn overlaps(&self, other: &Candidate) -> bool {
        for side in 0..2 {
            if self.starts[side] < other.ends[side] && other.starts[side] < self.ends[side] {
                return true;
            }
        }
        false
    }

    fn remove_overlapping(&self, other: &Candidate) -> Vec<Candidate<'a>> {
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
            result.push(Candidate::as_subinterval(self.original, alignment_interval));
        }
        result
    }
}

impl<'a> PartialEq for Candidate<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.starts == other.starts && self.ends == other.ends
    }
}

impl<'a> Eq for Candidate<'a> {}
impl<'a> Ord for Candidate<'a> {
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

impl<'a> PartialOrd for Candidate<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct SupersectionAlignment<'a> {
    pub supersections: Vec<Candidate<'a>>,
    pub sides: [Vec<usize>; 2],
    pub alignment: Vec<DiffOp>,
}

fn select_candidates(original_candidates: &Vec<OriginalCandidate>, text_lengths: [usize; 2]) -> SupersectionAlignment {
    #[derive(Clone, Copy, Eq, PartialEq)]
    struct CandidateByScore<'a> {
        candidate: Candidate<'a>,
    }

    impl<'a> Ord for CandidateByScore<'a> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            let ord = f64::partial_cmp(&self.candidate.score, &other.candidate.score).unwrap();
            if ord != std::cmp::Ordering::Equal {
                return ord;
            }
            Candidate::cmp(&self.candidate, &other.candidate)
        }
    }

    impl<'a> PartialOrd for CandidateByScore<'a> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    use std::collections::BTreeSet;

    let mut original_candidate_fragments: Vec<Vec<Candidate>> = vec![vec![]; original_candidates.len()];
    let mut candidates_by_score: BTreeSet<CandidateByScore> = BTreeSet::new();
    let mut original_covering_ids: [Vec<Vec<usize>>; 2] =
        [vec![vec![]; text_lengths[0]], vec![vec![]; text_lengths[1]]];

    for (id, original) in original_candidates.iter().enumerate() {
        for side in 0..2 {
            for position in original.starts[side]..original.ends[side] {
                original_covering_ids[side][position].push(id);
            }
        }

        let candidate = Candidate::from(original);
        candidates_by_score.insert(CandidateByScore { candidate });
        original_candidate_fragments[id].push(candidate);
    }

    let mut supersections = vec![];
    let mut is_supersection_match = vec![];
    let mut matching_indices: BTreeSet<(usize, usize)> = BTreeSet::new();
    let mut overlaping_set = vec![false; original_candidates.len()];

    while !candidates_by_score.is_empty() {
        let current = *candidates_by_score.iter().next_back().unwrap();
        candidates_by_score.remove(&current);
        let current = current.candidate;
        let start_indices = (current.starts[0], current.starts[1]);
        let match_before = matching_indices
            .range((std::ops::Bound::Unbounded, std::ops::Bound::Excluded(start_indices)))
            .next_back();
        let match_after = matching_indices
            .range((std::ops::Bound::Excluded(start_indices), std::ops::Bound::Unbounded))
            .next();
        let before_good = match_before == None || match_before.unwrap().1 <= start_indices.1;
        let after_good = match_after == None || match_after.unwrap().1 >= start_indices.1;
        if before_good && after_good {
            is_supersection_match.push(true);
            matching_indices.insert(start_indices);
        } else {
            const MOVED_SCORE_THRESHOLD: f64 = 20.0;
            if current.score < MOVED_SCORE_THRESHOLD {
                continue;
            }
            is_supersection_match.push(false);
        }
        supersections.push(current);

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
    let mut sides = [vec![], vec![]];
    let mut sides_match = [vec![], vec![]];
    for side in 0..2 {
        let mut ids_by_start = vec![];
        for (id, supersection) in supersections.iter().enumerate() {
            ids_by_start.push((supersection.starts[side], id));
        }
        ids_by_start.sort();
        for (_start, id) in ids_by_start {
            sides[side].push(id);
            sides_match[side].push(is_supersection_match[id]);
        }
    }
    let mut alignment = vec![];
    let mut new_index = 0;
    for (_old_index, old_match) in sides_match[0].iter().enumerate() {
        if *old_match {
            while !sides_match[1][new_index] {
                alignment.push(DiffOp::Insert);
                new_index += 1;
            }
            alignment.push(DiffOp::Match);
            new_index += 1;
        } else {
            alignment.push(DiffOp::Delete);
        }
    }
    while new_index < sides_match[1].len() {
        alignment.push(DiffOp::Insert);
        new_index += 1;
    }
    SupersectionAlignment {
        supersections,
        sides,
        alignment,
    }
}
