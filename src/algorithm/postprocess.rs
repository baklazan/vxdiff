use super::{AlignedFragment, Diff, DiffOp, FileDiff, PartitionedText, Section, SectionSide};
use std::ops::Range;

fn highlighted_subsegments<'a>(
    text: &PartitionedText<'a>,
    word_indices_and_highlight: &[(bool, usize)],
) -> SectionSide {
    if word_indices_and_highlight.is_empty() {
        return SectionSide {
            highlight_bounds: vec![],
            highlight_first: false,
        };
    }
    let (highlight_first, first_word_offset) = word_indices_and_highlight[0];
    let mut highlight_bounds = vec![text.word_bounds[first_word_offset]];

    for (i, (highlight, word_index)) in word_indices_and_highlight.iter().enumerate() {
        if i + 1 >= word_indices_and_highlight.len() || (*highlight != word_indices_and_highlight[i + 1].0) {
            let word_end_offset = text.word_bounds[word_index + 1];
            highlight_bounds.push(word_end_offset);
        }
    }
    SectionSide {
        highlight_bounds,
        highlight_first,
    }
}

fn make_sections<'a>(texts: &[PartitionedText<'a>; 2], alignment: &[DiffOp]) -> Vec<Section> {
    let mut result = vec![];

    let mut section_contents: [Vec<(bool, usize)>; 2] = [vec![], vec![]];
    let mut word_indices = [0, 0];
    let mut section_contains_match = false;
    let mut current_line_start = [0, 0]; // indices into section_contents

    for (i, &op) in alignment.iter().enumerate() {
        let is_last = i + 1 >= alignment.len();

        let is_match = op == DiffOp::Match;
        let used_words = op.movement();
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
                let sides = [0, 1].map(|side| {
                    highlighted_subsegments(&texts[side], &section_contents[side][0..current_line_start[side]])
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
            let sides = [0, 1].map(|side| highlighted_subsegments(&texts[side], &section_contents[side]));
            let equal = sides
                .iter()
                .all(|side| side.highlight_first == false && side.highlight_bounds.len() <= 2);
            result.push(Section { sides, equal });
            section_contents = [vec![], vec![]];
            current_line_start = [0, 0];
            section_contains_match = false;
        }
    }
    result
}

fn get_partitioned_subtext<'a>(text: &PartitionedText<'a>, word_range: Range<usize>) -> PartitionedText<'a> {
    PartitionedText {
        text: text.text,
        word_bounds: &text.word_bounds[word_range.start..=word_range.end],
    }
}

pub fn build_diff<'a>(texts: &[PartitionedText<'a>; 2], fragments: Vec<(AlignedFragment, bool)>) -> Diff {
    let mut sections = vec![];
    let mut sections_ranges_from_fragment: Vec<Range<usize>> = vec![];

    for (fragment, _is_main) in fragments.iter() {
        let parts = [
            get_partitioned_subtext(&texts[0], fragment.starts[0]..fragment.ends[0]),
            get_partitioned_subtext(&texts[1], fragment.starts[1]..fragment.ends[1]),
        ];
        let old_len = sections.len();
        sections.append(&mut make_sections(&parts, &fragment.alignment));
        sections_ranges_from_fragment.push(old_len..sections.len());
    }

    let mut file_ops = vec![];

    let make_unaligned_sections =
        |side: usize, word_range: Range<usize>, sections: &mut Vec<Section>, file_ops: &mut Vec<(DiffOp, usize)>| {
            let mut parts: [PartitionedText; 2] = Default::default();
            parts[side] = get_partitioned_subtext(&texts[side], word_range.clone());
            let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
            let indel_alignment = vec![indel_op; word_range.len()];
            let mut indel_sections = make_sections(&parts, &indel_alignment);
            for section_id in sections.len()..sections.len() + indel_sections.len() {
                file_ops.push((indel_op, section_id));
            }
            sections.append(&mut indel_sections);
        };

    let add_fragment_sections =
        |fragment_id: usize, fragment_op: DiffOp, sections: &Vec<Section>, file_ops: &mut Vec<(DiffOp, usize)>| {
            for section_id in sections_ranges_from_fragment[fragment_id].clone() {
                let section = &sections[section_id];
                let mut relevant_sides = fragment_op.movement();
                for side in 0..2 {
                    if section.sides[side].highlight_bounds.is_empty() {
                        relevant_sides[side] = 0;
                    }
                }
                let section_op = match relevant_sides {
                    [1, 1] => DiffOp::Match,
                    [1, 0] => DiffOp::Delete,
                    [0, 1] => DiffOp::Insert,
                    [0, 0] => {
                        continue;
                    }
                    _ => unreachable!(),
                };
                file_ops.push((section_op, section_id));
            }
        };

    let mut ids_by_start = [vec![], vec![]];
    for side in 0..2 {
        for (id, (fragment, _is_main)) in fragments.iter().enumerate() {
            ids_by_start[side].push((fragment.starts[side], id));
        }
        ids_by_start[side].sort();
    }

    let mut word_indices = [0, 0];
    let mut fragment_indices = [0, 0];
    loop {
        for side in 0..2 {
            while fragment_indices[side] < ids_by_start[side].len()
                && !fragments[ids_by_start[side][fragment_indices[side]].1].1
            {
                let fragment_id = ids_by_start[side][fragment_indices[side]].1;
                let fragment = &fragments[fragment_id].0;
                if word_indices[side] < fragment.starts[side] {
                    make_unaligned_sections(
                        side,
                        word_indices[side]..fragment.starts[side],
                        &mut sections,
                        &mut file_ops,
                    );
                }
                let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
                add_fragment_sections(fragment_id, indel_op, &sections, &mut file_ops);
                word_indices[side] = fragment.ends[side];
                fragment_indices[side] += 1;
            }
        }
        for side in 0..2 {
            let next_fragment_start = if fragment_indices[side] < ids_by_start[side].len() {
                fragments[ids_by_start[side][fragment_indices[side]].1].0.starts[side]
            } else {
                texts[side].word_count()
            };
            if word_indices[side] < next_fragment_start {
                make_unaligned_sections(
                    side,
                    word_indices[side]..next_fragment_start,
                    &mut sections,
                    &mut file_ops,
                );
            }
        }
        if fragment_indices[0] >= ids_by_start[0].len() {
            assert!(fragment_indices[1] >= ids_by_start[1].len());
            break;
        }
        let fragment_id = ids_by_start[0][fragment_indices[0]].1;
        assert!(ids_by_start[1][fragment_indices[1]].1 == fragment_id);
        add_fragment_sections(fragment_id, DiffOp::Match, &sections, &mut file_ops);
        word_indices[0] = fragments[fragment_id].0.ends[0];
        word_indices[1] = fragments[fragment_id].0.ends[1];
        fragment_indices[0] += 1;
        fragment_indices[1] += 1;
    }

    let file_diff = FileDiff { ops: file_ops };

    Diff {
        sections,
        files: vec![file_diff],
    }
}
