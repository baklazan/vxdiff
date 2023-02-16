use super::{get_partitioned_subtext, AlignedFragment, Diff, DiffOp, FileDiff, PartitionedText, Section, SectionSide};
use std::ops::Range;

fn highlighted_subsegments(
    text_words: &PartitionedText,
    word_indices_and_highlight: &[(bool, usize)],
    file_id: usize,
) -> SectionSide {
    for i in 1..word_indices_and_highlight.len() {
        assert_eq!(word_indices_and_highlight[i - 1].1 + 1, word_indices_and_highlight[i].1);
    }

    let first_word_index = word_indices_and_highlight[0].1;
    let last_word_index = word_indices_and_highlight.last().unwrap().1;

    let mut highlight_ranges = vec![];
    let mut current_highlight_from = None;
    let mut only_whitespace_is_highlighted = true;
    let mut only_leading_whitespace_is_highlighted = true;

    let sentinel = (false, last_word_index + 1);
    for &(highlight, word_index) in word_indices_and_highlight.iter().chain(std::iter::once(&sentinel)) {
        match (highlight, current_highlight_from) {
            (true, Some(_)) => {}
            (true, None) => current_highlight_from = Some(word_index),
            (false, Some(start_index)) => {
                let range = text_words.part_bounds[start_index]..text_words.part_bounds[word_index];
                let highlighted_content = &text_words.text[range.clone()];
                if !highlighted_content.chars().all(|c| c.is_whitespace() && c != '\n') {
                    only_whitespace_is_highlighted = false;
                    only_leading_whitespace_is_highlighted = false;
                }
                if start_index != first_word_index {
                    only_leading_whitespace_is_highlighted = false;
                }
                highlight_ranges.push(range);
                current_highlight_from = None;
            }
            (false, None) => {}
        }
    }

    SectionSide {
        file_id,
        byte_range: text_words.part_bounds[first_word_index]..text_words.part_bounds[last_word_index + 1],
        highlight_ranges,
        only_whitespace_is_highlighted,
        only_leading_whitespace_is_highlighted,
    }
}

fn make_sections(
    text_words: &[PartitionedText; 2],
    file_ids: [usize; 2],
    alignment: &[DiffOp],
    is_main: bool,
) -> Vec<Section> {
    let mut result = vec![];

    let mut section_contents: [Vec<(bool, usize)>; 2] = [vec![], vec![]];
    let mut word_indices = [0, 0];
    let mut section_contains_match = false;
    let mut current_line_start = [0, 0]; // indices into section_contents

    for (i, &op) in alignment.iter().enumerate() {
        let is_last = i + 1 >= alignment.len();

        let is_match = op == DiffOp::Match;

        if is_match && !section_contains_match {
            for indel_side in 0..2 {
                if current_line_start[indel_side] != 0 {
                    let sides = [0, 1].map(|side| {
                        if side == indel_side {
                            Some(highlighted_subsegments(
                                &text_words[side],
                                &section_contents[side][0..current_line_start[side]],
                                file_ids[side],
                            ))
                        } else if is_main {
                            None
                        } else {
                            let start_word_index = if section_contents[side].is_empty() {
                                word_indices[side]
                            } else {
                                section_contents[side][0].1
                            };
                            let byte_index = text_words[side].part_bounds[start_word_index];
                            Some(SectionSide {
                                file_id: file_ids[side],
                                byte_range: byte_index..byte_index,
                                highlight_ranges: vec![],
                                only_whitespace_is_highlighted: true,
                                only_leading_whitespace_is_highlighted: true,
                            })
                        }
                    });
                    result.push(Section { sides });
                    section_contents[indel_side].drain(0..current_line_start[indel_side]);
                    current_line_start[indel_side] = 0;
                }
            }
        }
        section_contains_match |= is_match;

        let mut is_newline = false;
        for side in 0..2 {
            for _i in 0..op.movement()[side] {
                is_newline = text_words[side].get_part(word_indices[side]) == "\n";
                section_contents[side].push((!is_match, word_indices[side]));
                word_indices[side] += 1;
                if is_newline {
                    current_line_start[side] = section_contents[side].len();
                }
            }
        }

        if (is_match && is_newline) || is_last {
            let sides = [0, 1].map(|side| {
                if section_contents[side].is_empty() {
                    if is_main {
                        None
                    } else {
                        let byte_index = text_words[side].part_bounds[word_indices[side]];
                        Some(SectionSide {
                            file_id: file_ids[side],
                            byte_range: byte_index..byte_index,
                            highlight_ranges: vec![],
                            only_whitespace_is_highlighted: true,
                            only_leading_whitespace_is_highlighted: true,
                        })
                    }
                } else {
                    Some(highlighted_subsegments(
                        &text_words[side],
                        &section_contents[side],
                        file_ids[side],
                    ))
                }
            });
            result.push(Section { sides });
            section_contents = [vec![], vec![]];
            current_line_start = [0, 0];
            section_contains_match = false;
        }
    }
    result
}

pub(super) fn build_diff(texts_words: &[[PartitionedText; 2]], fragments: Vec<(AlignedFragment, bool)>) -> Diff {
    let mut sections = vec![];
    let mut sections_ranges_from_fragment: Vec<Range<usize>> = vec![];

    for (fragment, is_main) in fragments.iter() {
        let parts = [
            get_partitioned_subtext(
                &texts_words[fragment.file_ids[0]][0],
                fragment.starts[0]..fragment.ends[0],
            ),
            get_partitioned_subtext(
                &texts_words[fragment.file_ids[1]][1],
                fragment.starts[1]..fragment.ends[1],
            ),
        ];
        let old_len = sections.len();
        sections.append(&mut make_sections(
            &parts,
            fragment.file_ids,
            &fragment.alignment,
            *is_main,
        ));
        sections_ranges_from_fragment.push(old_len..sections.len());
    }

    let make_unaligned_sections = |side: usize,
                                   file_id: usize,
                                   word_range: Range<usize>,
                                   sections: &mut Vec<Section>,
                                   file_ops: &mut Vec<(DiffOp, usize)>| {
        let mut parts: [PartitionedText; 2] = Default::default();
        parts[side] = get_partitioned_subtext(&texts_words[file_id][side], word_range.clone());
        let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
        let indel_alignment = vec![indel_op; word_range.len()];
        let mut indel_sections = make_sections(&parts, [file_id; 2], &indel_alignment, true);
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
                    if section.sides[side].is_none() {
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

    let mut fragment_orders = vec![[vec![], vec![]]; texts_words.len()];
    for side in 0..2 {
        for (id, (fragment, _is_main)) in fragments.iter().enumerate() {
            let file_id = fragment.file_ids[side];
            fragment_orders[file_id][side].push((fragment.starts[side], id));
        }
    }
    for fragment_order in fragment_orders.iter_mut() {
        for fragment_order_side in fragment_order {
            fragment_order_side.sort()
        }
    }

    let mut file_diffs = vec![];

    for file_id in 0..texts_words.len() {
        let mut file_ops = vec![];
        let mut word_indices = [0, 0];
        let mut fragment_order_indices = [0, 0];
        let fragment_order = &fragment_orders[file_id];
        loop {
            for side in 0..2 {
                let fragment_order_side = &fragment_order[side];
                while fragment_order_indices[side] < fragment_order_side.len()
                    && !fragments[fragment_order_side[fragment_order_indices[side]].1].1
                {
                    let fragment_id = fragment_order_side[fragment_order_indices[side]].1;
                    let fragment = &fragments[fragment_id].0;
                    assert!(fragment.file_ids[side] == file_id);
                    if word_indices[side] < fragment.starts[side] {
                        make_unaligned_sections(
                            side,
                            file_id,
                            word_indices[side]..fragment.starts[side],
                            &mut sections,
                            &mut file_ops,
                        );
                    }
                    let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
                    add_fragment_sections(fragment_id, indel_op, &sections, &mut file_ops);
                    word_indices[side] = fragment.ends[side];
                    fragment_order_indices[side] += 1;
                }
            }
            for side in 0..2 {
                let fragment_order_side = &fragment_order[side];
                let next_fragment_start = if fragment_order_indices[side] < fragment_order[side].len() {
                    fragments[fragment_order_side[fragment_order_indices[side]].1].0.starts[side]
                } else {
                    texts_words[file_id][side].part_count()
                };
                if word_indices[side] < next_fragment_start {
                    make_unaligned_sections(
                        side,
                        file_id,
                        word_indices[side]..next_fragment_start,
                        &mut sections,
                        &mut file_ops,
                    );
                }
            }
            if fragment_order_indices[0] >= fragment_order[0].len() {
                assert!(fragment_order_indices[1] >= fragment_order[1].len());
                break;
            }
            let fragment_id = fragment_order[0][fragment_order_indices[0]].1;
            if fragment_order[1][fragment_order_indices[1]].1 != fragment_id {
                println!(
                    "old side fragment id: {}, new side: {}",
                    fragment_id, fragment_order[1][fragment_order_indices[1]].1
                );
            }
            assert!(fragment_order[1][fragment_order_indices[1]].1 == fragment_id);
            add_fragment_sections(fragment_id, DiffOp::Match, &sections, &mut file_ops);
            word_indices[0] = fragments[fragment_id].0.ends[0];
            word_indices[1] = fragments[fragment_id].0.ends[1];
            fragment_order_indices[0] += 1;
            fragment_order_indices[1] += 1;
        }
        file_diffs.push(FileDiff { ops: file_ops });
    }

    Diff {
        sections,
        files: file_diffs,
    }
}
