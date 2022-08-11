use super::{AlignedFragment, Diff, DiffOp, FileDiff, PartitionedText, Section, SectionSide};

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

pub fn build_diff<'a>(texts: &[PartitionedText<'a>; 2], fragments: Vec<(AlignedFragment, bool)>) -> Diff<'a> {
    let mut first_section_from_fragment: Vec<usize> = vec![];
    let mut sections_count_from_fragment: Vec<usize> = vec![];
    let mut sections = vec![];

    for (fragment, _is_main) in fragments.iter() {
        for side in 0..2 {
            if texts[side].get_word(fragment.ends[side] - 1) != "\n" {
                println!(
                    "Bad fragment side:\n{}",
                    &texts[side].text
                        [texts[side].word_bounds[fragment.starts[side]]..texts[side].word_bounds[fragment.ends[side]]]
                );
            }
        }
        let parts = [
            get_partitioned_subtext(&texts[0], fragment.starts[0], fragment.ends[0]),
            get_partitioned_subtext(&texts[1], fragment.starts[1], fragment.ends[1]),
        ];
        first_section_from_fragment.push(sections.len());
        let mut sections_from_this_fragment = make_sections(&parts, &fragment.alignment);
        sections_count_from_fragment.push(sections_from_this_fragment.len());
        sections.append(&mut sections_from_this_fragment);
    }

    let mut file_ops = vec![];

    let make_unaligned_sections = |side: usize,
                                   word_range: (usize, usize),
                                   sections: &mut Vec<Section<'a>>,
                                   file_ops: &mut Vec<(DiffOp, usize)>| {
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
        parts[side] = get_partitioned_subtext(&texts[side], word_range.0, word_range.1);
        let indel_op = [DiffOp::Delete, DiffOp::Insert][side];
        let indel_alignment = vec![indel_op; word_range.1 - word_range.0];
        let mut indel_sections = make_sections(&parts, &indel_alignment);
        for section_id in sections.len()..sections.len() + indel_sections.len() {
            file_ops.push((indel_op, section_id));
        }
        sections.append(&mut indel_sections);
    };

    let add_fragment_sections =
        |fragment_id: usize, fragment_op: DiffOp, sections: &Vec<Section<'a>>, file_ops: &mut Vec<(DiffOp, usize)>| {
            for section_id in first_section_from_fragment[fragment_id]
                ..first_section_from_fragment[fragment_id] + sections_count_from_fragment[fragment_id]
            {
                let section = &sections[section_id];
                let mut relevant_sides = fragment_op.movement();
                for side in 0..2 {
                    if section.sides[side].text_with_words.is_empty() {
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
                        (word_indices[side], fragment.starts[side]),
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
                    (word_indices[side], next_fragment_start),
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

    for section in sections.iter() {
        let mut is_weird = false;
        for side in 0..2 {
            let words = &section.sides[side].text_with_words;
            if !words.is_empty() && !words.last().unwrap().1.ends_with("\n") {
                is_weird = true;
            }
        }
        if is_weird {
            println!("left:");
            for word in section.sides[0].text_with_words.iter() {
                print!("{}", word.1);
            }
            println!();
            println!("right:");
            for word in section.sides[1].text_with_words.iter() {
                print!("{}", word.1);
            }
            println!();
        }
    }

    Diff {
        sections,
        files: vec![file_diff],
    }
}

#[cfg(test)]
mod test {
    use crate::algorithm::{postprocess::make_sections, DiffOp, PartitionedText, Section, SectionSide};

    fn make_section<'a>(old_text: Vec<(bool, &'a str)>, new_text: Vec<(bool, &'a str)>, equal: bool) -> Section<'a> {
        Section {
            sides: [
                SectionSide {
                    text_with_words: old_text,
                },
                SectionSide {
                    text_with_words: new_text,
                },
            ],
            equal: equal,
        }
    }

    fn partitioned_text_from_list<'a>(list: &[&str], string_storage: &'a mut String) -> PartitionedText<'a> {
        let mut word_bounds = vec![0];
        for word in list {
            string_storage.push_str(word);
            word_bounds.push(string_storage.len());
        }
        PartitionedText {
            text: string_storage,
            word_bounds,
        }
    }

    #[test]
    fn long_mismatch_section() {
        let mut old_storage = String::new();
        let mut new_storage = String::new();
        let old = partitioned_text_from_list(&["a", "\n", "b", "\n", "x", "z"], &mut old_storage);
        let new = partitioned_text_from_list(&["c", "\n", "d", "\n", "e", "\n", "y", "z"], &mut new_storage);
        use DiffOp::*;
        let alignment = &[
            Delete, Delete, Delete, Delete, Delete, Insert, Insert, Insert, Insert, Insert, Insert, Insert, Match,
        ];
        let actual = make_sections(&[old, new], alignment);
        let expected = vec![
            make_section(vec![(true, "a\nb\n")], vec![(true, "c\nd\ne\n")], false),
            make_section(vec![(true, "x"), (false, "z")], vec![(true, "y"), (false, "z")], false),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn match_after_long_mismatch_section() {
        let mut old_storage = String::new();
        let mut new_storage = String::new();
        let old = partitioned_text_from_list(&["a", "\n", "b", "\n", "z"], &mut old_storage);
        let new = partitioned_text_from_list(&["c", "\n", "d", "\n", "e", "\n", "z"], &mut new_storage);
        use DiffOp::*;
        let alignment = &[
            Delete, Delete, Delete, Delete, Insert, Insert, Insert, Insert, Insert, Insert, Match,
        ];
        let actual = make_sections(&[old, new], alignment);
        let expected = vec![
            make_section(vec![(true, "a\nb\n")], vec![(true, "c\nd\ne\n")], false),
            make_section(vec![(false, "z")], vec![(false, "z")], true),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn section_with_only_newline_matching() {
        let mut old_storage = String::new();
        let mut new_storage = String::new();
        let old = partitioned_text_from_list(&["a", "\n", "b", "\n"], &mut old_storage);
        let new = partitioned_text_from_list(&["c", "\n", "d", "\n", "e", "\n"], &mut new_storage);
        use DiffOp::*;
        let alignment = &[Delete, Delete, Delete, Insert, Insert, Insert, Insert, Insert, Match];
        let actual = make_sections(&[old, new], alignment);
        let expected = vec![make_section(
            vec![(true, "a\nb"), (false, "\n")],
            vec![(true, "c\nd\ne"), (false, "\n")],
            false,
        )];
        assert_eq!(expected, actual);
    }
}
