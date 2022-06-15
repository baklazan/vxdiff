use string_interner::StringInterner;

#[derive(Debug, PartialEq)]
pub struct FileDiff<'a>(pub Vec<Section<'a>>);

#[derive(Debug, PartialEq)]
pub struct Section<'a> {
    pub sides: [SectionSide<'a>; 2],
    pub equal: bool,
}

#[derive(Debug, PartialEq)]
pub struct SectionSide<'a> {
    pub text_with_words: Vec<(bool, &'a str)>,
    // pub moved: Option<(String, usize)>,
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

fn align_words(texts: &[PartitionedText; 2]) -> Vec<DiffOp> {
    let mut interner = StringInterner::default();
    let mut symbols = [Vec::new(), Vec::new()];
    for side in 0..2 {
        for i in 0..texts[side].word_count() {
            let word = texts[side].get_word(i);
            symbols[side].push(interner.get_or_intern(word));
        }
    }
    return align(&symbols[0], &symbols[1]);
}

struct PartitionedText<'a> {
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

fn word_diff_chunk<'a>(old: &'a str, new: &'a str) -> Vec<Section<'a>> {
    let texts = [partition_into_words(old), partition_into_words(new)];
    let alignment = align_words(&texts);
    make_sections(&texts, &alignment)
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

pub fn diff_file<'a>(old: &'a str, new: &'a str) -> FileDiff<'a> {
    FileDiff(word_diff_chunk(old, new))
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum DiffOp {
    Match,
    Insert,
    Delete,
}

fn align(old: &[string_interner::symbol::SymbolU32], new: &[string_interner::symbol::SymbolU32]) -> Vec<DiffOp> {
    let inf = old.len() + new.len() + 1;
    let mut dp: Vec<Vec<(usize, Option<DiffOp>)>> = vec![vec![(inf, None); new.len() + 1]; old.len() + 1];
    dp[old.len()][new.len()] = (0, None);
    for old_index in (0..old.len()).rev() {
        dp[old_index][new.len()] = (1 + dp[old_index + 1][new.len()].0, Some(DiffOp::Delete));
    }
    for new_index in (0..new.len()).rev() {
        dp[old.len()][new_index] = (1 + dp[old.len()][new_index + 1].0, Some(DiffOp::Insert));
    }
    for old_index in (0..old.len()).rev() {
        for new_index in (0..new.len()).rev() {
            let is_match = old[old_index] == new[new_index];
            for (score, diffop, valid) in [
                (dp[old_index + 1][new_index + 1].0 + 1, DiffOp::Match, is_match),
                (dp[old_index + 1][new_index].0 + 1, DiffOp::Delete, true),
                (dp[old_index][new_index + 1].0 + 1, DiffOp::Insert, true),
            ] {
                if dp[old_index][new_index].0 > score && valid {
                    dp[old_index][new_index] = (score, Some(diffop));
                }
            }
        }
    }
    let mut result = Vec::<DiffOp>::new();
    let mut old_index = 0;
    let mut new_index = 0;
    while old_index < old.len() || new_index < new.len() {
        result.push(dp[old_index][new_index].1.unwrap());
        match dp[old_index][new_index].1.unwrap() {
            DiffOp::Insert => {
                new_index += 1;
            }
            DiffOp::Delete => {
                old_index += 1;
            }
            DiffOp::Match => {
                new_index += 1;
                old_index += 1;
            }
        }
    }
    result
}

#[cfg(test)]
mod test {

    use super::*;

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

    #[test]
    fn pure_delete() {
        let old = "a\n\
                   b\n\
                   c\n\
                   d\n";
        let new = "a\n\
                   d\n";
        let actual = diff_file(old, new);
        let expected = FileDiff(vec![
            make_section(vec![(false, "a\n")], vec![(false, "a\n")], true),
            make_section(vec![(true, "b\nc\n")], vec![], false),
            make_section(vec![(false, "d\n")], vec![(false, "d\n")], true),
        ]);
        assert_eq!(expected, actual);
    }

    #[test]
    fn line_split() {
        let old = "... :::\n";
        let new = "...\n\
                   :::\n";
        let actual = diff_file(old, new);
        let expected = FileDiff(vec![make_section(
            vec![(false, "..."), (true, " "), (false, ":::\n")],
            vec![(false, "..."), (true, "\n"), (false, ":::\n")],
            false,
        )]);
        assert_eq!(expected, actual);
    }

    #[test]
    fn deletion_before_modified_line() {
        let old = "...\n\
                   [[[ ::: ]]]\n";
        let new = "[[[ $$$ ]]]\n";
        let actual = diff_file(old, new);
        let expected = FileDiff(vec![
            make_section(vec![(true, "...\n")], vec![], false),
            make_section(
                vec![(false, "[[[ "), (true, ":::"), (false, " ]]]\n")],
                vec![(false, "[[[ "), (true, "$$$"), (false, " ]]]\n")],
                false,
            ),
        ]);
        assert_eq!(expected, actual);
    }

    #[test]
    fn line_split_with_insert() {
        let old = "... :::\n";
        let new = "...\n\
                   $$$\n\
                   :::\n";
        let actual = diff_file(old, new);
        let expected = FileDiff(vec![make_section(
            vec![(false, "..."), (true, " "), (false, ":::\n")],
            vec![(false, "..."), (true, "\n$$$\n"), (false, ":::\n")],
            false,
        )]);
        assert_eq!(expected, actual);
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
