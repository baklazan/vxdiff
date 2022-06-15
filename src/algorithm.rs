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

fn word_bounds_bytes(string: &str) -> Vec<usize> {
    let mut result = Vec::new();
    let mut was_last_alphabetic = false;
    let mut was_last_numeric = false;
    for (i, c) in string.char_indices() {
        if c.is_alphabetic() && was_last_alphabetic {
            continue;
        }
        if c.is_numeric() && was_last_numeric {
            continue;
        }
        was_last_alphabetic = c.is_alphabetic();
        was_last_numeric = c.is_numeric();
        result.push(i);
    }
    result.push(string.len());
    result
}

fn align_words(texts: &[&str; 2], word_bounds: &[Vec<usize>; 2]) -> Vec<DiffOp> {
    let mut interner = StringInterner::default();
    let mut symbols = [Vec::new(), Vec::new()];
    for side in 0..2 {
        for i in 0..word_bounds[side].len() - 1 {
            let word_start = word_bounds[side][i];
            let word_end = word_bounds[side][i + 1];
            let word = &texts[side][word_start..word_end];
            symbols[side].push(interner.get_or_intern(word));
        }
    }
    return align(&symbols[0], &symbols[1]);
}

fn highlighted_subsegments<'a>(
    string: &'a str,
    word_bounds: &[usize],
    word_indices_and_highlight: Vec<(bool, usize)>,
) -> Vec<(bool, &'a str)> {
    if word_indices_and_highlight.is_empty() {
        return Vec::new();
    }
    let mut result = Vec::new();

    let mut subsegment_starting_word_index = word_indices_and_highlight[0].1;
    for (i, (highlight, word_index)) in word_indices_and_highlight.iter().enumerate() {
        if i + 1 >= word_indices_and_highlight.len() || (*highlight != word_indices_and_highlight[i + 1].0) {
            let word_start_offset = word_bounds[subsegment_starting_word_index];
            let word_end_offset = word_bounds[word_index + 1];
            let word = &string[word_start_offset..word_end_offset];
            result.push((*highlight, word));
            subsegment_starting_word_index = word_index + 1;
        }
    }
    result
}

fn word_diff_chunk<'a>(old: &'a str, new: &'a str) -> Vec<Section<'a>> {
    let texts = [old, new];
    let word_bounds = [word_bounds_bytes(old), word_bounds_bytes(new)];
    let alignment = align_words(&texts, &word_bounds);

    let mut result = Vec::new();

    let mut section_contents: [Vec<(bool, usize)>; 2] = [Vec::new(), Vec::new()];
    let mut word_indices = [0, 0];
    let mut section_contains_match = false;
    let mut is_section_equal = true;

    for (i, op) in alignment.iter().enumerate() {
        let mut should_push_both = i + 1 >= alignment.len();

        let (used_words, is_match) = match op {
            DiffOp::Delete => ([1, 0], false),
            DiffOp::Insert => ([0, 1], false),
            DiffOp::Match => ([1, 1], true),
        };

        is_section_equal &= is_match;
        section_contains_match |= is_match;

        for side in 0..=1 {
            for _i in 0..used_words[side] {
                let word_start = word_bounds[side][word_indices[side]];
                let word_end = word_bounds[side][word_indices[side] + 1];
                let word = &texts[side][word_start..word_end];
                section_contents[side].push((!is_match, word_indices[side]));
                word_indices[side] += 1;

                if word == "\n" {
                    if !is_match && !section_contains_match {
                        let mut sides = [(); 2].map(|_| SectionSide {
                            text_with_words: vec![],
                        });
                        sides[side].text_with_words = highlighted_subsegments(
                            texts[side],
                            &word_bounds[side],
                            std::mem::take(&mut section_contents[side]),
                        );

                        result.push(Section {
                            sides: sides,
                            equal: is_section_equal,
                        });
                        section_contains_match = false;
                        should_push_both = false;
                        is_section_equal = true;
                    }

                    if is_match {
                        should_push_both = true;
                    }
                }
            }
        }

        if should_push_both {
            let sides = [0, 1].map(|side| SectionSide {
                text_with_words: highlighted_subsegments(
                    texts[side],
                    &word_bounds[side],
                    std::mem::take(&mut section_contents[side]),
                ),
            });
            result.push(Section {
                sides: sides,
                equal: is_section_equal,
            });
            section_contains_match = false;
            is_section_equal = true;
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
            make_section(vec![(true, "b\n")], vec![], false),
            make_section(vec![(true, "c\n")], vec![], false),
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
}
