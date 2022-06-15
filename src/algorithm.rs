use string_interner::StringInterner;

#[derive(Debug, PartialEq)]
pub struct FileDiff<'a>(pub Vec<Section<'a>>);

#[derive(Debug, PartialEq)]
pub struct Section<'a> {
    pub old: SectionSide<'a>,
    pub new: SectionSide<'a>,
    pub equal: bool,
}

#[derive(Debug, PartialEq)]
pub struct SectionSide<'a> {
    pub lineno_initial: usize,
    pub text_with_words: Vec<(bool, &'a str)>,
    // pub moved: Option<(String, usize)>,
}

impl<'a> Section<'a> {
    fn new(old_lineno: usize, new_lineno: usize) -> Section<'a> {
        Section {
            old: SectionSide {
                lineno_initial: old_lineno,
                text_with_words: Vec::new(),
            },
            new: SectionSide {
                lineno_initial: new_lineno,
                text_with_words: Vec::new(),
            },
            equal: true,
        }
    }
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

fn word_diff_chunk<'a>(
    old: &'a str,
    old_lineno_initial: usize,
    new: &'a str,
    new_lineno_initial: usize,
) -> Vec<Section<'a>> {
    const OLD: usize = 0;
    const NEW: usize = 1;
    let texts = [old, new];
    let word_bounds = [word_bounds_bytes(old), word_bounds_bytes(new)];
    let alignment = align_words(&texts, &word_bounds);

    let mut result = Vec::new();

    let mut section_contents: [Vec<(bool, usize)>; 2] = [Vec::new(), Vec::new()];
    let mut word_indices = [0, 0];
    let mut line_numbers = [old_lineno_initial, new_lineno_initial];
    let mut section = Section::new(old_lineno_initial, new_lineno_initial);
    let mut section_contains_match = false;

    for (i, op) in alignment.iter().enumerate() {
        let mut should_push_both = i + 1 >= alignment.len();

        let (used_words, is_match) = match op {
            DiffOp::Delete => ([1, 0], false),
            DiffOp::Insert => ([0, 1], false),
            DiffOp::Match => ([1, 1], true),
        };

        section.equal &= is_match;
        section_contains_match |= is_match;

        for side in 0..=1 {
            for _i in 0..used_words[side] {
                let word_start = word_bounds[side][word_indices[side]];
                let word_end = word_bounds[side][word_indices[side] + 1];
                let word = &texts[side][word_start..word_end];
                section_contents[side].push((!is_match, word_indices[side]));
                word_indices[side] += 1;
                if word == "\n" {
                    line_numbers[side] += 1;

                    if !is_match && !section_contains_match {
                        let section_side = if side == OLD {
                            &mut section.old
                        } else {
                            &mut section.new
                        };
                        section_side.text_with_words = highlighted_subsegments(
                            texts[side],
                            &word_bounds[side],
                            std::mem::take(&mut section_contents[side]),
                        );
                        result.push(section);
                        section = Section::new(line_numbers[OLD], line_numbers[NEW]);
                        section_contains_match = false;
                        should_push_both = false;
                    }

                    if is_match {
                        should_push_both = true;
                    }
                }
            }
        }

        if should_push_both {
            section.old.text_with_words =
                highlighted_subsegments(old, &word_bounds[OLD], std::mem::take(&mut section_contents[OLD]));
            section.new.text_with_words =
                highlighted_subsegments(new, &word_bounds[NEW], std::mem::take(&mut section_contents[NEW]));
            result.push(section);
            section = Section::new(line_numbers[OLD], line_numbers[NEW]);
            section_contains_match = false;
        }
    }
    result
}

pub fn diff_file<'a>(old: &'a str, new: &'a str) -> FileDiff<'a> {
    FileDiff(word_diff_chunk(old, 0, new, 0))
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

    #[test]
    fn pure_delete() {
        let old = "a\n\
                   b\n\
                   c\n\
                   d\n";
        let new = "a\n\
                   d\n";
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![
                Section {
                    old: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, "a\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, "a\n")],
                    },
                    equal: true,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(true, "b\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![],
                    },
                    equal: false,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 2,
                        text_with_words: vec![(true, "c\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![],
                    },
                    equal: false,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 3,
                        text_with_words: vec![(false, "d\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(false, "d\n")],
                    },
                    equal: true,
                },
            ],
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn line_split() {
        let old = "... :::\n";
        let new = "...\n\
                   :::\n";
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![Section {
                old: SectionSide {
                    lineno_initial: 0,
                    text_with_words: vec![(false, "..."), (true, " "), (false, ":::\n")],
                },
                new: SectionSide {
                    lineno_initial: 0,
                    text_with_words: vec![(false, "..."), (true, "\n"), (false, ":::\n")],
                },
                equal: false,
            }],
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn deletion_before_modified_line() {
        let old = "...\n\
                   [[[ ::: ]]]\n";
        let new = "[[[ $$$ ]]]\n";
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![
                Section {
                    old: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(true, "...\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![],
                    },
                    equal: false,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(false, "[[[ "), (true, ":::"), (false, " ]]]\n")],
                    },
                    new: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, "[[[ "), (true, "$$$"), (false, " ]]]\n")],
                    },
                    equal: false,
                },
            ],
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn line_split_with_insert() {
        let old = "... :::\n";
        let new = "...\n\
                   $$$\n\
                   :::\n";
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![Section {
                old: SectionSide {
                    lineno_initial: 0,
                    text_with_words: vec![(false, "..."), (true, " "), (false, ":::\n")],
                },
                new: SectionSide {
                    lineno_initial: 0,
                    text_with_words: vec![(false, "..."), (true, "\n$$$\n"), (false, ":::\n")],
                },
                equal: false,
            }],
        };
        assert_eq!(expected, actual);
    }
}
