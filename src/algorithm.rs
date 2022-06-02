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

fn line_start_positions(string: &str) -> Vec<usize> {
    let mut result = vec![0];
    for (i, c) in string.chars().enumerate() {
        if c == '\n' {
            result.push(i + 1);
        }
    }
    if *result.last().unwrap() != string.len() {
        result.push(string.len())
    }
    result
}

pub fn diff_file<'a>(old: &'a str, new: &'a str) -> FileDiff<'a> {
    let mut interner = StringInterner::default();
    let old_line_starts = line_start_positions(old);
    let mut old_symbols = Vec::new();
    for i in 0..old_line_starts.len() - 1 {
        old_symbols.push(interner.get_or_intern(&old[old_line_starts[i]..old_line_starts[i + 1]]));
    }
    let new_line_starts = line_start_positions(new);
    let mut new_symbols = Vec::new();
    for i in 0..new_line_starts.len() - 1 {
        new_symbols.push(interner.get_or_intern(&new[new_line_starts[i]..new_line_starts[i + 1]]));
    }
    let diff_result = diff(&old_symbols[..], &new_symbols[..]);

    let mut result = FileDiff { 0: vec![] };

    let mut in_match = diff_result[0] == DiffOp::Match;
    let mut old_index = 0;
    let mut new_index = 0;

    let mut section = Section {
        old: SectionSide {
            lineno_initial: old_index,
            text_with_words: vec![],
        },
        new: SectionSide {
            lineno_initial: new_index,
            text_with_words: vec![],
        },
        equal: in_match,
    };

    for (i, op) in diff_result.iter().enumerate() {
        match op {
            DiffOp::Delete => {
                old_index += 1;
            }
            DiffOp::Insert => {
                new_index += 1;
            }
            DiffOp::Match => {
                old_index += 1;
                new_index += 1;
            }
        }
        if i + 1 >= diff_result.len() || (diff_result[i + 1] == DiffOp::Match) != in_match {
            let old_text =
                &old[old_line_starts[section.old.lineno_initial]..old_line_starts[old_index]];
            if old_text.len() > 0 {
                section.old.text_with_words.push((!in_match, old_text));
            }
            let new_text =
                &new[new_line_starts[section.new.lineno_initial]..new_line_starts[new_index]];
            if new_text.len() > 0 {
                section.new.text_with_words.push((!in_match, new_text));
            }
            result.0.push(section);
            in_match = !in_match;
            section = Section {
                old: SectionSide {
                    lineno_initial: old_index,
                    text_with_words: vec![],
                },
                new: SectionSide {
                    lineno_initial: new_index,
                    text_with_words: vec![],
                },
                equal: in_match,
            };
        }
    }
    result
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum DiffOp {
    Match,
    Insert,
    Delete,
}

fn diff(
    old: &[string_interner::symbol::SymbolU32],
    new: &[string_interner::symbol::SymbolU32],
) -> Vec<DiffOp> {
    let inf = old.len() + new.len() + 1;
    let mut dp: Vec<Vec<(usize, Option<DiffOp>)>> =
        vec![vec![(inf, None); new.len() + 1]; old.len() + 1];
    dp[old.len()][new.len()] = (0, None);
    for old_index in (0..old.len()).rev() {
        dp[old_index][new.len()] = (1 + dp[old_index + 1][new.len()].0, Some(DiffOp::Delete));
    }
    for new_index in (0..new.len()).rev() {
        dp[old.len()][new_index] = (1 + dp[old.len()][new_index + 1].0, Some(DiffOp::Insert));
    }
    for old_index in (0..old.len()).rev() {
        for new_index in (0..new.len()).rev() {
            let proposed_down = dp[old_index + 1][new_index].0 + 1;
            if dp[old_index][new_index].0 > proposed_down {
                dp[old_index][new_index] = (proposed_down, Some(DiffOp::Delete));
            }
            let proposed_right = dp[old_index][new_index + 1].0 + 1;
            if dp[old_index][new_index].0 > proposed_right {
                dp[old_index][new_index] = (proposed_right, Some(DiffOp::Insert));
            }
            let proposed_diagonal = dp[old_index + 1][new_index + 1].0;
            if old[old_index] == new[new_index] && dp[old_index][new_index].0 > proposed_diagonal {
                dp[old_index][new_index] = (proposed_diagonal, Some(DiffOp::Match));
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

    fn line_starts<'a>(string: &str) -> (Vec<&str>, Vec<usize>) {
        let mut lines = Vec::new();
        let mut line_starts = Vec::new();
        let mut line_start = 0;
        for (i, c) in string.chars().enumerate() {
            if c == '\n' {
                lines.push(&string[line_start..i + 1]);
                line_starts.push(line_start);
                line_start = i + 1;
            }
        }
        if line_start < string.len() {
            lines.push(&string[line_start..string.len()]);
        }
        line_starts.push(string.len());
        (lines, line_starts)
    }

    #[test]
    fn simple() {
        let old = "a\n\
                   b\n\
                   c\n\
                   d\n\
                   e\n";
        let new = "a\n\
                   ba\n\
                   c\n\
                   c2\n";
        let (old_lines, old_starts) = line_starts(old);
        let (new_lines, new_starts) = line_starts(new);
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![
                Section {
                    old: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, old_lines[0])],
                    },
                    new: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, new_lines[0])],
                    },
                    equal: true,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(true, old_lines[1])],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(true, new_lines[1])],
                    },
                    equal: false,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 2,
                        text_with_words: vec![(false, old_lines[2])],
                    },
                    new: SectionSide {
                        lineno_initial: 2,
                        text_with_words: vec![(false, new_lines[2])],
                    },
                    equal: true,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 3,
                        text_with_words: vec![(true, &old[old_starts[3]..old_starts[5]])],
                    },
                    new: SectionSide {
                        lineno_initial: 3,
                        text_with_words: vec![(true, new_lines[3])],
                    },
                    equal: false,
                },
            ],
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn pure_delete() {
        let old = "a\n\
                   b\n\
                   c\n\
                   d\n\
                   e\n";
        let new = "a\n\
                   e\n";
        let (old_lines, old_starts) = line_starts(old);
        let (new_lines, new_starts) = line_starts(new);
        let actual = diff_file(old, new);
        let expected = FileDiff {
            0: vec![
                Section {
                    old: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, old_lines[0])],
                    },
                    new: SectionSide {
                        lineno_initial: 0,
                        text_with_words: vec![(false, new_lines[0])],
                    },
                    equal: true,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(true, &old[old_starts[1]..old_starts[4]])],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![],
                    },
                    equal: false,
                },
                Section {
                    old: SectionSide {
                        lineno_initial: 4,
                        text_with_words: vec![(false, old_lines[4])],
                    },
                    new: SectionSide {
                        lineno_initial: 1,
                        text_with_words: vec![(false, new_lines[1])],
                    },
                    equal: true,
                },
            ],
        };
        assert_eq!(expected, actual);
    }
}
