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

/*
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
}*/

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
