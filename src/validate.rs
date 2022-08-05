use super::algorithm::{Diff, DiffOp, FileDiff};

pub fn validate(diff: &Diff, file_input: &[[&str; 2]]) -> Vec<String> {
    let mut errors = vec![];

    fn side_str(section_id: usize, side: usize) -> String {
        let side_name = ["Left", "Right"][side];
        format!("{side_name} side of section {section_id}")
    }

    let mut used: Vec<[Vec<String>; 2]> = vec![Default::default(); diff.sections.len()];
    let mut at_eof: Vec<[bool; 2]> = vec![Default::default(); diff.sections.len()];
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let mut last = [None; 2];
        for (op_index, &(op, section_id)) in ops.iter().enumerate() {
            // TODO: movement()
            if op != DiffOp::Insert {
                used[section_id][0].push(format!("{op:?} in file #{file_id} at op index #{op_index}"));
                last[0] = Some(section_id);
            }
            if op != DiffOp::Delete {
                used[section_id][1].push(format!("{op:?} in file #{file_id} at op index #{op_index}"));
                last[1] = Some(section_id);
            }
        }
        for side in 0..2 {
            if let Some(section_id) = last[side] {
                at_eof[section_id][side] = true;
            }
        }
    }

    // Each FileDiff should contain nonzero DiffOps.
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        if ops.is_empty() {
            errors.push(format!("File {file_id} is empty"));
        }
    }

    // Each section_id should be used by at most 2 DiffOps: 1 on the left and 1 on the right.
    for section_id in 0..diff.sections.len() {
        for side in 0..2 {
            if used[section_id][side].len() > 1 {
                errors.push(format!(
                    "{} is used by multiple DiffOps: {}",
                    side_str(section_id, side),
                    used[section_id][side].join(", ")
                ));
            }
        }
    }

    // Each section_id should be used somewhere.
    for section_id in 0..diff.sections.len() {
        if used[section_id][0].is_empty() && used[section_id][1].is_empty() {
            errors.push(format!("Section {section_id} is not referenced anywhere in diff.files"));
        }
    }

    // Each section side should be non-empty if-and-only-if it's used.
    // TODO: Currently the algorithm sometimes generates a Match with one empty side. The renderer
    // is OK with that, but it would be better to have an Insert/Delete so it can be merged with
    // surrounding Inserts/Deletes into one padded group.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let is_empty = section_side.text_with_words.is_empty();
            let is_used = !used[section_id][side].is_empty();
            if is_empty && is_used {
                errors.push(format!(
                    "{} is empty, but it is used by {}",
                    side_str(section_id, side),
                    used[section_id][side][0]
                ));
            }
            if !is_empty && !is_used {
                errors.push(format!(
                    "{} is non-empty, but it is not used by any DiffOp",
                    side_str(section_id, side)
                ));
            }
        }
    }

    // Each section side should end with a '\n', unless it's at the end of file.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let ends_with_newline = match section_side.text_with_words.last() {
                None => true,
                Some((_, part)) => part.ends_with("\n"),
            };
            if !at_eof[section_id][side] && !ends_with_newline {
                errors.push(format!("{} does not end with a newline", side_str(section_id, side)));
            }
        }
    }

    // Every '\n' should be highlighted, except the '\n' at the end of a section.
    // Because sections should be split by matching newlines so we can add padding.
    // TODO: Decide whether we really want this, and in which cases (equal/non-equal).
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            for (i, &(highlight, part)) in section_side.text_with_words.iter().enumerate() {
                let part: &str = if i == section_side.text_with_words.len() - 1 && part.ends_with("\n") {
                    &part[0..part.len() - 1]
                } else {
                    part
                };
                if !highlight && part.contains("\n") {
                    errors.push(format!(
                        "{} contains a newline that is not highlighted at {i}",
                        side_str(section_id, side)
                    ));
                }
            }
        }
    }

    // Sections with `section.equal == true` should contain the same string on both sides.
    // At least for now. We might change this later when ignoring whitespace is enabled.
    for (section_id, section) in diff.sections.iter().enumerate() {
        if section.equal && section.sides[0].text_with_words != section.sides[1].text_with_words {
            errors.push(format!(
                "Section {section_id} has equal==true, but its sides actually differ"
            ));
        }
    }

    // Sections with `section.equal == true` should not contain any highlighted words.
    for (section_id, section) in diff.sections.iter().enumerate() {
        if section.equal && section.sides[0].text_with_words.iter().any(|&(highlight, _)| highlight) {
            errors.push(format!(
                "Section {section_id} has equal==true, but it contains highlighted text"
            ));
        }
    }

    // Each word in text_with_words should be non-empty.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            for (i, (_, part)) in section_side.text_with_words.iter().enumerate() {
                if part.is_empty() {
                    errors.push(format!(
                        "{} has an empty string in text_with_words at {i}",
                        side_str(section_id, side)
                    ));
                }
            }
        }
    }

    // The non-highlighted words should match between each section's sides.
    for (section_id, section) in diff.sections.iter().enumerate() {
        let non_highlighted_bytes = |side: usize| {
            section.sides[side]
                .text_with_words
                .iter()
                .filter(|(highlight, _)| !highlight)
                .flat_map(|(_, part)| part.bytes())
        };
        if !non_highlighted_bytes(0).eq(non_highlighted_bytes(1)) {
            errors.push(format!(
                "Section {section_id} has unequal non-highlighted bytes on its left and right side"
            ));
        }
    }

    // Highlights in text_with_words should alternate between true and false.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let mut last: Option<bool> = None;
            for (i, &(highlight, _)) in section_side.text_with_words.iter().enumerate() {
                if last == Some(highlight) {
                    errors.push(format!(
                        "{} has the same highlight boolean twice text_with_words at {i}",
                        side_str(section_id, side)
                    ));
                }
                last = Some(highlight);
            }
        }
    }

    // The diff fragments of each file and side match the original file content.
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        for side in 0..2 {
            let side_name = ["left", "right"][side];
            let prefix = format!("Diff output of {side_name} side of file {file_id} does not match string input");
            let input = file_input[file_id][side];
            let check_input = || {
                let mut cursor = 0;
                for &(op, section_id) in ops {
                    // TODO: movement()
                    if (op == DiffOp::Insert && side == 0) || (op == DiffOp::Delete && side == 1) {
                        continue;
                    }
                    for &(_, part) in &diff.sections[section_id].sides[side].text_with_words {
                        let end = cursor + part.len();
                        if end > input.len() {
                            return Some(format!(
                                "{prefix} at {cursor}: part {part:?} in diff continues beyond EOF in input"
                            ));
                        }
                        if let Some(input_part) = input.get(cursor..end) {
                            if part != input_part {
                                return Some(format!("{prefix} at {cursor}: part {part:?} in diff does not match part {input_part:?} in input"));
                            }
                        } else {
                            return Some(format!("{prefix} at {cursor}: part {part:?} at {cursor} does not match character boundaries in input"));
                        }
                        cursor = end;
                    }
                }
                if cursor != input.len() {
                    return Some(format!(
                        "{prefix} at {cursor}: diff ended too early before EOF in input"
                    ));
                }
                None
            };
            if let Some(error) = check_input() {
                errors.push(error);
            }
        }
    }

    errors
}

pub fn print_errors(errors: &[String]) {
    if !errors.is_empty() {
        eprintln!("Diff validation errors:");
        for error in errors {
            eprintln!("  {error}");
        }
    }
}
