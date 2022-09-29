use super::algorithm::{Diff, FileDiff};

pub fn validate(diff: &Diff, file_input: &[[&str; 2]]) -> Vec<String> {
    let mut errors = vec![];

    fn side_str(section_id: usize, side: usize) -> String {
        let side_name = ["Left", "Right"][side];
        format!("{side_name} side of section {section_id}")
    }

    let mut used: Vec<[Vec<String>; 2]> = vec![Default::default(); diff.sections.len()];
    let mut content: Vec<[&str; 2]> = vec![[""; 2]; diff.sections.len()];
    let mut at_eof: Vec<[bool; 2]> = vec![Default::default(); diff.sections.len()];
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let mut last = [None; 2];
        for (op_index, &(op, section_id)) in ops.iter().enumerate() {
            for side in 0..2 {
                if op.movement()[side] != 0 {
                    used[section_id][side].push(format!("{op:?} in file #{file_id} at op index #{op_index}"));
                    last[side] = Some(section_id);
                    let highlight_bounds = &diff.sections[section_id].sides[side].highlight_bounds;
                    if !highlight_bounds.is_empty() {
                        content[section_id][side] =
                            &file_input[file_id][side][highlight_bounds[0]..*highlight_bounds.last().unwrap()];
                    }
                }
            }
        }
        for side in 0..2 {
            if let Some(section_id) = last[side] {
                at_eof[section_id][side] = true;
            }
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

    // Each section side's higlight_bounds should be either empty or have at least 2 elements (start and end).
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            if section_side.highlight_bounds.len() == 1 {
                errors.push(format!("{} has highlight_bounds length 1", side_str(section_id, side),));
            }
        }
    }

    // Each section side should be non-empty if-and-only-if it's used.
    // TODO: Currently the algorithm sometimes generates a Match with one empty side. The renderer
    // is OK with that, but it would be better to have an Insert/Delete so it can be merged with
    // surrounding Inserts/Deletes into one padded group.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let is_empty = section_side.highlight_bounds.is_empty();
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

    // Each non-empty section side should end with a '\n', unless it's at the end of file.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let is_empty = section_side.highlight_bounds.is_empty();
            let ends_with_newline = content[section_id][side].ends_with("\n");
            if !at_eof[section_id][side] && !is_empty && !ends_with_newline {
                errors.push(format!("{} does not end with a newline", side_str(section_id, side)));
            }
        }
    }

    // Every '\n' should be highlighted, except the '\n' at the end of a section (that one doesn't matter).
    // Because sections should be split by matching newlines so we can add padding.
    // TODO: Decide whether we really want this, and in which cases (equal/non-equal).
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let highlight_bounds = &diff.sections[section_id].sides[side].highlight_bounds;
            if highlight_bounds.is_empty() {
                continue;
            }
            for i in 0..(highlight_bounds.len() - 1) {
                let highlight = section_side.highlight_first ^ (i % 2 == 1);
                let rel_start_offset = highlight_bounds[i] - highlight_bounds[0];
                let rel_end_offset = highlight_bounds[i + 1] - highlight_bounds[0];
                let part = &content[section_id][side][rel_start_offset..rel_end_offset];
                let part: &str = if i == highlight_bounds.len() - 2 && part.ends_with("\n") {
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
        if section.equal && content[section_id][0] != content[section_id][1] {
            errors.push(format!(
                "Section {section_id} has equal==true, but its sides actually differ"
            ));
        }
    }

    // Sections with `section.equal == true` should not contain any highlighted words.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            if section.equal && (section_side.highlight_first || section_side.highlight_bounds.len() > 2) {
                errors.push(format!(
                    "{} has equal==true, but it contains highlighted text",
                    side_str(section_id, side)
                ));
            }
        }
    }

    // Byte offsets in highlight_bounds should be increasing.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            for i in 1..section_side.highlight_bounds.len() {
                if !(section_side.highlight_bounds[i - 1] < section_side.highlight_bounds[i]) {
                    errors.push(format!(
                        "{} highlight_bounds has {} at {} which is not smaller than {} at {}",
                        side_str(section_id, side),
                        section_side.highlight_bounds[i - 1],
                        i - 1,
                        section_side.highlight_bounds[i],
                        i
                    ));
                }
            }
        }
    }

    // The non-highlighted words should match between each section's sides.
    for (section_id, section) in diff.sections.iter().enumerate() {
        let non_highlighted_bytes = |side: usize| {
            let content_ref = &content;
            let highlight_bounds = &section.sides[side].highlight_bounds;
            let inner_iterator = if highlight_bounds.is_empty() {
                None
            } else {
                Some(
                    (0..(highlight_bounds.len() - 1))
                        .filter(move |i| !(section.sides[side].highlight_first ^ (i % 2 == 1)))
                        .flat_map(move |i| {
                            let rel_start_offset = highlight_bounds[i] - highlight_bounds[0];
                            let rel_end_offset = highlight_bounds[i + 1] - highlight_bounds[0];
                            content_ref[section_id][side][rel_start_offset..rel_end_offset].bytes()
                        }),
                )
            };
            inner_iterator.into_iter().flatten()
        };
        if !non_highlighted_bytes(0).eq(non_highlighted_bytes(1)) {
            errors.push(format!(
                "Section {section_id} has unequal non-highlighted bytes on its left and right side"
            ));
        }
    }

    // The sections of each file should exactly cover the file content.
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        for side in 0..2 {
            let side_name = ["left", "right"][side];
            let mut current_offset = 0;
            for &(op, section_id) in ops {
                if op.movement()[side] == 0 {
                    continue;
                }
                let highlight_bounds = &diff.sections[section_id].sides[side].highlight_bounds;
                if highlight_bounds.is_empty() {
                    continue; // We already complained.
                }
                let start_offset = highlight_bounds[0];
                if current_offset != start_offset {
                    errors.push(format!("The {side_name} side of file {file_id} has section {section_id} which starts at offset {start_offset}, but it should start at offset {current_offset}"));
                }
                current_offset = highlight_bounds[highlight_bounds.len() - 1];
            }
            let file_size = file_input[file_id][side].len();
            if current_offset != file_size {
                errors.push(format!("The last section of the {side_name} side of file {file_id} ends at offset {current_offset}, but the file is {file_size} bytes long"));
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
