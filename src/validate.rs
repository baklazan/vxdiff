use super::algorithm::{Diff, FileDiff};
use std::iter::once;
use std::ops::Range;

pub fn validate(diff: &Diff, file_input: &[[&str; 2]], file_names: &[[&str; 2]]) -> Vec<String> {
    let mut errors = vec![];

    fn side_str(section_id: usize, side: usize) -> String {
        let side_name = ["Left", "Right"][side];
        format!("{side_name} side of section {section_id}")
    }

    let mut used: Vec<[Vec<String>; 2]> = vec![Default::default(); diff.sections.len()];
    let mut whole_content: Vec<[&str; 2]> = vec![[""; 2]; diff.sections.len()];
    let mut at_eof: Vec<[bool; 2]> = vec![Default::default(); diff.sections.len()];
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        let mut last = [None; 2];
        for (op_index, &(op, section_id)) in ops.iter().enumerate() {
            for side in 0..2 {
                if op.movement()[side] != 0 {
                    let filename = file_names[file_id][side];
                    used[section_id][side].push(format!(
                        "{op:?} in file #{file_id} ({filename:?}) at op index #{op_index}"
                    ));
                    last[side] = Some(section_id);
                    whole_content[section_id][side] = file_input[file_id][side];
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

    // Each section side should be non-empty if-and-only-if it's used.
    // Each empty section side should be 0..0.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let is_empty = section_side.byte_range.is_empty();
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
            if is_empty && section_side.byte_range != (0..0) {
                errors.push(format!(
                    "{} is empty, but the byte range is {:?} instead of 0..0",
                    side_str(section_id, side),
                    section_side.byte_range
                ));
            }
        }
    }

    // Each non-empty section side should end with a '\n', unless it's at the end of file.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let is_empty = section_side.byte_range.is_empty();
            let content = &whole_content[section_id][side][section_side.byte_range.clone()];
            let ends_with_newline = content.ends_with('\n');
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
            if section_side.byte_range.is_empty() {
                continue;
            }
            for offset in section_side.byte_range.start..(section_side.byte_range.end - 1) {
                if whole_content[section_id][side].as_bytes()[offset] == b'\n' {
                    let cmp = |range: &Range<usize>| {
                        if range.contains(&offset) {
                            std::cmp::Ordering::Equal
                        } else {
                            range.start.cmp(&offset)
                        }
                    };
                    if section_side.highlight_ranges.binary_search_by(cmp).is_err() {
                        errors.push(format!(
                            "{} contains a newline that is not highlighted at {offset}",
                            side_str(section_id, side)
                        ));
                    }
                }
            }
        }
    }

    // Sections with `section.equal == true` should contain the same string on both sides.
    // At least for now. We might change this later when ignoring whitespace is enabled.
    for (section_id, section) in diff.sections.iter().enumerate() {
        let get = |side: usize| &whole_content[section_id][side][section.sides[side].byte_range.clone()];
        if section.equal && get(0) != get(1) {
            errors.push(format!(
                "Section {section_id} has equal==true, but its sides actually differ"
            ));
        }
    }

    // Sections with `section.equal == true` should not contain any highlighted words.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            if section.equal && !section_side.highlight_ranges.is_empty() {
                errors.push(format!(
                    "{} has equal==true, but it contains highlighted text",
                    side_str(section_id, side)
                ));
            }
        }
    }

    // Byte offsets in highlight_ranges should be increasing. (start < end && end < next start)
    // Byte offsets in highlight_ranges should be within byte_range.
    for (section_id, section) in diff.sections.iter().enumerate() {
        for (side, section_side) in section.sides.iter().enumerate() {
            let mut last = None;
            for value in section_side.highlight_ranges.iter().flat_map(|r| [r.start, r.end]) {
                if value < section_side.byte_range.start || value > section_side.byte_range.end {
                    errors.push(format!(
                        "{} highlight_ranges contains {} which is outside of {:?}",
                        side_str(section_id, side),
                        value,
                        section_side.byte_range
                    ));
                }
                if let Some(last) = last {
                    if !(last < value) {
                        errors.push(format!(
                            "{} highlight_ranges contains {} followed by {}",
                            side_str(section_id, side),
                            last,
                            value
                        ));
                    }
                }
                last = Some(value);
            }
        }
    }

    // The non-highlighted words should match between each section's sides.
    for (section_id, section) in diff.sections.iter().enumerate() {
        let non_highlighted_chars = |side: usize| {
            let whole_content_ref = &whole_content[section_id][side];
            let highlight_ranges = &section.sides[side].highlight_ranges;
            let starts = once(section.sides[side].byte_range.start).chain(highlight_ranges.iter().map(|r| r.end));
            let ends = (highlight_ranges.iter().map(|r| r.start)).chain(once(section.sides[side].byte_range.end));
            std::iter::zip(starts, ends).flat_map(move |(start, end)| whole_content_ref[start..end].chars())
        };
        if !non_highlighted_chars(0).eq(non_highlighted_chars(1)) {
            errors.push(format!(
                "Section {section_id} has unequal non-highlighted text on its left and right side: {:?} vs {:?}",
                non_highlighted_chars(0).collect::<String>(),
                non_highlighted_chars(1).collect::<String>(),
            ));
        }
    }

    // The sections of each file should exactly cover the file content.
    for (file_id, FileDiff { ops }) in diff.files.iter().enumerate() {
        for side in 0..2 {
            let side_name = ["left", "right"][side];
            let filename = file_names[file_id][side];
            let mut current_offset = 0;
            for &(op, section_id) in ops {
                if op.movement()[side] == 0 {
                    continue;
                }
                let byte_range = &diff.sections[section_id].sides[side].byte_range;
                if byte_range.is_empty() {
                    continue; // We already complained.
                }
                let start_offset = byte_range.start;
                if current_offset != start_offset {
                    errors.push(format!("The {side_name} side of file {file_id} ({filename:?}) has section {section_id} which starts at offset {start_offset}, but it should start at offset {current_offset}"));
                }
                current_offset = byte_range.end;
            }
            let file_size = file_input[file_id][side].len();
            if current_offset != file_size {
                errors.push(format!("The last section of the {side_name} side of file {file_id} ({filename:?}) ends at offset {current_offset}, but the file is {file_size} bytes long"));
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
