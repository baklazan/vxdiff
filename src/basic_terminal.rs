use super::algorithm::{FileDiff, SectionSide};
use console::Style;
use std::error::Error;
use std::fmt::Write;
use std::io;

type TheResult = Result<(), Box<dyn Error>>;

fn print_side(
    prefix: char,
    style_var: Style,
    side: &SectionSide,
    write_line: &mut impl FnMut(&str, usize, &Style) -> TheResult,
) -> TheResult {
    let highlighted_style = style_var.clone().underlined().on_yellow();

    let mut current_line = "".to_string();
    let mut current_line_visible_length = 0;
    for (highlight, text) in &side.text_with_words {
        let my_style = if *highlight {
            &highlighted_style
        } else {
            &style_var
        };
        for part_with_eol in text.split_inclusive('\n') {
            let (part, eol) = match part_with_eol.strip_suffix('\n') {
                Some(part) => (part, true),
                None => (part_with_eol, false),
            };
            if current_line.is_empty() {
                write!(&mut current_line, "{}", style_var.apply_to(prefix))?;
                current_line_visible_length += 1;
            }
            write!(&mut current_line, "{}", my_style.apply_to(part))?;
            current_line_visible_length += part.len();
            if eol {
                write_line(&current_line, current_line_visible_length, my_style)?;
                current_line = "".to_string();
                current_line_visible_length = 0;
            }
        }
    }
    if !current_line.is_empty() {
        write_line(&current_line, current_line_visible_length, &Style::new())?;
        let warn = "\\ No newline at end of file";
        write_line(warn, warn.len(), &Style::new())?;
    }

    Ok(())
}

pub fn print(diff: &FileDiff, output: &mut impl io::Write) -> TheResult {
    // TODO: Why dyn?
    let mut write_line: &mut dyn FnMut(&str, usize, &Style) -> TheResult =
        &mut |line, _visible_length, _newline_style| {
            // Never highlight the final newline because terminals are bad.
            write!(output, "{}\n", line)?;
            Ok(())
        };
    for section in &diff.0 {
        if section.equal {
            // TODO: Check old == new.
            print_side(' ', Style::new(), &section.old, &mut write_line)?;
        } else {
            print_side('-', Style::new().red(), &section.old, &mut write_line)?;
            print_side('+', Style::new().green(), &section.new, &mut write_line)?;
        }
    }
    Ok(())
}

pub fn print_side_by_side(diff: &FileDiff, output: &mut impl io::Write) -> TheResult {
    let width = 30;
    for section in &diff.0 {
        write!(output, "---section---\n")?;
        let (left_char, left_style, right_char, right_style) = if section.equal {
            (' ', Style::new(), ' ', Style::new())
        } else {
            ('-', Style::new().red(), '+', Style::new().green())
        };
        let mut left_lines = vec![];
        let mut right_lines = vec![];
        print_side(
            left_char,
            left_style,
            &section.old,
            &mut |line: &str, visible_length, _newline_style| {
                left_lines.push(line.to_string() + &" ".repeat(width - visible_length));
                Ok(())
            },
        )?;
        print_side(
            right_char,
            right_style,
            &section.new,
            &mut |line: &str, visible_length, _newline_style| {
                right_lines.push(line.to_string() + &" ".repeat(width - visible_length));
                Ok(())
            },
        )?;
        // TODO: The 'itertools' crate has a zip_longest function.
        while left_lines.len() < right_lines.len() {
            left_lines.push(" ".repeat(width));
        }
        while right_lines.len() < left_lines.len() {
            right_lines.push(" ".repeat(width));
        }
        for (left_line, right_line) in std::iter::zip(left_lines, right_lines) {
            write!(output, "{} | {}\n", left_line, right_line)?;
        }
    }
    Ok(())
}
