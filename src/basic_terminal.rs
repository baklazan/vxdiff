use super::algorithm::{Diff, DiffOp, SectionSide};
use console::Style;
use std::error::Error;
use std::fmt::Write;
use std::io;

type TheResult = Result<(), Box<dyn Error>>;

fn print_side(
    prefix: char,
    style_var: &Style,
    side: &SectionSide,
    write_line: &mut impl FnMut(&str, usize, &Style) -> TheResult,
) -> TheResult {
    let highlighted_style = style_var.clone().on_yellow();

    let mut current_line = "".to_string();
    let mut current_line_visible_length = 0;
    for (highlight, text) in &side.text_with_words {
        let my_style = if *highlight { &highlighted_style } else { &style_var };
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

pub fn print(diff: &Diff, output: &mut impl io::Write) -> TheResult {
    let mut write_line = |line: &str, _visible_length: usize, _newline_style: &Style| {
        // Never highlight the final newline because terminals are bad.
        write!(output, "{}\n", line)?;
        Ok(())
    };
    for (op, section_id) in diff.files[0].ops.iter() {
        let section = &diff.sections[*section_id];
        match op {
            DiffOp::Match => {
                if section.equal {
                    // TODO: Check old == new.
                    print_side(' ', &Style::new(), &section.sides[0], &mut write_line)?;
                } else {
                    print_side('-', &Style::new().red(), &section.sides[0], &mut write_line)?;
                    print_side('+', &Style::new().green(), &section.sides[1], &mut write_line)?;
                }
            }
            DiffOp::Insert => {
                print_side('+', &Style::new().green(), &section.sides[1], &mut write_line)?;
            }
            DiffOp::Delete => {
                print_side('-', &Style::new().red(), &section.sides[0], &mut write_line)?;
            }
        }
    }
    Ok(())
}

pub fn print_side_by_side(diff: &Diff, output: &mut impl io::Write) -> TheResult {
    let width = 121;
    let empty = String::from("x") + &" ".repeat(width - 1);

    for (op, section_id) in diff.files[0].ops.iter() {
        let section = &diff.sections[*section_id];
        let styles = match op {
            DiffOp::Match => {
                if section.equal {
                    [(' ', Style::new()), (' ', Style::new())]
                } else {
                    [('-', Style::new().red()), ('+', Style::new().green())]
                }
            }
            DiffOp::Insert => [('*', Style::new().blue()), ('+', Style::new().green())],
            DiffOp::Delete => [('-', Style::new().red()), ('*', Style::new().blue())],
        };

        let mut lines = [vec![], vec![]];
        for i in 0..2 {
            let mut write_line = |line: &str, visible_length, _newline_style: &Style| {
                lines[i].push(line.to_string() + &" ".repeat(width - visible_length));
                Ok(())
            };
            print_side(styles[i].0, &styles[i].1, &section.sides[i], &mut write_line)?;
        }
        for i in 0..std::cmp::max(lines[0].len(), lines[1].len()) {
            let left = lines[0].get(i).unwrap_or(&empty);
            let right = lines[1].get(i).unwrap_or(&empty);
            write!(output, "{} | {}\n", left, right)?;
        }
    }
    Ok(())
}
