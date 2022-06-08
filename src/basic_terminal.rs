use super::algorithm::{FileDiff, SectionSide};
use console::Style;
use std::io;

fn print_side(
    prefix: char,
    style_var: Style,
    side: &SectionSide,
    output: &mut impl io::Write,
) -> io::Result<()> {
    let mut fresh_line = true;
    for (highlight, text) in &side.text_with_words {
        for part_with_eol in text.split_inclusive('\n') {
            let (part, eol) = match part_with_eol.strip_suffix('\n') {
                Some(part) => (part, true),
                None => (part_with_eol, false),
            };
            if fresh_line {
                write!(output, "{}", style_var.apply_to(prefix))?;
                fresh_line = false;
            }
            let styled_part = style_var.apply_to(part);
            if *highlight {
                write!(output, "{}", styled_part.underlined().on_yellow())?;
            } else {
                write!(output, "{}", styled_part)?;
            }
            if eol {
                // Never highlight the final newline because terminals are bad.
                write!(output, "\n")?;
                fresh_line = true;
            }
        }
    }
    if !fresh_line {
        write!(output, "\n\\ No newline at end of file\n")?;
    }

    Ok(())
}

pub fn print(diff: &FileDiff, output: &mut impl io::Write) -> io::Result<()> {
    for section in &diff.0 {
        if section.equal {
            // TODO: Check old == new.
            print_side(' ', Style::new(), &section.old, output)?;
        } else {
            print_side('-', Style::new().red(), &section.old, output)?;
            print_side('+', Style::new().green(), &section.new, output)?;
        }
    }
    Ok(())
}
