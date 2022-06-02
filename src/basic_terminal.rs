use super::algorithm::{FileDiff, SectionSide};
use console::Style;
use std::io;

fn print_side(
    prefix: char,
    style_var: Style,
    side: &SectionSide,
    output: &mut dyn io::Write,
) -> io::Result<()> {
    if side.text_with_words.len() != 0 {
        write!(output, "{}", style_var.apply_to(prefix))?;
        for (highlight, text) in &side.text_with_words {
            write!(
                output,
                "{}",
                if *highlight {
                    style_var.apply_to(text).underlined().on_black()
                } else {
                    style_var.apply_to(text)
                }
            )?;
        }
    }
    Ok(())
}

pub fn print(diff: &FileDiff, output: &mut dyn io::Write) -> io::Result<()> {
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
