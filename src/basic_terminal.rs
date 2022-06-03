use super::algorithm::{FileDiff, SectionSide};
use console::Style;
use std::io;

fn print_side(
    prefix: char,
    style_var: Style,
    side: &SectionSide,
    output: &mut impl io::Write,
) -> io::Result<()> {
    if side.text_with_words.len() != 0 {
        write!(output, "{}", style_var.apply_to(prefix))?;
        for (highlight, text) in &side.text_with_words {
            let mut fixed_text = String::new();
            let mut it = text.chars().peekable();
            while let Some(ch) = it.next() {
                fixed_text.push(ch);
                if ch == '\n' && !it.peek().is_none() {
                    fixed_text.push(prefix);
                }
            }

            write!(
                output,
                "{}",
                if *highlight {
                    style_var.apply_to(fixed_text).underlined().on_black()
                } else {
                    style_var.apply_to(fixed_text)
                }
            )?;
        }
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
