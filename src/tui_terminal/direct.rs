use super::TheResult;
use crossterm::queue;
use crossterm::style::{
    Attribute as CAttribute, Color as CColor, Print, SetAttribute, SetBackgroundColor, SetForegroundColor,
};
use std::io::Write;
use tui::buffer::Buffer;
use tui::style::{Color, Modifier};
use unicode_width::UnicodeWidthStr as _;

pub fn print_buffer_directly(buffer: &Buffer, mut output: impl Write) -> TheResult {
    // This is based on tui::backend::CrosstermBackend::draw() but without the MoveTo calls.
    // With bits of tui::buffer::Buffer::diff() mixed in.
    // TODO: Can we switch from tui-rs to some better library?
    let mut skip = false;
    let mut fg = Color::Reset;
    let mut bg = Color::Reset;
    let mut modifier = Modifier::empty();
    for cell in &buffer.content {
        if skip {
            skip = false;
            continue;
        }
        if cell.modifier != modifier {
            modifier_diff(modifier, cell.modifier, &mut output)?;
            modifier = cell.modifier;
        }
        if cell.fg != fg {
            queue!(output, SetForegroundColor(CColor::from(cell.fg)))?;
            fg = cell.fg;
        }
        if cell.bg != bg {
            queue!(output, SetBackgroundColor(CColor::from(cell.bg)))?;
            bg = cell.bg;
        }
        queue!(output, Print(&cell.symbol))?;
        if cell.symbol.width() > 1 {
            skip = true;
        }
    }
    queue!(
        output,
        SetForegroundColor(CColor::Reset),
        SetBackgroundColor(CColor::Reset),
        SetAttribute(CAttribute::Reset),
        Print("\n"),
    )?;
    Ok(())
}

fn modifier_diff(from: Modifier, to: Modifier, mut w: impl Write) -> TheResult {
    // This is based on tui::backend::crossterm::ModifierDiff, because it isn't public.
    let removed = from - to;
    if removed.contains(Modifier::REVERSED) {
        (queue!(w, SetAttribute(CAttribute::NoReverse)))?;
    }
    if removed.contains(Modifier::BOLD) {
        (queue!(w, SetAttribute(CAttribute::NormalIntensity)))?;
        if to.contains(Modifier::DIM) {
            (queue!(w, SetAttribute(CAttribute::Dim)))?;
        }
    }
    if removed.contains(Modifier::ITALIC) {
        (queue!(w, SetAttribute(CAttribute::NoItalic)))?;
    }
    if removed.contains(Modifier::UNDERLINED) {
        (queue!(w, SetAttribute(CAttribute::NoUnderline)))?;
    }
    if removed.contains(Modifier::DIM) {
        (queue!(w, SetAttribute(CAttribute::NormalIntensity)))?;
    }
    if removed.contains(Modifier::CROSSED_OUT) {
        (queue!(w, SetAttribute(CAttribute::NotCrossedOut)))?;
    }
    if removed.contains(Modifier::SLOW_BLINK) || removed.contains(Modifier::RAPID_BLINK) {
        (queue!(w, SetAttribute(CAttribute::NoBlink)))?;
    }

    let added = to - from;
    if added.contains(Modifier::REVERSED) {
        (queue!(w, SetAttribute(CAttribute::Reverse)))?;
    }
    if added.contains(Modifier::BOLD) {
        (queue!(w, SetAttribute(CAttribute::Bold)))?;
    }
    if added.contains(Modifier::ITALIC) {
        (queue!(w, SetAttribute(CAttribute::Italic)))?;
    }
    if added.contains(Modifier::UNDERLINED) {
        (queue!(w, SetAttribute(CAttribute::Underlined)))?;
    }
    if added.contains(Modifier::DIM) {
        (queue!(w, SetAttribute(CAttribute::Dim)))?;
    }
    if added.contains(Modifier::CROSSED_OUT) {
        (queue!(w, SetAttribute(CAttribute::CrossedOut)))?;
    }
    if added.contains(Modifier::SLOW_BLINK) {
        (queue!(w, SetAttribute(CAttribute::SlowBlink)))?;
    }
    if added.contains(Modifier::RAPID_BLINK) {
        (queue!(w, SetAttribute(CAttribute::RapidBlink)))?;
    }

    Ok(())
}
