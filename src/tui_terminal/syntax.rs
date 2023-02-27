use crate::config::{Config, Style as VxdiffStyle};
use anyhow::{bail, Result};
use std::ffi::OsStr;
use std::ops::Range;
use std::path::Path;
use syntect::highlighting::{
    Color as SyntectColor, FontStyle, HighlightState, Highlighter, RangedHighlightIterator, Style as SyntectStyle,
    Theme, ThemeSet,
};
use syntect::parsing::{ParseState, ScopeStack, SyntaxSet};
use syntect::util::LinesWithEndings;
use tui::style::Color as VxdiffColor;

fn convert_style(style: SyntectStyle) -> VxdiffStyle {
    fn convert_color(color: SyntectColor) -> Option<VxdiffColor> {
        // TODO: Properly deal with alpha not equal to 0 or 255.
        if color.a != 0 {
            Some(VxdiffColor::Rgb(color.r, color.g, color.b))
        } else {
            None
        }
    }

    fn convert_font_style(value: bool) -> Option<bool> {
        match value {
            true => Some(true),
            false => None,
        }
    }

    VxdiffStyle {
        fg: convert_color(style.foreground),
        bg: convert_color(style.background),
        bold: convert_font_style(style.font_style.contains(FontStyle::BOLD)),
        underlined: convert_font_style(style.font_style.contains(FontStyle::UNDERLINE)),
        dim: None,
        italic: convert_font_style(style.font_style.contains(FontStyle::ITALIC)),
        crossed_out: None,
    }
}

fn load_theme(theme_name: &OsStr) -> Result<Theme> {
    if let Some(theme_name) = theme_name.to_str() {
        if !theme_name.contains('/') {
            let mut default_themes = ThemeSet::load_defaults().themes;
            if let Some(theme) = default_themes.remove(theme_name) {
                return Ok(theme);
            }
        }
    }

    if !Path::new(theme_name).exists() {
        let default_names = ThemeSet::load_defaults()
            .themes
            .into_iter()
            .map(|(k, _v)| format!("\"{k}\""))
            .collect::<Vec<_>>() // https://stackoverflow.com/q/56033289 :(
            .join(", ");
        bail!("Not an existing file or a built-in theme name ({default_names})");
    }

    Ok(ThemeSet::get_theme(theme_name)?)
}

pub fn make_syntax_highlights(
    config: &Config,
    file_input: &[[&str; 2]],
    file_names: &[[&str; 2]],
    mut err_out: impl std::io::Write,
) -> Vec<[Vec<(Range<usize>, VxdiffStyle)>; 2]> {
    if !config.syntax_highlighting || config.syntax_theme.is_empty() {
        return file_input.iter().map(|_| [vec![], vec![]]).collect();
    }

    let mut theme = match load_theme(&config.syntax_theme) {
        Ok(theme) => theme,
        Err(e) => {
            let name = config.syntax_theme.to_string_lossy();
            writeln!(&mut err_out, "Error loading syntax highlighting theme '{name}': {e:#}").unwrap();
            return file_input.iter().map(|_| [vec![], vec![]]).collect();
        }
    };

    theme.settings.background = Some(SyntectColor {
        r: 255,
        g: 255,
        b: 255,
        a: 0,
    });

    // TODO: If we add runtime theme switching in the future, save SyntaxSet in State.
    let ss = if !config.syntax_dir.is_empty() && Path::new(&config.syntax_dir).is_dir() {
        let mut builder = SyntaxSet::load_defaults_newlines().into_builder();
        match builder.add_from_folder(Path::new(&config.syntax_dir), true) {
            Ok(()) => builder.build(),
            Err(e) => {
                let name = config.syntax_dir.to_string_lossy();
                writeln!(&mut err_out, "Error loading syntax definitions from '{name}': {e}").unwrap();
                SyntaxSet::load_defaults_newlines()
            }
        }
    } else {
        SyntaxSet::load_defaults_newlines()
    };

    let mut make = |content: &str, filename: &str| {
        let path = Path::new(filename);
        let first_line = LinesWithEndings::from(content).next().unwrap_or("");

        // Based on SyntaxSet::find_syntax_for_file.
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        let extension = path.extension().and_then(|x| x.to_str()).unwrap_or("");
        let syntax = (ss.find_syntax_by_extension(file_name))
            .or_else(|| ss.find_syntax_by_extension(extension))
            .or_else(|| ss.find_syntax_by_first_line(first_line))
            .unwrap_or_else(|| ss.find_syntax_plain_text());

        let highlighter = Highlighter::new(&theme);
        let mut highlight_state = HighlightState::new(&highlighter, ScopeStack::new());
        let mut parse_state = ParseState::new(syntax);
        let mut result = vec![];
        let mut line_offset = 0;
        for line in LinesWithEndings::from(content) {
            // Based on HighlightLines::highlight_line.
            let ops = match parse_state.parse_line(line, &ss) {
                Ok(ops) => ops,
                Err(e) => {
                    writeln!(&mut err_out, "Syntax highlighting error: {e}").unwrap();
                    return vec![];
                }
            };
            for (style, _, range) in RangedHighlightIterator::new(&mut highlight_state, &ops[..], line, &highlighter) {
                if range.is_empty() {
                    continue;
                }
                let vxdiff_style = convert_style(style);
                result.push(((line_offset + range.start)..(line_offset + range.end), vxdiff_style));
            }
            line_offset += line.len();
        }
        result
    };

    std::iter::zip(file_input, file_names)
        .map(|(contents, names)| [0, 1].map(|side| make(contents[side], names[side])))
        .collect()
}
