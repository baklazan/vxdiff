use clap::{Args, ValueEnum};
use serde::Deserialize;
use std::path::PathBuf;
use tui::style::Color;

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum ClipboardMechanism {
    /// Copy to clipboard using the OSC 52 escape sequence.
    /// Needs support from the terminal.
    /// See also: https://github.com/zyedidia/micro/blob/master/runtime/help/copypaste.md
    Terminal,

    /// Copy to clipboard by running a helper such as xclip, xsel or pbcopy.
    ExternalHelper,

    /// Don't copy to system clipboard.
    None,
}

#[derive(Clone, Copy, Default, Deserialize)]
pub struct Style {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub bold: Option<bool>,
    pub underlined: Option<bool>,
    pub dim: Option<bool>,
    pub italic: Option<bool>,
    pub crossed_out: Option<bool>,
}

impl Style {
    pub fn patch(self, other: Style) -> Style {
        Style {
            fg: other.fg.or(self.fg),
            bg: other.bg.or(self.bg),
            bold: other.bold.or(self.bold),
            underlined: other.underlined.or(self.underlined),
            dim: other.dim.or(self.dim),
            italic: other.italic.or(self.italic),
            crossed_out: other.crossed_out.or(self.crossed_out),
        }
    }
}

#[derive(Deserialize)]
pub struct Theme {
    pub cursor: Style,
    pub line_numbers_default: Style,
    pub line_numbers_phantom: Style,
    pub text_equal: Style,
    pub text_padding: Style,
    pub text_change_old: Style,
    pub text_change_new: Style,
    pub text_move_old: Style,
    pub text_move_new: Style,
    pub text_phantom_old: Style,
    pub text_phantom_new: Style,
    pub highlight_change_old: Style,
    pub highlight_change_new: Style,
    pub highlight_move_old: Style,
    pub highlight_move_new: Style,
    pub highlight_phantom_old: Style,
    pub highlight_phantom_new: Style,
    pub fabricated_symbol: Style,
    pub search_highlight: Style,
    pub select_highlight: Style,
    pub button: Style,
    pub button_hint: Style,
}

macro_rules! style {
    ( $( $field:ident = $value:expr ),* ) => { Style { $( $field: Some($value), )* ..Style::default() } }
}

#[allow(dead_code)]
fn default_theme() -> Theme {
    let highlight = style!(bold = true, underlined = true);
    Theme {
        cursor: style!(fg = Color::White, bg = Color::Blue),
        line_numbers_default: style!(),
        line_numbers_phantom: style!(fg = Color::Cyan),
        text_equal: style!(),
        text_padding: style!(fg = Color::DarkGray),
        text_change_old: style!(fg = Color::Red),
        text_change_new: style!(fg = Color::Green),
        text_move_old: style!(fg = Color::Yellow),
        text_move_new: style!(fg = Color::Yellow),
        text_phantom_old: style!(fg = Color::Cyan, dim = true),
        text_phantom_new: style!(fg = Color::Cyan, dim = true),
        highlight_change_old: highlight,
        highlight_change_new: highlight,
        highlight_move_old: highlight,
        highlight_move_new: highlight,
        highlight_phantom_old: highlight,
        highlight_phantom_new: highlight,
        fabricated_symbol: style!(bg = Color::Cyan),
        search_highlight: style!(fg = Color::Black, bg = Color::Yellow),
        select_highlight: style!(fg = Color::Black, bg = Color::White),
        button: style!(fg = Color::Black, bg = Color::White),
        button_hint: style!(fg = Color::White, bg = Color::Blue, bold = true),
    }
}

#[allow(dead_code)]
fn new_theme() -> Theme {
    let black = Color::Indexed(16); // #000000
    let white = Color::Indexed(253); // #DADADA
    let darkest_red = Color::Indexed(52); // #5F0000
    let darkest_green = Color::Indexed(22); // #005F00
    let darkest_yellow = Color::Indexed(58); // #5F5F00
    let darkest_cyan = Color::Indexed(23); // #005F5F
    Theme {
        cursor: style!(fg = Color::White, bg = Color::Blue),
        line_numbers_default: style!(),
        line_numbers_phantom: style!(fg = Color::Cyan),
        text_equal: style!(fg = white, bg = black),
        text_padding: style!(fg = Color::DarkGray, bg = Color::Indexed(238)),
        text_change_old: style!(fg = white, bg = darkest_red),
        text_change_new: style!(fg = white, bg = darkest_green),
        text_move_old: style!(fg = white, bg = darkest_yellow),
        text_move_new: style!(fg = white, bg = darkest_yellow),
        text_phantom_old: style!(fg = white, bg = darkest_cyan, dim = true),
        text_phantom_new: style!(fg = white, bg = darkest_cyan, dim = true),
        highlight_change_old: style!(bg = Color::Indexed(88), bold = true),
        highlight_change_new: style!(bg = Color::Indexed(28), bold = true),
        highlight_move_old: style!(bg = Color::Indexed(100), bold = true),
        highlight_move_new: style!(bg = Color::Indexed(100), bold = true),
        highlight_phantom_old: style!(bg = Color::Indexed(30), bold = true, dim = false),
        highlight_phantom_new: style!(bg = Color::Indexed(30), bold = true, dim = false),
        fabricated_symbol: style!(fg = Color::Cyan, bold = true),
        search_highlight: style!(fg = Color::Black, bg = Color::Yellow),
        select_highlight: style!(fg = Color::Black, bg = Color::White),
        button: style!(fg = black, bg = Color::Indexed(195)), // #D7FFFF
        button_hint: style!(fg = black, bg = Color::Indexed(123), bold = true), // #87FFFF
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum SearchCaseSensitivity {
    CaseSensitive,
    CaseInsensitive,
    DependsOnPattern,
}

macro_rules! config_structs {
    {
        $(
            $( #[config_opt($attr:meta)] )*
            pub $name:ident: $typ:ty,
        )*
        $(
            #[config_alias($atarget:ident = $avalue:expr)]
            $( #[config_opt($aattr:meta)] )*
            pub $aname:ident: bool,
        )*
    } => {
        pub struct Config {
            $( pub $name: $typ, )*
        }

        #[derive(Args, Deserialize)]
        pub struct ConfigOpt {
            $( $( #[$aattr] )* #[serde(skip)] pub $aname: bool, )*
            $( $( #[$attr] )* pub $name: Option<$typ>, )*

            #[arg(long)]
            #[serde(skip)]
            pub config_file: Option<PathBuf>,
        }

        impl Config {
            pub fn update(self, mut opt: ConfigOpt) -> Config {
                $(
                    if opt.$aname {
                        opt.$atarget = Some($avalue);
                    }
                )*
                Config {
                    $( $name: opt.$name.unwrap_or(self.$name), )*
                }
            }
        }
    }
}

config_structs! {
    #[config_opt(arg(long, value_name = "NUM"))]
    pub context_lines: usize,

    #[config_opt(arg(long, value_name = "NUM"))]
    pub mouse_wheel_scroll_lines: usize,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "phantom_rendering_group"))]
    pub phantom_rendering: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "highlight_newlines_group"))]
    pub highlight_newlines: bool,

    #[config_opt(arg(skip))]
    pub theme: Theme,

    #[config_opt(arg(long))]
    pub clipboard_mechanism: ClipboardMechanism,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "search_incremental_group"))]
    pub search_incremental: bool,

    #[config_opt(arg(long, group = "search_default_case_sensitivity_group"))]
    pub search_default_case_sensitivity: SearchCaseSensitivity,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "search_default_regexp_group"))]
    pub search_default_regexp: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "open_all_files_group"))]
    pub open_all_files: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "show_cursor_group"))]
    pub show_cursor: bool,

    #[config_opt(arg(long, value_name = "CHARS"))]
    pub button_hint_chars: String,

    #[config_alias(phantom_rendering = false)]
    #[config_opt(arg(long, group = "phantom_rendering_group"))]
    pub no_phantom_rendering: bool,

    #[config_alias(highlight_newlines = false)]
    #[config_opt(arg(long, group = "highlight_newlines_group"))]
    pub no_highlight_newlines: bool,

    #[config_alias(search_incremental = false)]
    #[config_opt(arg(long, group = "search_incremental_group"))]
    pub no_search_incremental: bool,

    #[config_alias(search_default_case_sensitivity = SearchCaseSensitivity::CaseInsensitive)]
    #[config_opt(arg(short = 'I', long, group = "search_default_case_sensitivity_group"))]
    pub search_default_case_insensitive: bool,

    #[config_alias(search_default_regexp = false)]
    #[config_opt(arg(long, group = "search_default_regexp_group"))]
    pub no_search_default_regexp: bool,

    #[config_alias(open_all_files = false)]
    #[config_opt(arg(long, group = "open_all_files_group"))]
    pub no_open_all_files: bool,

    #[config_alias(show_cursor = false)]
    #[config_opt(arg(long, group = "show_cursor_group"))]
    pub hide_cursor: bool,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            context_lines: 3,
            mouse_wheel_scroll_lines: 3,
            phantom_rendering: true,
            highlight_newlines: false,
            theme: new_theme(),
            clipboard_mechanism: ClipboardMechanism::Terminal,
            search_incremental: true,
            search_default_case_sensitivity: SearchCaseSensitivity::DependsOnPattern,
            search_default_regexp: true,
            open_all_files: true,
            show_cursor: true,
            button_hint_chars: "1234567890qwertyuiopasdfghjklzxcvbnm".to_owned(),
        }
    }
}
