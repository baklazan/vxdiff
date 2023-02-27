use clap::{Args, ValueEnum};
use serde::Deserialize;
use std::ffi::OsString;
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
pub struct DiffStyles {
    pub equal: Style,
    pub padding: Style,
    pub change_old: Style,
    pub change_new: Style,
    pub move_old: Style,
    pub move_new: Style,
    pub phantom_old: Style,
    pub phantom_new: Style,
}

#[derive(Deserialize)]
pub struct Theme {
    pub cursor: Style,
    pub line_numbers: DiffStyles,
    pub text: DiffStyles,
    pub highlight: DiffStyles,
    pub fabricated_symbol: Style,
    pub search_highlight: Style,
    pub select_highlight: Style,
    pub middle_separator: Style,
    pub expander: Style,
    pub file_header_closed: Style,
    pub file_header_open: Style,
    pub button: Style,
    pub button_hint: Style,
    pub line_number_hint: Style,
    pub line_number_half_hint: Style,
}

macro_rules! style {
    ( $( $field:ident = $value:expr ),* ) => { Style { $( $field: Some($value), )* ..Style::default() } }
}

#[allow(dead_code)]
fn default_theme() -> Theme {
    let highlight = style!(bold = true, underlined = true);
    Theme {
        cursor: style!(fg = Color::White, bg = Color::Blue),
        line_numbers: DiffStyles {
            equal: style!(),
            padding: style!(),
            change_old: style!(),
            change_new: style!(),
            move_old: style!(),
            move_new: style!(),
            phantom_old: style!(fg = Color::Cyan),
            phantom_new: style!(fg = Color::Cyan),
        },
        text: DiffStyles {
            equal: style!(),
            padding: style!(fg = Color::DarkGray),
            change_old: style!(fg = Color::Red),
            change_new: style!(fg = Color::Green),
            move_old: style!(fg = Color::Yellow),
            move_new: style!(fg = Color::Yellow),
            phantom_old: style!(fg = Color::Cyan, dim = true),
            phantom_new: style!(fg = Color::Cyan, dim = true),
        },
        highlight: DiffStyles {
            equal: highlight,
            padding: highlight,
            change_old: highlight,
            change_new: highlight,
            move_old: highlight,
            move_new: highlight,
            phantom_old: highlight,
            phantom_new: highlight,
        },
        fabricated_symbol: style!(bg = Color::Cyan),
        search_highlight: style!(fg = Color::Black, bg = Color::Yellow),
        select_highlight: style!(fg = Color::Black, bg = Color::White),
        middle_separator: style!(),
        expander: style!(),
        file_header_closed: style!(fg = Color::Black, bg = Color::Red),
        file_header_open: style!(fg = Color::Black, bg = Color::Green),
        button: style!(fg = Color::Black, bg = Color::White),
        button_hint: style!(fg = Color::White, bg = Color::Blue, bold = true),
        line_number_hint: style!(bold = true),
        line_number_half_hint: style!(fg = Color::Yellow, bold = true),
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
        line_numbers: DiffStyles {
            equal: style!(),
            padding: style!(),
            change_old: style!(),
            change_new: style!(),
            move_old: style!(),
            move_new: style!(),
            phantom_old: style!(fg = Color::Cyan),
            phantom_new: style!(fg = Color::Cyan),
        },
        text: DiffStyles {
            equal: style!(fg = white, bg = black),
            padding: style!(fg = Color::DarkGray, bg = Color::Indexed(238)),
            change_old: style!(fg = white, bg = darkest_red),
            change_new: style!(fg = white, bg = darkest_green),
            move_old: style!(fg = white, bg = darkest_yellow),
            move_new: style!(fg = white, bg = darkest_yellow),
            phantom_old: style!(fg = white, bg = darkest_cyan, dim = true),
            phantom_new: style!(fg = white, bg = darkest_cyan, dim = true),
        },
        highlight: DiffStyles {
            equal: style!(),
            padding: style!(),
            change_old: style!(bg = Color::Indexed(88), bold = true),
            change_new: style!(bg = Color::Indexed(28), bold = true),
            move_old: style!(bg = Color::Indexed(100), bold = true),
            move_new: style!(bg = Color::Indexed(100), bold = true),
            phantom_old: style!(bg = Color::Indexed(30), bold = true, dim = false),
            phantom_new: style!(bg = Color::Indexed(30), bold = true, dim = false),
        },
        fabricated_symbol: style!(fg = Color::Cyan, bold = true),
        search_highlight: style!(fg = Color::Black, bg = Color::Yellow),
        select_highlight: style!(fg = Color::Black, bg = Color::White),
        middle_separator: style!(),
        expander: style!(),
        file_header_closed: style!(fg = Color::Black, bg = Color::Red),
        file_header_open: style!(fg = Color::Black, bg = Color::Green),
        button: style!(fg = black, bg = Color::Indexed(195)), // #D7FFFF
        button_hint: style!(fg = black, bg = Color::Indexed(123), bold = true), // #87FFFF
        line_number_hint: style!(bold = true),
        line_number_half_hint: style!(fg = Color::Yellow, bold = true),
    }
}

#[allow(dead_code)]
fn light_theme() -> Theme {
    Theme {
        cursor: style!(fg = Color::Blue),
        line_numbers: DiffStyles {
            equal: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            padding: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            change_old: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            change_new: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            move_old: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            move_new: style!(fg = Color::Rgb(95, 99, 104), bg = Color::Rgb(248, 249, 250)),
            // TODO
            phantom_old: style!(fg = Color::Cyan),
            phantom_new: style!(fg = Color::Cyan),
        },
        text: DiffStyles {
            equal: style!(fg = Color::Rgb(32, 33, 36), bg = Color::Rgb(255, 255, 255)),
            // TODO text_padding fg
            padding: style!(fg = Color::Rgb(190, 191, 192), bg = Color::Rgb(248, 249, 250)),
            change_old: style!(fg = Color::Rgb(32, 33, 36), bg = Color::Rgb(255, 235, 238)),
            change_new: style!(fg = Color::Rgb(32, 33, 36), bg = Color::Rgb(216, 254, 216)),
            // TODO
            move_old: style!(fg = Color::Yellow),
            move_new: style!(fg = Color::Yellow),
            phantom_old: style!(fg = Color::Cyan, dim = true),
            phantom_new: style!(fg = Color::Cyan, dim = true),
        },
        highlight: DiffStyles {
            equal: style!(bg = Color::Rgb(220, 220, 220)),
            padding: style!(),
            change_old: style!(bg = Color::Rgb(255, 205, 210)),
            change_new: style!(bg = Color::Rgb(170, 242, 170)),
            // TODO
            move_old: style!(bg = Color::Indexed(100), bold = true),
            move_new: style!(bg = Color::Indexed(100), bold = true),
            phantom_old: style!(bg = Color::Indexed(30), bold = true, dim = false),
            phantom_new: style!(bg = Color::Indexed(30), bold = true, dim = false),
        },
        fabricated_symbol: style!(fg = Color::Cyan, bold = true),
        search_highlight: style!(fg = Color::Black, bg = Color::Yellow),
        select_highlight: style!(fg = Color::Black, bg = Color::White),
        middle_separator: style!(fg = Color::Rgb(140, 140, 180), bg = Color::Rgb(248, 249, 250)),
        expander: style!(fg = Color::Rgb(200, 200, 200), bg = Color::Rgb(248, 249, 250)),
        file_header_closed: style!(fg = Color::Rgb(32, 33, 36), bg = Color::Rgb(172, 227, 246), bold = true),
        file_header_open: style!(fg = Color::Rgb(32, 33, 36), bg = Color::Rgb(112, 219, 255), bold = true),
        button: style!(fg = Color::Indexed(16), bg = Color::Indexed(195)), // #D7FFFF
        button_hint: style!(fg = Color::Indexed(16), bg = Color::Indexed(123), bold = true), // #87FFFF
        line_number_hint: style!(bg = Color::Rgb(220, 221, 222), bold = true),
        line_number_half_hint: style!(bg = Color::Rgb(240, 240, 100), bold = true),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum OutputMode {
    Debug,
    TuiPlain,
    Tui,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum DiffAlgorithm {
    Naive,
    LinesThenWords,
    MainThenMoved,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum SearchCaseSensitivity {
    CaseSensitive,
    CaseInsensitive,
    DependsOnPattern,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum, Deserialize)]
pub enum IgnoreWhitespace {
    None,
    Leading,
    All,
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
    #[config_opt(arg(short, long))]
    pub mode: OutputMode,

    #[config_opt(arg(short, long))]
    pub algorithm: DiffAlgorithm,

    #[config_opt(arg(long, value_name = "NUM"))]
    pub context_lines: usize,

    #[config_opt(arg(long, value_name = "NUM"))]
    pub mouse_wheel_scroll_lines: usize,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "phantom_rendering_group"))]
    pub phantom_rendering: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "highlight_newlines_group"))]
    pub highlight_newlines: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "syntax_highlighting_group"))]
    pub syntax_highlighting: bool,

    #[config_opt(arg(long))]
    pub syntax_dir: OsString,

    #[config_opt(arg(long))]
    pub syntax_theme: OsString,

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

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "true", value_name = "BOOL", group = "debug_sections_group"))]
    pub debug_sections: bool,

    #[config_opt(arg(long, require_equals = true, num_args = 0..=1, default_missing_value = "all", group = "ignore_whitespace_group"))]
    pub ignore_whitespace: IgnoreWhitespace,

    #[config_alias(phantom_rendering = false)]
    #[config_opt(arg(long, group = "phantom_rendering_group"))]
    pub no_phantom_rendering: bool,

    #[config_alias(highlight_newlines = false)]
    #[config_opt(arg(long, group = "highlight_newlines_group"))]
    pub no_highlight_newlines: bool,

    #[config_alias(syntax_highlighting = false)]
    #[config_opt(arg(long, group = "syntax_highlighting_group"))]
    pub no_syntax_highlighting: bool,

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

    #[config_alias(debug_sections = false)]
    #[config_opt(arg(long, group = "debug_sections_group"))]
    pub no_debug_sections: bool,

    #[config_alias(ignore_whitespace = IgnoreWhitespace::None)]
    #[config_opt(arg(long, group = "ignore_whitespace_group"))]
    pub no_ignore_whitespace: bool,

    #[config_alias(ignore_whitespace = IgnoreWhitespace::Leading)]
    #[config_opt(arg(long, group = "ignore_whitespace_group"))]
    pub ignore_leading_whitespace: bool,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            mode: OutputMode::Tui,
            algorithm: DiffAlgorithm::LinesThenWords,
            context_lines: 3,
            mouse_wheel_scroll_lines: 3,
            phantom_rendering: true,
            highlight_newlines: false,
            syntax_highlighting: true,
            syntax_dir: OsString::new(),
            // TODO: Find a better default theme.
            syntax_theme: OsString::from("InspiredGitHub"),
            theme: light_theme(),
            clipboard_mechanism: ClipboardMechanism::Terminal,
            search_incremental: true,
            search_default_case_sensitivity: SearchCaseSensitivity::DependsOnPattern,
            search_default_regexp: true,
            open_all_files: true,
            show_cursor: true,
            button_hint_chars: "1234567890qwertyuiopasdfghjklzxcvbnm".to_owned(),
            debug_sections: false,
            ignore_whitespace: IgnoreWhitespace::None,
        }
    }
}
