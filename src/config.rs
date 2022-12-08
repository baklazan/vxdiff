use clap::{Args, ValueEnum};
use serde::Deserialize;
use std::path::PathBuf;
use tui::style::{Color, Modifier, Style};

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

#[derive(Debug, Deserialize)]
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
}

#[allow(dead_code)]
fn default_theme() -> Theme {
    let highlight = Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED);
    Theme {
        cursor: Style::default().fg(Color::White).bg(Color::Blue),
        line_numbers_default: Style::default(),
        line_numbers_phantom: Style::default().fg(Color::Cyan),
        text_equal: Style::default(),
        text_padding: Style::default().fg(Color::DarkGray),
        text_change_old: Style::default().fg(Color::Red),
        text_change_new: Style::default().fg(Color::Green),
        text_move_old: Style::default().fg(Color::Yellow),
        text_move_new: Style::default().fg(Color::Yellow),
        text_phantom_old: Style::default().fg(Color::Cyan).add_modifier(Modifier::DIM),
        text_phantom_new: Style::default().fg(Color::Cyan).add_modifier(Modifier::DIM),
        highlight_change_old: highlight,
        highlight_change_new: highlight,
        highlight_move_old: highlight,
        highlight_move_new: highlight,
        highlight_phantom_old: highlight,
        highlight_phantom_new: highlight,
        fabricated_symbol: Style::default().bg(Color::Cyan),
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
    let highlight = Style::default().add_modifier(Modifier::BOLD);
    Theme {
        cursor: Style::default().fg(Color::White).bg(Color::Blue),
        line_numbers_default: Style::default(),
        line_numbers_phantom: Style::default().fg(Color::Cyan),
        text_equal: Style::default().fg(white).bg(black),
        text_padding: Style::default().fg(Color::DarkGray).bg(Color::Indexed(238)),
        text_change_old: Style::default().fg(white).bg(darkest_red),
        text_change_new: Style::default().fg(white).bg(darkest_green),
        text_move_old: Style::default().fg(white).bg(darkest_yellow),
        text_move_new: Style::default().fg(white).bg(darkest_yellow),
        text_phantom_old: Style::default().fg(white).bg(darkest_cyan).add_modifier(Modifier::DIM),
        text_phantom_new: Style::default().fg(white).bg(darkest_cyan).add_modifier(Modifier::DIM),
        highlight_change_old: highlight.bg(Color::Indexed(88)),
        highlight_change_new: highlight.bg(Color::Indexed(28)),
        highlight_move_old: highlight.bg(Color::Indexed(100)),
        highlight_move_new: highlight.bg(Color::Indexed(100)),
        highlight_phantom_old: highlight.bg(Color::Indexed(30)),
        highlight_phantom_new: highlight.bg(Color::Indexed(30)),
        fabricated_symbol: highlight.fg(Color::Cyan),
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

        #[derive(Args, Debug, Deserialize)]
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

    #[config_alias(phantom_rendering = false)]
    #[config_opt(arg(long, group = "phantom_rendering_group"))]
    pub no_phantom_rendering: bool,

    #[config_alias(highlight_newlines = false)]
    #[config_opt(arg(long, group = "highlight_newlines_group"))]
    pub no_highlight_newlines: bool,

    #[config_alias(search_incremental = false)]
    #[config_opt(arg(long, group = "search_incremental_group"))]
    pub no_search_incremental: bool,

    #[config_alias(search_default_regexp = false)]
    #[config_opt(arg(long, group = "search_default_regexp_group"))]
    pub no_search_default_regexp: bool,

    #[config_alias(search_default_case_sensitivity = SearchCaseSensitivity::CaseInsensitive)]
    #[config_opt(arg(short = 'I', long, group = "search_default_case_sensitivity_group"))]
    pub search_default_case_insensitive: bool,
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
        }
    }
}
