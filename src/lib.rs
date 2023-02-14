pub mod algorithm;
pub mod config;
pub mod input;
pub mod tui_terminal;
pub mod validate;

pub type DynResult<T> = Result<T, Box<dyn std::error::Error>>;
