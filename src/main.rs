use std::fs::read_to_string;
use std::process::exit;

mod algorithm;
mod basic_terminal;
mod tui_terminal;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 4 {
        eprintln!("usage: {:?} {{debug|unified|side|tui}} {{OLD}} {{NEW}}", args[0]);
        exit(1);
    }

    let old = &read_to_string(&args[2])?;
    let new = &read_to_string(&args[3])?;

    let diff = algorithm::diff_file(old, new);

    let mode = args[1].to_str().unwrap_or("???");
    match mode {
        "debug" => println!("{diff:#?}"),
        "unified" => basic_terminal::print(&diff, &mut std::io::stdout())?,
        "side" => basic_terminal::print_side_by_side(&diff, &mut std::io::stdout())?,
        "tui" => tui_terminal::run_in_terminal(|terminal| tui_terminal::run_tui(&diff, terminal))?,
        _ => eprintln!("invalid mode: {mode}"),
    }

    Ok(())
}
