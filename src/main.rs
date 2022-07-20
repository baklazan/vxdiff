use std::fs::read_to_string;
use std::process::exit;

mod algorithm;
mod basic_terminal;
mod tui_terminal;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("usage: {:?} [old] [new]", args[0]);
        exit(1);
    }

    let old = &read_to_string(&args[1])?;
    let new = &read_to_string(&args[2])?;

    let diff = algorithm::diff_file(old, new);

    println!("Normal print:");
    basic_terminal::print(&diff, &mut std::io::stdout())?;

    println!("Side by side print:");
    basic_terminal::print_side_by_side(&diff, &mut std::io::stdout())?;

    Ok(())
}
