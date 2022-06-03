use std::fs::read_to_string;
use std::io;
use std::process::exit;

mod algorithm;
mod basic_terminal;

fn main() -> io::Result<()> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("usage: {:?} [old] [new]", args[0]);
        exit(1);
    }

    let old = &read_to_string(&args[1])?;
    let new = &read_to_string(&args[2])?;

    let diff = algorithm::diff_file(old, new);
    println!("{:?}", diff);

    basic_terminal::print(&diff, &mut std::io::stdout())?;

    Ok(())
}
