use std::fs::read;
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

    let old_bytes = read(&args[1])?;
    let new_bytes = read(&args[2])?;
    // TODO: Convert errors instead of unwrap()???
    let old = std::str::from_utf8(&old_bytes).unwrap();
    let new = std::str::from_utf8(&new_bytes).unwrap();

    let diff = algorithm::diff_file(old, new);
    println!("{:?}", diff);

    basic_terminal::print(&diff, &mut std::io::stdout())?;

    Ok(())
}
