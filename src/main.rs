use std::io;

mod algorithm;
mod basic_terminal;

fn main() -> io::Result<()> {
    let old = "hello\nworld\n!\n";
    let new = "hell\nworld\n!\n";

    let diff = algorithm::diff_file(old, new);
    println!("{:?}", diff);

    basic_terminal::print(&diff, &mut std::io::stdout())?;

    Ok(())
}
