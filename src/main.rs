use std::io;

mod algorithm;
mod basic_terminal;

fn main() -> io::Result<()> {
    let old = vec!["hello", "world", "!"];
    let new = vec!["hell", "world", "!"];
    let result = algorithm::diff(&old[..], &new[..]);
    println!("{:?}", result);

    let diff = algorithm::experiment(&old[..], &new[..]);
    println!("{:?}", diff);

    basic_terminal::print(&diff, &mut std::io::stdout())?;

    Ok(())
}
