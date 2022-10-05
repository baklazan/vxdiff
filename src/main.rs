use std::fs::read_to_string;
use std::process::exit;

mod algorithm;
mod basic_terminal;
mod tui_terminal;
mod validate;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() < 4 || args.len() % 2 != 0 {
        eprintln!(
            "usage: {:?} {}",
            args[0], "{debug|unified|side|tui} {OLD} {NEW} [{OLD2} {NEW2} ...]"
        );
        exit(1);
    }

    let mut file_input_storage = vec![];
    let mut diff = Default::default();

    for i in (2..args.len()).step_by(2) {
        let old = read_to_string(&args[i])?;
        let new = read_to_string(&args[i + 1])?;
        let file_diff = algorithm::diff_file(&old, &new);
        algorithm::merge_diffs(&mut diff, file_diff);
        file_input_storage.push([old, new]);
    }

    // TODO: array.each_ref() might help, but it's not in stable Rust yet.
    let file_input: Vec<[&str; 2]> = file_input_storage
        .iter()
        .map(|&[ref old, ref new]| -> [&str; 2] { [old, new] })
        .collect();

    validate::print_errors(&validate::validate(&diff, &file_input));

    let mode = args[1].to_str().unwrap_or("???");
    match mode {
        "debug" => println!("{diff:#?}"),
        "unified" => basic_terminal::print(&diff, &file_input, &mut std::io::stdout())?,
        "side" => basic_terminal::print_side_by_side(&diff, &file_input, &mut std::io::stdout())?,
        "tuiplain" => tui_terminal::print_side_by_side_diff_plainly(&diff, &file_input, &mut std::io::stdout())?,
        "tui" => tui_terminal::run_in_terminal(|terminal| tui_terminal::run_tui(&diff, &file_input, terminal))?,
        _ => eprintln!("invalid mode: {mode}"),
    }

    Ok(())
}
