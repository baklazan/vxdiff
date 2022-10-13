use std::fs::read_to_string;
use std::io::stdout;
use std::process::exit;
use vxdiff::tui_terminal::{print_side_by_side_diff_plainly, run_in_terminal, run_tui};
use vxdiff::{
    algorithm::compute_diff,
    basic_terminal::{print, print_side_by_side},
    validate::{print_errors, validate},
};

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
    let mut file_names_storage = vec![];

    for i in (2..args.len()).step_by(2) {
        let old_name = &args[i];
        let new_name = &args[i + 1];
        let old = read_to_string(old_name)?;
        let new = read_to_string(new_name)?;
        file_input_storage.push([old, new]);
        file_names_storage.push([old_name.to_string_lossy(), new_name.to_string_lossy()]);
    }

    // TODO: array.each_ref() might help, but it's not in stable Rust yet.
    let file_input: Vec<[&str; 2]> = file_input_storage
        .iter()
        .map(|&[ref old, ref new]| -> [&str; 2] { [old, new] })
        .collect();
    let file_names: Vec<[&str; 2]> = file_names_storage
        .iter()
        .map(|&[ref old, ref new]| -> [&str; 2] { [old, new] })
        .collect();

    let diff = compute_diff(&file_input);

    print_errors(&validate(&diff, &file_input, &file_names));

    let mode = args[1].to_str().unwrap_or("???");
    match mode {
        "debug" => println!("{diff:#?}"),
        "unified" => print(&diff, &file_input, &mut stdout())?,
        "side" => print_side_by_side(&diff, &file_input, &mut stdout())?,
        "tuiplain" => print_side_by_side_diff_plainly(&diff, &file_input, &file_names, &mut stdout())?,
        "tui" => run_in_terminal(|terminal| run_tui(&diff, &file_input, &file_names, terminal))?,
        _ => eprintln!("invalid mode: {mode}"),
    }

    Ok(())
}
