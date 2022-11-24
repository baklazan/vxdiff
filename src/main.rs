use clap::{error::ErrorKind, CommandFactory as _, Parser, ValueEnum};
use std::fs::read_to_string;
use std::io::stdout;
use vxdiff::tui_terminal::{print_side_by_side_diff_plainly, run_in_terminal, run_tui};
use vxdiff::{
    algorithm::compute_diff,
    basic_terminal::{print, print_side_by_side},
    validate::{print_errors, validate},
};

#[derive(Clone, ValueEnum)]
enum OutputMode {
    Debug,
    Unified,
    Side,
    TuiPlain,
    Tui,
}

#[derive(Clone, ValueEnum)]
pub enum DiffAlgorithm {
    Naive,
    MainSeeds,
    MovingSeeds,
    LinesThenWords,
}

impl DiffAlgorithm {
    pub fn convert(&self) -> vxdiff::algorithm::DiffAlgorithm {
        match &self {
            DiffAlgorithm::Naive => {
                vxdiff::algorithm::DiffAlgorithm::MainSequence(vxdiff::algorithm::MainSequenceAlgorithm::Naive)
            }
            DiffAlgorithm::MainSeeds => {
                vxdiff::algorithm::DiffAlgorithm::MainSequence(vxdiff::algorithm::MainSequenceAlgorithm::Seeds)
            }
            DiffAlgorithm::LinesThenWords => {
                vxdiff::algorithm::DiffAlgorithm::MainSequence(vxdiff::algorithm::MainSequenceAlgorithm::LinesThenWords)
            }
            DiffAlgorithm::MovingSeeds => vxdiff::algorithm::DiffAlgorithm::MovingSeeds,
        }
    }
}

#[derive(Parser)]
#[command(arg_required_else_help(true))]
struct Args {
    #[arg(short, long, default_value_t = OutputMode::Tui, value_enum)]
    mode: OutputMode,
    #[arg(short, long, default_value_t = DiffAlgorithm::MovingSeeds, value_enum)]
    algorithm: DiffAlgorithm,
    #[arg(required = true, value_names = ["OLD1", "NEW1", "OLD2", "NEW2"])]
    files: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let mut file_input_storage = vec![];
    let mut file_names_storage = vec![];

    if args.files.len() % 2 != 0 {
        Args::command()
            .error(ErrorKind::TooFewValues, "File count must be even")
            .exit();
    }

    for i in (0..args.files.len()).step_by(2) {
        let old_name = &args.files[i];
        let new_name = &args.files[i + 1];
        let old = read_to_string(old_name)?;
        let new = read_to_string(new_name)?;
        file_input_storage.push([old, new]);
        file_names_storage.push([old_name, new_name]);
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

    let diff = compute_diff(&file_input, args.algorithm.convert());

    print_errors(&validate(&diff, &file_input, &file_names));

    match args.mode {
        OutputMode::Debug => println!("{diff:#?}"),
        OutputMode::Unified => print(&diff, &file_input, &mut stdout())?,
        OutputMode::Side => print_side_by_side(&diff, &file_input, &mut stdout())?,
        OutputMode::TuiPlain => print_side_by_side_diff_plainly(&diff, &file_input, &file_names, &mut stdout())?,
        OutputMode::Tui => run_in_terminal(|terminal| run_tui(&diff, &file_input, &file_names, terminal))?,
    }

    Ok(())
}
