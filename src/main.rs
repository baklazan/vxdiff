use clap::{error::ErrorKind, ArgGroup, CommandFactory as _, Parser, ValueEnum};
use std::io::stdout;
use vxdiff::{
    algorithm::compute_diff,
    basic_terminal::{print, print_side_by_side},
    input::{read_as_git_pager, read_file_list, run_external_helper_for_git_diff, run_git_diff, ProgramInput},
    tui_terminal::{print_side_by_side_diff_plainly, run_in_terminal, run_tui},
    validate::{print_errors, validate},
    DynResult,
};

#[derive(Clone, ValueEnum, Debug)]
enum OutputMode {
    Debug,
    Unified,
    Side,
    TuiPlain,
    Tui,
}

#[derive(Clone, ValueEnum, Debug)]
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

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
#[command(group(ArgGroup::new("input").required(true)))]
struct Args {
    #[arg(short, long, default_value_t = OutputMode::Tui, value_enum)]
    mode: OutputMode,
    #[arg(short, long, default_value_t = DiffAlgorithm::MovingSeeds, value_enum)]
    algorithm: DiffAlgorithm,
    #[arg(group = "input", value_names = ["OLD1", "NEW1", "OLD2", "NEW2"])]
    files: Vec<String>,
    #[arg(long, group = "input", value_name = "GIT DIFF ARGS", num_args = .., allow_hyphen_values = true)]
    git: Option<Vec<String>>,
    #[arg(long, group = "input")]
    git_pager: bool,
    #[arg(long, group = "input", value_name = "?", num_args = 1.., allow_hyphen_values = true, exclusive = true)]
    git_external_diff: Vec<String>,
}

fn try_main() -> DynResult<()> {
    let args = Args::parse();

    let input: ProgramInput;

    if let Some(git_diff_args) = &args.git {
        run_git_diff(git_diff_args)?;
        return Ok(());
    }

    if !args.git_external_diff.is_empty() {
        run_external_helper_for_git_diff(&args.git_external_diff)?;
        return Ok(());
    }

    if args.git_pager {
        input = read_as_git_pager()?;
    } else if args.files.len() % 2 != 0 {
        Args::command()
            .error(ErrorKind::TooFewValues, "File count must be even")
            .exit();
    } else {
        input = read_file_list(&args.files)?;
    }

    if input.file_input.is_empty() {
        eprintln!("The diff is empty.");
        return Ok(());
    }

    fn borrow_array(arr: &[String; 2]) -> [&str; 2] {
        // TODO: array.each_ref() might help, but it's not in stable Rust yet.
        [&arr[0], &arr[1]]
    }
    let file_input: Vec<_> = input.file_input.iter().map(borrow_array).collect();
    let file_names: Vec<_> = input.file_names.iter().map(borrow_array).collect();

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

fn main() {
    // If main() itself returns Result, Rust prints the error with Debug, not Display.
    // See `impl Termination for Result<T, E>` in src/std/process.rs.
    // See also https://users.rust-lang.org/t/why-does-error-require-display-but-then-not-use-it/65273
    // Another workaround would be to use `main_error`.
    // Another workaround would be to use `anyhow` and return `anyhow::Error`.
    if let Err(e) = try_main() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}
