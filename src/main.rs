use anyhow::{Context as _, Result};
use clap::{error::ErrorKind, ArgGroup, CommandFactory as _, Parser};
use std::io::stdout;
use std::path::PathBuf;
use vxdiff::{
    algorithm::{compute_diff, DiffAlgorithm, LineScoringStrategy, MainSequenceAlgorithm},
    config::{Config, ConfigOpt, DiffAlgorithm as DiffAlgorithmArg, OutputMode},
    input::{
        read_as_git_pager, read_by_running_git_diff_raw, read_file_list, run_external_helper_for_git_diff,
        run_git_diff, ProgramInput,
    },
    tui_terminal::{print_noninteractive_diff, run_in_terminal, run_tui},
    validate::{print_errors, validate},
};

fn convert_algorithm(algorithm: DiffAlgorithmArg) -> DiffAlgorithm {
    match algorithm {
        DiffAlgorithmArg::Naive => DiffAlgorithm::MainSequence(MainSequenceAlgorithm::Naive),
        DiffAlgorithmArg::LinesThenWords => {
            DiffAlgorithm::MainSequence(MainSequenceAlgorithm::LinesThenWords(LineScoringStrategy::KGram))
        }
        DiffAlgorithmArg::MainThenMoved => {
            DiffAlgorithm::MainThenMoved(MainSequenceAlgorithm::LinesThenWords(LineScoringStrategy::KGram))
        }
    }
}

#[derive(Parser)]
#[command(arg_required_else_help(true))]
#[command(group(ArgGroup::new("input").required(true)))]
struct Args {
    #[arg(group = "input", value_names = ["OLD1", "NEW1", "OLD2", "NEW2"])]
    files: Vec<String>,

    #[arg(long, group = "input", value_name = "GIT DIFF ARGS", num_args = .., allow_hyphen_values = true)]
    git: Option<Vec<String>>,

    #[arg(long, group = "input", value_name = "GIT DIFF ARGS", num_args = .., allow_hyphen_values = true)]
    git_old: Option<Vec<String>>,

    #[arg(long, group = "input")]
    git_pager: bool,

    #[arg(long, group = "input", value_name = "?", num_args = 1.., allow_hyphen_values = true, exclusive = true)]
    git_external_diff: Vec<String>,

    #[arg(long, hide = true)]
    git_pager_hack: bool,

    #[arg(long)]
    config_file: Option<PathBuf>,

    #[command(flatten)]
    config: ConfigOpt,
}

fn try_main() -> Result<()> {
    let args = Args::parse();

    let args = if args.git_pager_hack {
        assert!(args.git_old.is_some());
        assert!(!args.git_pager);
        Args {
            git_old: None,
            git_pager: true,
            ..args
        }
    } else {
        args
    };

    if let Some(git_diff_args) = &args.git_old {
        let current_exe = std::env::current_exe().context("Failed to get vxdiff program path")?;
        let current_exe = current_exe.to_str().context("current_exe is not valid UTF-8")?;
        let pager_args: Vec<String> = std::env::args().skip(1).collect();
        run_git_diff(current_exe, git_diff_args, &pager_args)?;
        return Ok(());
    }

    if !args.git_external_diff.is_empty() {
        run_external_helper_for_git_diff(&args.git_external_diff).context("Failed in 'vxdiff --git-external-diff'")?;
        return Ok(());
    }

    let mut config = Config::default();

    let config_file_default = || dirs::config_dir().map(|c| c.join("vxdiff").join("config.toml"));
    let config_file = args.config_file.clone().or_else(config_file_default);
    if let Some(config_file) = config_file {
        match std::fs::read(&config_file) {
            Ok(config_binary_content) => {
                let config_text_content = std::str::from_utf8(&config_binary_content)
                    .with_context(|| format!("Config file '{}' is not valid UTF-8", config_file.display()))?;
                let config_parsed = toml::from_str(config_text_content)
                    .with_context(|| format!("Failed parsing TOML config file '{}'", config_file.display()))?;
                config = config.update(config_parsed);
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(e).context(format!("Failed to read config file '{}'", config_file.display())),
        }
    }

    config = config.update(args.config);

    let input: ProgramInput;

    if let Some(git_diff_args) = &args.git {
        input = read_by_running_git_diff_raw(git_diff_args).context("Failed to read input in 'vxdiff --git'")?;
    } else if args.git_pager {
        input = read_as_git_pager().context("Failed to read input in 'vxdiff --git-pager'")?;
    } else if args.files.len() % 2 != 0 {
        Args::command()
            .error(ErrorKind::TooFewValues, "File count must be even")
            .exit();
    } else {
        input = read_file_list(&args.files).context("Failed to read input file list")?;
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
    let file_status_text: Vec<&str> = input.file_status_text.iter().map(AsRef::as_ref).collect();

    let algorithm = convert_algorithm(config.algorithm);
    let diff = compute_diff(&file_input, algorithm);

    print_errors(&validate(&diff, &file_input, &file_names));

    match config.mode {
        OutputMode::Debug => println!("{diff:#?}"),
        OutputMode::TuiPlain => print_noninteractive_diff(
            diff,
            config,
            &file_input,
            &file_names,
            &file_status_text,
            algorithm,
            &mut stdout(),
        )
        .context("Failed to print diff")?,
        OutputMode::Tui => run_in_terminal(|terminal| {
            run_tui(
                diff,
                config,
                &file_input,
                &file_names,
                &file_status_text,
                algorithm,
                terminal,
            )
        })
        .context("Error in terminal UI")?,
    }

    Ok(())
}

fn main() {
    // If main() itself returns Result, Rust prints the error with Debug, not Display.
    // See `impl Termination for Result<T, E>` in src/std/process.rs.
    // See also https://users.rust-lang.org/t/why-does-error-require-display-but-then-not-use-it/65273
    //
    // Using `anyhow::Result` and `anyhow::Error` partly solves this problem because their
    // implementation of Debug just calls the inner error's Display. But its {:#} format looks nicer
    // than {:?}. So we still have both main() and try_main().
    // See https://docs.rs/anyhow/latest/anyhow/struct.Error.html#display-representations
    if let Err(e) = try_main() {
        if std::env::var("RUST_LIB_BACKTRACE") == Ok("1".to_string())
            || std::env::var("RUST_BACKTRACE") == Ok("1".to_string())
        {
            eprintln!("Error: {e:?}");
        } else {
            eprintln!("Error: {e:#}");
        }
        std::process::exit(1);
    }
}
