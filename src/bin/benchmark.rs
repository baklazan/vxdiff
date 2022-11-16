use std::{
    ffi::OsStr,
    fs::read_to_string,
    path::{Path, PathBuf},
};

use clap::{Parser, ValueEnum};
use regex::Regex;
use vxdiff::algorithm::{
    benchmark::{compute_optimal_score, run_algorithm, PreprocessedTestcase},
    MainSequenceAlgorithm,
};

#[derive(Debug)]
struct Testcase {
    left: PathBuf,
    right: PathBuf,
    score: PathBuf,
}

#[derive(Clone, Debug, ValueEnum)]
pub enum AlgorithmType {
    Naive,
    Seeds,
}

impl AlgorithmType {
    pub fn convert(&self) -> MainSequenceAlgorithm {
        match &self {
            AlgorithmType::Naive => MainSequenceAlgorithm::Naive,
            AlgorithmType::Seeds => MainSequenceAlgorithm::Seeds,
        }
    }
}

#[derive(Parser)]
struct Args {
    testcase_directory: String,

    #[arg(short, long, default_value_t = String::from(""))]
    filter: String,

    #[arg(value_enum)]
    algorithms: Vec<AlgorithmType>,
}

#[derive(Clone)]
struct AlgorithmStats {
    worst_input: Option<(String, String)>,
    worst_error: f64,
    total_error: f64,
    inputs_processed: usize,
}

impl AlgorithmStats {
    pub fn new() -> AlgorithmStats {
        AlgorithmStats {
            worst_input: None,
            worst_error: 0.0,
            total_error: 0.0,
            inputs_processed: 0,
        }
    }

    pub fn add_input(&mut self, error: f64, filenames: &(String, String)) {
        self.total_error += error;
        self.inputs_processed += 1;
        if error > self.worst_error {
            self.worst_error = error;
            self.worst_input = Some(filenames.clone());
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let regex = Regex::new(&args.filter)?;

    let mut testcases = vec![];
    let directory = Path::new(&args.testcase_directory);
    let files = std::fs::read_dir(directory)?;
    for file in files {
        let path = file?.path();
        if path.extension() != Some(OsStr::new("left")) {
            continue;
        }

        if path
            .file_stem()
            .and_then(OsStr::to_str)
            .map(|s: &str| regex.is_match(s))
            != Some(true)
        {
            continue;
        }
        let left = path;
        let mut right = left.clone();
        right.set_extension("right");
        if !right.exists() {
            continue;
        }

        let mut score = left.clone();
        score.set_extension("score");

        testcases.push(Testcase { left, right, score });
    }

    let mut stats = vec![AlgorithmStats::new(); args.algorithms.len()];

    for testcase in testcases {
        let filenames = (
            String::from(testcase.left.to_str().unwrap()),
            String::from(testcase.right.to_str().unwrap()),
        );
        println!("Compare {:?} vs {:?}", testcase.left, testcase.right);

        let left = read_to_string(testcase.left)?;
        let right = read_to_string(testcase.right)?;

        let preprocessed_testcase = PreprocessedTestcase::new(&left, &right);

        let optimal_score = if testcase.score.exists() {
            read_to_string(testcase.score)?.parse::<f64>()?
        } else {
            let score = compute_optimal_score(&preprocessed_testcase);
            std::fs::write(testcase.score, format!("{}", score))?;
            score
        };

        println!("Optimal score is {}", optimal_score);

        for (i, algorithm) in args.algorithms.iter().enumerate() {
            let score = run_algorithm(&preprocessed_testcase, algorithm.convert());
            stats[i].add_input(optimal_score - score, &filenames);
            println!(
                "Algorithm {:?} scores: {} [{}]",
                algorithm,
                score,
                score - optimal_score
            );
        }
    }

    println!("--------Summary----------");

    for (algorithm, stats) in args.algorithms.iter().zip(stats) {
        println!(
            "{:?}: Avg. error {}, worst input {:?} [error = {}]",
            algorithm,
            stats.total_error / stats.inputs_processed as f64,
            stats.worst_input,
            stats.worst_error
        );
    }

    Ok(())
}
