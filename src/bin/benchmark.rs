use std::{
    ffi::OsStr,
    fs::read_to_string,
    path::{Path, PathBuf},
    process::exit,
};

use regex::Regex;
use vxdiff::algorithm::{main_sequence, preprocess::partition_into_words, DiffOp, PartitionedText};

#[derive(Debug)]
struct Testcase {
    left: PathBuf,
    right: PathBuf,
    score: PathBuf,
}

type AlignmentScoring = vxdiff::algorithm::scoring::AffineScoring;
type Algorithm = fn(&[PartitionedText; 2], &AlignmentScoring, [usize; 2]) -> Vec<DiffOp>;

fn naive_dp(_partitioned_texts: &[PartitionedText; 2], scoring: &AlignmentScoring, sizes: [usize; 2]) -> Vec<DiffOp> {
    main_sequence::naive_dp::naive_dp(scoring, sizes)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() < 2 || args.len() > 3 {
        eprintln!("usage: {:?} directory-with-test-cases [regex]", args[0]);
        exit(1);
    }

    let regex: Regex = if args.len() == 3 {
        Regex::new(args[2].to_str().ok_or("can't convert second argument to str")?)?
    } else {
        Regex::new("")?
    };

    let mut testcases = vec![];
    let directory = Path::new(&args[1]);
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

    let algorithms: [Algorithm; 1] = [naive_dp];

    for testcase in testcases {
        println!("Compare {:?} vs {:?}", testcase.left, testcase.right);

        let left = read_to_string(testcase.left)?;
        let right = read_to_string(testcase.right)?;
        let left_bounds = partition_into_words(&left);
        let right_bounds = partition_into_words(&right);
        let all_texts = [[
            PartitionedText {
                text: &left,
                word_bounds: &left_bounds,
            },
            PartitionedText {
                text: &right,
                word_bounds: &right_bounds,
            },
        ]];
        let file_texts = &all_texts[0];

        let scoring = AlignmentScoring::new(&all_texts);
        let sizes = [file_texts[0].word_count(), file_texts[1].word_count()];

        let optimal_score = if testcase.score.exists() {
            read_to_string(testcase.score)?.parse::<f64>()?
        } else {
            let score = main_sequence::naive_dp::compute_score(&scoring, sizes);
            std::fs::write(testcase.score, format!("{}", score))?;
            score
        };

        println!("Input size is {:?}", sizes);
        println!("Optimal score is {}", optimal_score);

        for algorithm in algorithms {
            let alignment = algorithm(&all_texts[0], &scoring, sizes);
            println!("score: {:?}", scoring.alignment_score(&alignment, [0, 0], [0, 0]));
        }
    }

    Ok(())
}
