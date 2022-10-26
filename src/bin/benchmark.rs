use std::{
    ffi::OsStr,
    fs::read_to_string,
    path::{Path, PathBuf},
    process::exit,
};

use vxdiff::algorithm::{main_sequence, preprocess::partition_into_words, DiffOp, PartitionedText};

#[derive(Debug)]
struct Testcase {
    left: PathBuf,
    right: PathBuf,
    score: PathBuf,
}

type AlignmentScoring = vxdiff::algorithm::scoring::AffineScoring;
type Algorithm = fn(&AlignmentScoring, [usize; 2]) -> Vec<DiffOp>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 2 {
        eprintln!("usage: {:?} directory-with-test-cases", args[0]);
        exit(1);
    }

    let mut testcases = vec![];
    let directory = Path::new(&args[1]);
    let files = std::fs::read_dir(directory)?;
    for file in files {
        let path = file?.path();
        if path.extension() != Some(OsStr::new("left")) {
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

    let algorithms: [Algorithm; 1] = [main_sequence::naive_dp::naive_dp];

    for testcase in testcases {
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
            let alignment = algorithm(&scoring, sizes);
            println!("score: {:?}", scoring.alignment_score(&alignment, [0, 0], [0, 0]));
        }
    }

    Ok(())
}
