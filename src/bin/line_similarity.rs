use std::{ffi::OsStr, fs::read_to_string, path::Path};

use clap::Parser;
use regex::Regex;

#[derive(Parser)]
struct Args {
    testcase_directory: String,

    #[arg(short, long, default_value_t = String::from(""))]
    filter: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let regex = Regex::new(&args.filter)?;

    let directory = Path::new(&args.testcase_directory);
    let files = std::fs::read_dir(directory)?;

    let mut inputs: Vec<[String; 2]> = vec![];
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

        inputs.push([read_to_string(left)?, read_to_string(right)?]);
    }
    let borrowed_inputs: Vec<[&str; 2]> = inputs.iter().map(|file| [&file[0][..], &file[1][..]]).collect();

    let samples = vxdiff::algorithm::line_samples::generate_samples(&borrowed_inputs);

    println!("{}", samples.len());
    for sample in samples.iter() {
        println!(
            "{} {} {} {} {}",
            sample.texts[0].len(),
            sample.texts[1].len(),
            sample.individual_values[0],
            sample.individual_values[1],
            sample.common_score
        );
        print!("{}{}", sample.texts[0], sample.texts[1]);
    }

    Ok(())
}
