use super::{
    main_sequence::naive_dp,
    preprocess::{partition_into_lines, partition_into_words},
    scoring::{
        simple::{zero_or_information::ZeroOrInformationScoring, SimpleScoring},
        AlignmentSliceScoring, DpDirection, InputSliceBounds, TScore,
    },
    PartitionedText,
};

use rand::Rng;

pub struct Sample<'a> {
    pub texts: [&'a str; 2],
    pub individual_values: [TScore; 2],
    pub common_score: TScore,
}

fn partition_correspondence(coarse: &[usize], fine: &[usize]) -> Vec<usize> {
    let mut result = vec![];
    let mut coarse_index = 0;
    for (fine_index, &fine_value) in fine.iter().enumerate() {
        if fine_value == coarse[coarse_index] {
            result.push(fine_index);
            coarse_index += 1;
        }
    }
    assert!(result.len() == coarse.len());
    result
}

pub fn generate_samples<'a>(texts: &'a [[&str; 2]]) -> Vec<Sample<'a>> {
    let word_bounds: Vec<[Vec<usize>; 2]> = texts.iter().map(|file| file.map(partition_into_words)).collect();
    let line_bounds: Vec<[Vec<usize>; 2]> = texts.iter().map(|file| file.map(partition_into_lines)).collect();

    let mut line_bound_word_indices: Vec<[Vec<usize>; 2]> = vec![];
    for file_id in 0..texts.len() {
        line_bound_word_indices.push(
            [0, 1].map(|side| partition_correspondence(&line_bounds[file_id][side], &word_bounds[file_id][side])),
        );
    }

    let text_words: Vec<[PartitionedText; 2]> = (0..texts.len())
        .map(|file_id| {
            [0, 1].map(|side| PartitionedText {
                text: texts[file_id][side],
                part_bounds: &word_bounds[file_id][side],
            })
        })
        .collect();

    let word_scoring = SimpleScoring {
        match_scoring: ZeroOrInformationScoring::new(&text_words),
    };

    const IOU_BUCKETS: usize = 5;
    const LEN_SUM_BUCKETS: usize = 5;
    const LEN_SUM_BUCKET_SIZE: usize = 50;
    const SAMPLES_PER_BUCKET: usize = 1000;

    let mut buckets_done = 0;
    let mut in_bucket = [[0; LEN_SUM_BUCKETS]; IOU_BUCKETS];
    let mut rng = rand::thread_rng();
    let mut result = vec![];
    while buckets_done < IOU_BUCKETS * LEN_SUM_BUCKETS {
        let file_id = rng.gen_range(0..texts.len());
        let lines = [0, 1].map(|side| rng.gen_range(0..(line_bounds[file_id][side].len() - 1)));
        let word_indices_start = [0, 1].map(|side| line_bound_word_indices[file_id][side][lines[side]]);
        let word_indices_end = [0, 1].map(|side| line_bound_word_indices[file_id][side][lines[side] + 1]);

        let slice = InputSliceBounds {
            file_ids: [file_id, file_id],
            start: word_indices_start,
            direction: DpDirection::Forward,
            size: [0, 1].map(|side| word_indices_end[side] - word_indices_start[side]),
        };

        let text_slices = [0, 1].map(|side| {
            &texts[file_id][side][line_bounds[file_id][side][lines[side]]..line_bounds[file_id][side][lines[side] + 1]]
        });

        let common_score = naive_dp::compute_score(&AlignmentSliceScoring {
            slice,
            scoring: &word_scoring,
        });

        let individual_values = [0, 1].map(|side| {
            word_scoring.match_scoring.range_information_value(
                file_id,
                side,
                word_indices_start[side],
                word_indices_end[side],
            )
        });

        let iou = common_score / (individual_values[0] + individual_values[1] - common_score);
        let iou_bucket = usize::min((iou * IOU_BUCKETS as f64) as usize, IOU_BUCKETS - 1);
        let line_lengths =
            [0, 1].map(|side| line_bounds[file_id][side][lines[side] + 1] - line_bounds[file_id][side][lines[side]]);
        let length_sum = line_lengths[0] + line_lengths[1];
        let length_sum_bucket = usize::min(length_sum / LEN_SUM_BUCKET_SIZE, LEN_SUM_BUCKETS - 1);

        if in_bucket[length_sum_bucket][iou_bucket] < SAMPLES_PER_BUCKET {
            in_bucket[length_sum_bucket][iou_bucket] += 1;
            result.push(Sample {
                texts: text_slices,
                individual_values,
                common_score,
            });

            if in_bucket[length_sum_bucket][iou_bucket] == SAMPLES_PER_BUCKET {
                buckets_done += 1;
            }

            for i in 0..LEN_SUM_BUCKETS {
                for j in 0..IOU_BUCKETS {
                    eprint!("{} ", in_bucket[i][j]);
                }
                eprintln!();
            }
            eprintln!();
        }
    }

    result
}
