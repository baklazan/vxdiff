use crate::algorithm::{
    get_partitioned_subtext,
    scoring::{affine_scoring::AffineWordScoring, TScore},
    seed_selection::select_seeds,
    DiffOp, PartitionedText,
};

fn greedy_seeds_recursive(text_words: &[[PartitionedText; 2]], prefix_scores: &[TScore], result: &mut Vec<DiffOp>) {
    let seeds = select_seeds(text_words);

    let mut best = (TScore::NEG_INFINITY, None);
    for seed in seeds {
        let score = prefix_scores[seed.end[0]] - prefix_scores[seed.start[0]];
        if score > best.0 {
            best = (score, Some(seed));
        }
    }

    if best.1.is_none() {
        for _ in 0..text_words[0][0].part_count() {
            result.push(DiffOp::Delete);
        }
        for _ in 0..text_words[0][1].part_count() {
            result.push(DiffOp::Insert);
        }
        return;
    }

    let seed = best.1.unwrap();
    let text_words_before = [[
        get_partitioned_subtext(&text_words[0][0], 0..seed.start[0]),
        get_partitioned_subtext(&text_words[0][1], 0..seed.start[1]),
    ]];
    greedy_seeds_recursive(&text_words_before, &prefix_scores[0..=seed.start[0]], result);
    for _ in seed.start[0]..seed.end[0] {
        result.push(DiffOp::Match);
    }

    let text_words_after = [[
        get_partitioned_subtext(&text_words[0][0], seed.end[0]..text_words[0][0].part_count()),
        get_partitioned_subtext(&text_words[0][1], seed.end[1]..text_words[0][1].part_count()),
    ]];
    greedy_seeds_recursive(
        &text_words_after,
        &prefix_scores[seed.end[0]..=text_words[0][0].part_count()],
        result,
    );
}

pub(in crate::algorithm) fn greedy_seeds(
    text_words: &[[PartitionedText; 2]],
    scoring: &AffineWordScoring,
) -> Vec<Vec<DiffOp>> {
    let mut result = vec![];
    for (file_id, file_text_words) in text_words.iter().enumerate() {
        let mut alignment = vec![];
        let mut prefix_scores = vec![0.0];
        let mut score = 0.0;
        for value in scoring.information_values[file_id][0].iter() {
            score += value;
            prefix_scores.push(score);
        }
        greedy_seeds_recursive(&[file_text_words.clone()], &prefix_scores[..], &mut alignment);
        result.push(alignment);
    }
    result
}
