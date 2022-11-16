use crate::algorithm::{
    get_partitioned_subtext,
    scoring::{affine_scoring::AffineScoring, TScore},
    seed_selection::select_seeds,
    DiffOp, PartitionedText,
};

fn greedy_seeds_recursive(texts: &[[PartitionedText; 2]], prefix_scores: &[TScore], result: &mut Vec<DiffOp>) {
    let seeds = select_seeds(texts);

    let mut best = (TScore::NEG_INFINITY, None);
    for seed in seeds {
        let score = prefix_scores[seed.end[0]] - prefix_scores[seed.start[0]];
        if score > best.0 {
            best = (score, Some(seed));
        }
    }

    if best.1.is_none() {
        for _ in 0..texts[0][0].word_count() {
            result.push(DiffOp::Delete);
        }
        for _ in 0..texts[0][1].word_count() {
            result.push(DiffOp::Insert);
        }
        return;
    }

    let seed = best.1.unwrap();
    let texts_before = [[
        get_partitioned_subtext(&texts[0][0], 0..seed.start[0]),
        get_partitioned_subtext(&texts[0][1], 0..seed.start[1]),
    ]];
    greedy_seeds_recursive(&texts_before, &prefix_scores[0..=seed.start[0]], result);
    for _ in seed.start[0]..seed.end[0] {
        result.push(DiffOp::Match);
    }

    let texts_after = [[
        get_partitioned_subtext(&texts[0][0], seed.end[0]..texts[0][0].word_count()),
        get_partitioned_subtext(&texts[0][1], seed.end[1]..texts[0][1].word_count()),
    ]];
    greedy_seeds_recursive(
        &texts_after,
        &prefix_scores[seed.end[0]..=texts[0][0].word_count()],
        result,
    );
}

pub(in crate::algorithm) fn greedy_seeds(
    texts: &[PartitionedText; 2],
    scoring: &AffineScoring,
    file_ids: [usize; 2],
) -> Vec<DiffOp> {
    let mut result = vec![];

    let mut prefix_scores = vec![0.0];
    let mut score = 0.0;
    for value in scoring.information_values[file_ids[0]][0].iter() {
        score += value;
        prefix_scores.push(score);
    }

    greedy_seeds_recursive(&[[texts[0].clone(), texts[1].clone()]], &prefix_scores[..], &mut result);
    result
}
