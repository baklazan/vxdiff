use std::collections::HashMap;

use float_ord::FloatOrd;
use rand::Rng;
use rand::SeedableRng;

use crate::algorithm::{
    preprocess::{internalize_parts, part_values, CharScorer},
    scoring::TScore,
    PartitionedText,
};

use super::MatchScoring;

pub struct KGramSamplingScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    bitvectors: Vec<[Vec<[u128; Self::BITVECTOR_LENGTH / 128]>; 2]>,
    ones_counts: Vec<[Vec<usize>; 2]>,
    part_scores: Vec<[Vec<TScore>; 2]>,
    negative_score_multiplier: f64,
}

const HASH_MOD_P: u64 = 1000000009;

impl KGramSamplingScoring {
    const K: usize = 3;
    const BITVECTOR_LENGTH: usize = 128;
    const SAMPLES: usize = 25;
    const ROUND_DOWN_CORRECTED_HITS: f64 = 5.0;
    const MAX_NONEXACT_SCORE_RATIO: TScore = 0.9;

    fn priority(hash: u64, information_score: TScore) -> FloatOrd<f64> {
        let r = (hash as f64 + 0.5) / (HASH_MOD_P as f64);
        FloatOrd(r.ln() / information_score)
    }

    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]], zero_at_similarity: f64) -> Self {
        const BASE_MOD_P: u64 = (1u64 << 32) % HASH_MOD_P;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);

        let hash_factors: Vec<u64> = (0..Self::SAMPLES).map(|_| rng.gen_range(1..HASH_MOD_P)).collect();
        let hash_additives: Vec<u64> = (0..Self::SAMPLES).map(|_| rng.gen_range(0..HASH_MOD_P)).collect();

        let mut powers_of_base = vec![1];
        for i in 1..=(Self::K) {
            powers_of_base.push((powers_of_base[i - 1] * BASE_MOD_P) % HASH_MOD_P);
        }

        let mut bitvectors = vec![];
        let mut ones_counts = vec![];

        let char_scorer = CharScorer::from_texts(text_parts);
        for file_texts_parts in text_parts.iter() {
            let mut file_bitvectors = [vec![], vec![]];
            let mut file_ones_counts = [vec![], vec![]];
            for (side, side_parts) in file_texts_parts.iter().enumerate() {
                for part_index in 0..side_parts.part_count() {
                    let mut value_seen_counts: HashMap<u64, usize> = HashMap::new();

                    let mut prefix_values = vec![0];
                    let mut current_prefix_value = 0;
                    let mut bytes_read = 0;
                    let mut prefix_char_scores = vec![0.0];
                    let mut current_prefix_char_score = 0.0;

                    let mut best_hashes = vec![(FloatOrd(f64::NEG_INFINITY), 0); Self::SAMPLES];

                    let part = side_parts.get_part(part_index);
                    for (char_index, c) in part.chars().enumerate() {
                        current_prefix_value *= BASE_MOD_P;
                        current_prefix_value += c as u64;
                        current_prefix_value %= HASH_MOD_P;

                        prefix_values.push(current_prefix_value);
                        bytes_read += c.len_utf8();
                        current_prefix_char_score += char_scorer.score(c);
                        prefix_char_scores.push(current_prefix_char_score);

                        if char_index + 1 >= Self::K || bytes_read >= part.len() {
                            let char_length = usize::min(Self::K, char_index + 1);
                            let start_index = char_index + 1 - char_length;
                            let value = (current_prefix_value + HASH_MOD_P
                                - (prefix_values[start_index] * powers_of_base[char_length]) % HASH_MOD_P)
                                % HASH_MOD_P;

                            let seen_count = *value_seen_counts.get(&value).unwrap_or(&0);
                            let value_with_seen =
                                (value * BASE_MOD_P + u64::try_from(seen_count).unwrap()) % HASH_MOD_P;
                            value_seen_counts.insert(value, seen_count + 1);

                            let information_score = current_prefix_char_score - prefix_char_scores[start_index];
                            for i in 0..Self::SAMPLES {
                                let hash = (value_with_seen * hash_factors[i] + hash_additives[i]) % HASH_MOD_P;
                                best_hashes[i] =
                                    std::cmp::max(best_hashes[i], (Self::priority(hash, information_score), hash));
                            }
                        }
                    }

                    let mut bitvector = [0; Self::BITVECTOR_LENGTH / 128];
                    let mut ones_count = 0;
                    for (_, hash) in best_hashes {
                        let bit_index = usize::try_from(hash).unwrap() % Self::BITVECTOR_LENGTH;
                        if bitvector[bit_index / 128] & (1u128 << (bit_index % 128)) == 0 {
                            ones_count += 1;
                            bitvector[bit_index / 128] |= 1u128 << (bit_index % 128);
                        }
                    }
                    file_bitvectors[side].push(bitvector);
                    file_ones_counts[side].push(ones_count);
                }
            }
            bitvectors.push(file_bitvectors);
            ones_counts.push(file_ones_counts);
        }

        KGramSamplingScoring {
            symbols: internalize_parts(text_parts),
            bitvectors,
            ones_counts,
            part_scores: part_values(text_parts, &char_scorer),
            negative_score_multiplier: -0.5 * zero_at_similarity / (1.0 - zero_at_similarity),
        }
    }
}

impl MatchScoring for KGramSamplingScoring {
    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        let scores = [0, 1].map(|side| self.part_scores[file_ids[side]][side][part_indices[side]]);
        if self.is_match(part_indices, file_ids) {
            return scores[0];
        }

        let one_counts = [0, 1].map(|side| self.ones_counts[file_ids[side]][side][part_indices[side]]);
        let bitvectors = [0, 1].map(|side| &self.bitvectors[file_ids[side]][side][part_indices[side]]);
        let mut hits: usize = 0;
        for i in 0..Self::BITVECTOR_LENGTH / 128 {
            hits += (bitvectors[0][i] & bitvectors[1][i]).count_ones() as usize;
        }
        let corrected_hits = (hits * Self::BITVECTOR_LENGTH).saturating_sub(one_counts[0] * one_counts[1]) as f64
            / (Self::BITVECTOR_LENGTH - usize::max(one_counts[0], one_counts[1])) as f64;
        let corrected_hits = if corrected_hits <= Self::ROUND_DOWN_CORRECTED_HITS {
            0.0
        } else {
            corrected_hits
        };
        let iou = corrected_hits / usize::min(one_counts[0], one_counts[1]) as f64;
        let lower_exact_score = TScore::min(scores[0], scores[1]);
        let positive_score = TScore::min(
            iou / (1.0 + iou) * (scores[0] + scores[1]),
            Self::MAX_NONEXACT_SCORE_RATIO * lower_exact_score,
        );
        let negative_score = (scores[0] + scores[1]) - positive_score;

        let score = positive_score + negative_score * self.negative_score_multiplier;
        score
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
    }
}
