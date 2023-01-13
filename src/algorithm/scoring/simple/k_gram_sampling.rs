use std::collections::HashMap;

use rand::Rng;
use rand::SeedableRng;

use crate::algorithm::{
    preprocess::{information_values, internalize_parts},
    scoring::TScore,
    PartitionedText,
};

use super::MatchScoring;

pub struct KGramSamplingScoring {
    symbols: Vec<[Vec<string_interner::symbol::SymbolU32>; 2]>,
    bitvectors: Vec<[Vec<[u128; Self::BITVECTOR_LENGTH / 128]>; 2]>,
    ones_counts: Vec<[Vec<usize>; 2]>,
    part_scores: Vec<[Vec<TScore>; 2]>,
}

impl KGramSamplingScoring {
    const K: usize = 3;
    const BITVECTOR_LENGTH: usize = 128;
    const SAMPLES: usize = 25;

    const HASH_MOD_P: u64 = 1000000009;

    pub(in crate::algorithm) fn new(text_parts: &[[PartitionedText; 2]]) -> Self {
        const BASE: u64 = 257;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);
        let hash_factor: u64 = rng.gen_range(1..Self::HASH_MOD_P);
        let hash_additive: u64 = rng.gen_range(0..Self::HASH_MOD_P);
        let mut base_to_k = 1;
        for _ in 0..Self::K {
            base_to_k *= BASE;
            base_to_k %= Self::HASH_MOD_P;
        }

        let mut bitvectors = vec![];
        let mut ones_counts = vec![];

        for file_texts_parts in text_parts.iter() {
            let mut file_bitvectors = [vec![], vec![]];
            let mut file_ones_counts = [vec![], vec![]];
            for (side, side_parts) in file_texts_parts.iter().enumerate() {
                for i in 0..side_parts.part_count() {
                    let mut value_seen_counts: HashMap<u64, usize> = HashMap::new();

                    let mut prefix_values = vec![0];
                    let mut hashes = vec![];
                    let mut current_prefix_value = 0;
                    for (j, &b) in side_parts.get_part(i).as_bytes().iter().enumerate() {
                        current_prefix_value *= BASE;
                        current_prefix_value += u64::from(b);
                        current_prefix_value %= Self::HASH_MOD_P;
                        prefix_values.push(current_prefix_value);

                        if j + 1 >= Self::K {
                            let value = (current_prefix_value + Self::HASH_MOD_P
                                - (prefix_values[j + 1 - Self::K] * base_to_k) % Self::HASH_MOD_P)
                                % Self::HASH_MOD_P;

                            let seen_count = *value_seen_counts.get(&value).unwrap_or(&0);
                            let value_with_seen =
                                (value * BASE + u64::try_from(seen_count).unwrap()) % Self::HASH_MOD_P;
                            value_seen_counts.insert(value, seen_count + 1);

                            hashes.push(((value_with_seen * hash_factor) + hash_additive) % Self::HASH_MOD_P);
                        }
                    }
                    let mut samples = &hashes[..];
                    if hashes.len() > Self::SAMPLES {
                        samples = hashes.select_nth_unstable(Self::SAMPLES).0;
                    }

                    let mut bitvector = [0; Self::BITVECTOR_LENGTH / 128];
                    let mut ones_count = 0;
                    for &hash in samples {
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
            part_scores: information_values(text_parts),
        }
    }
}

impl MatchScoring for KGramSamplingScoring {
    fn score(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> TScore {
        let one_counts = [0, 1].map(|side| self.ones_counts[file_ids[side]][side][part_indices[side]]);
        if one_counts[0] == 0 || one_counts[1] == 0 {
            return 0.0;
        }

        let bitvectors = [0, 1].map(|side| &self.bitvectors[file_ids[side]][side][part_indices[side]]);
        let mut hits: usize = 0;
        for i in 0..Self::BITVECTOR_LENGTH / 128 {
            hits += (bitvectors[0][i] & bitvectors[1][i]).count_ones() as usize;
        }

        let corrected_hits = (hits * Self::BITVECTOR_LENGTH).saturating_sub(one_counts[0] * one_counts[1]) as f64
            / (Self::BITVECTOR_LENGTH - usize::max(one_counts[0], one_counts[1])) as f64;

        let scores = [0, 1].map(|side| &self.part_scores[file_ids[side]][side][part_indices[side]]);
        corrected_hits / (one_counts[0] + one_counts[1]) as f64 * (scores[0] + scores[1])
    }

    fn is_match(&self, part_indices: [usize; 2], file_ids: [usize; 2]) -> bool {
        self.symbols[file_ids[0]][0][part_indices[0]] == self.symbols[file_ids[1]][1][part_indices[1]]
    }
}
