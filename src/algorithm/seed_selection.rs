use super::PartitionedText;
use rand::Rng;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Seed {
    pub start: [usize; 2],
    pub end: [usize; 2],
    pub file_ids: [usize; 2],
}

struct HashFunctionParams {
    pub mod_p: u64,
    pub factor: u64,
}

fn k_word_hashes(text: &PartitionedText, k: usize, f: &HashFunctionParams) -> Vec<u64> {
    const BASE: u64 = 257;

    let mut base_powers = vec![1];
    let mut base_power = |power: usize| -> u64 {
        while base_powers.len() <= power {
            base_powers.push((base_powers.last().unwrap() * BASE) % f.mod_p);
        }
        base_powers[power]
    };

    let mut prefix_values = vec![0];
    let mut hashes = vec![];
    let mut current_prefix_value = 0;
    for i in 0..text.word_count() {
        for &b in text.get_word(i).as_bytes() {
            current_prefix_value *= BASE;
            current_prefix_value += u64::from(b);
            current_prefix_value %= f.mod_p;
        }
        prefix_values.push(current_prefix_value);
        if i + 1 >= k {
            let k_word_length = text.word_bounds[i + 1] - text.word_bounds[i + 1 - k];
            let value = (current_prefix_value + f.mod_p
                - (prefix_values[i + 1 - k] * base_power(k_word_length)) % f.mod_p)
                % f.mod_p;
            hashes.push((value * f.factor) % f.mod_p);
        }
    }
    hashes
}

pub fn select_seeds(texts: &[[PartitionedText; 2]]) -> Vec<Seed> {
    const K: usize = 10;
    const MOD_P: u64 = 1000000009;
    let mut rng = rand::thread_rng();
    let factor = rng.gen_range(1..MOD_P);
    let hash_function = HashFunctionParams { mod_p: MOD_P, factor };

    let mut hashes = vec![];
    let mut hashtable_size = 0;
    for file_texts in texts {
        hashes.push([
            k_word_hashes(&file_texts[0], K, &hash_function),
            k_word_hashes(&file_texts[1], K, &hash_function),
        ]);
        hashtable_size += file_texts[0].word_count();
    }

    let mut old_starts_by_hash = vec![vec![]; hashtable_size];
    for file_id in 0..texts.len() {
        for (i, hash) in hashes[file_id][0].iter().enumerate() {
            old_starts_by_hash[usize::try_from(*hash).unwrap() % hashtable_size].push((i, file_id, *hash));
        }
    }

    let mut result = vec![];

    for new_file_id in 0..texts.len() {
        let mut open_seed_starts: Vec<(usize, i64, [usize; 2])> = vec![];
        for (new_start, &hash) in hashes[new_file_id][1].iter().enumerate() {
            let mut new_open_seed_starts = vec![];
            let mut open_seed_index = 0;
            for &(old_start, old_file_id, old_hash) in
                old_starts_by_hash[usize::try_from(hash).unwrap() % hashtable_size].iter()
            {
                if old_hash != hash {
                    continue;
                }
                let diagonal = i64::try_from(old_start).unwrap() - i64::try_from(new_start).unwrap();
                while open_seed_index < open_seed_starts.len() {
                    let (open_file_id, open_diagonal, seed_start) = open_seed_starts[open_seed_index];
                    if open_file_id > old_file_id || open_file_id == old_file_id && open_diagonal >= diagonal {
                        break;
                    }
                    let seed_length = new_start - 1 - seed_start[1] + K;
                    result.push(Seed {
                        start: seed_start,
                        end: [seed_start[0] + seed_length, seed_start[1] + seed_length],
                        file_ids: [open_file_id, new_file_id],
                    });
                    open_seed_index += 1;
                }
                if open_seed_index < open_seed_starts.len() {
                    let (open_file_id, open_diagonal, _) = open_seed_starts[open_seed_index];
                    if open_file_id == old_file_id && open_diagonal == diagonal {
                        new_open_seed_starts.push(open_seed_starts[open_seed_index]);
                        open_seed_index += 1;
                    }
                } else {
                    new_open_seed_starts.push((old_file_id, diagonal, [old_start, new_start]));
                }
            }
            while open_seed_index < open_seed_starts.len() {
                let (old_file_id, _, seed_start) = open_seed_starts[open_seed_index];
                let seed_length = new_start - 1 - seed_start[1] + K;
                result.push(Seed {
                    start: seed_start,
                    end: [seed_start[0] + seed_length, seed_start[1] + seed_length],
                    file_ids: [old_file_id, new_file_id],
                });
                open_seed_index += 1;
            }
            open_seed_starts = new_open_seed_starts;
        }
        for open_seed in open_seed_starts {
            let (old_file_id, _, seed_start) = open_seed;
            let seed_length = texts[new_file_id][1].word_count() - seed_start[1];
            result.push(Seed {
                start: seed_start,
                end: [seed_start[0] + seed_length, seed_start[1] + seed_length],
                file_ids: [old_file_id, new_file_id],
            });
        }
    }

    result
}
