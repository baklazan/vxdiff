use super::*;

pub type TScore = f64;

#[derive(Debug)]
pub struct OriginalCandidate {
    pub starts: [usize; 2],
    pub ends: [usize; 2],
    pub alignment: Vec<DiffOp>,
    pub prefix_scores: Vec<TScore>,
    pub word_to_alignment: [Vec<usize>; 2], // words map to alignment steps
    pub alignment_to_word: [Vec<usize>; 2], // points between alignment steps map to points between words
}

impl OriginalCandidate {
    pub fn word_interval_to_alignment_interval(&self, interval: (usize, usize), side: usize) -> (usize, usize) {
        let interval = clamp_interval(interval, (self.starts[side], self.ends[side]));
        if interval.0 >= self.ends[side] {
            return (self.alignment.len(), self.alignment.len());
        }
        if interval.1 <= self.starts[side] {
            return (0, 0);
        }
        let first_word_index = interval.0 - self.starts[side];
        let last_word_index = interval.1 - 1 - self.starts[side];
        (self.word_to_alignment[side][first_word_index], self.word_to_alignment[side][last_word_index] + 1)
    }
}

pub struct Scoring {
    symbols: [Vec<string_interner::symbol::SymbolU32>; 2],
    scores: [Vec<TScore>; 2]
}

impl Scoring {
    const P_MATCH: TScore = 0.9;
    const CHANGE_STATE_SCORE: TScore = -15.0;
    const SCORE_THRESHOLD: TScore = 15.0;
    
    fn match_score(&self, old_index : usize, new_index : usize) -> TScore {
        if self.symbols[0][old_index] != self.symbols[1][new_index] {
            return TScore::NEG_INFINITY;
        }
        self.scores[0][old_index]
    }
    
    fn gap_score() -> TScore {
        (1.0 - Scoring::P_MATCH).log2()
    }
    
    pub fn new(texts: &[PartitionedText; 2]) -> Scoring {
        use std::collections::HashMap;
        
        let mut char_frequencies : HashMap<char, usize> = HashMap::new();
        let mut total_chars : usize = 0;
        
        for side in 0..2 {
            for c in texts[side].text.chars() {
                char_frequencies.insert(c, char_frequencies.get(&c).unwrap_or(&0) + 1);
                total_chars += 1;
            }
        }
        
        let mut symbols = [vec![], vec![]];
        let mut interner = StringInterner::default();
        let mut scores = [vec![], vec![]];
        for side in 0..2 {
            for i in 0..texts[side].word_count() {
                let word = texts[side].get_word(i);
                symbols[side].push(interner.get_or_intern(word));
                let mut frequency : f64 = 1.0;
                for c in word.chars() {
                    frequency *= (*char_frequencies.get(&c).unwrap() as f64) / total_chars as f64;
                }
                scores[side].push(Scoring::P_MATCH.log2() - frequency.log2());
            }
        }
        
        Scoring { symbols, scores }
    }
}

pub fn supersection_candidates(
    old_len: usize,
    new_len: usize,
    scoring: &Scoring,
    old_can_change_state: &Vec<bool>,
    new_can_change_state: &Vec<bool>,
) -> Vec<OriginalCandidate> {
    type Movement = Option<DiffOp>;
    type Position = (usize, usize);

    let mut match_dp: Vec<Vec<(TScore, Movement, Position)>> =
        vec![vec![(f64::NEG_INFINITY, None, (old_len, new_len)); new_len + 1]; old_len + 1];
    let mut mismatch_dp: Vec<Vec<TScore>> = vec![vec![f64::NEG_INFINITY; new_len + 1]; old_len + 1];

    fn improve(left: &mut (TScore, Movement, Position), right: (TScore, Movement, Position)) {
        *left = if left.0 > right.0 { *left } else { right }
    }

    let mut best_leaf_for_root: std::collections::HashMap<Position, (TScore, Position)> =
        std::collections::HashMap::new();

    match_dp[old_len][new_len].0 = 0.0;
    for old_index in (0..=old_len).rev() {
        for new_index in (0..=new_len).rev() {
            let position = (old_index, new_index);
            if old_index < old_len {
                let next = match_dp[old_index + 1][new_index];
                improve(
                    &mut match_dp[old_index][new_index],
                    (next.0 + Scoring::gap_score(), Some(DiffOp::Delete), next.2),
                );
                mismatch_dp[old_index][new_index] =
                    f64::max(mismatch_dp[old_index][new_index], mismatch_dp[old_index + 1][new_index]);
            }
            if new_index < new_len {
                let next = match_dp[old_index][new_index + 1];
                improve(
                    &mut match_dp[old_index][new_index],
                    (next.0 + Scoring::gap_score(), Some(DiffOp::Insert), next.2),
                );
                mismatch_dp[old_index][new_index] =
                    f64::max(mismatch_dp[old_index][new_index], mismatch_dp[old_index][new_index + 1]);
            }
            if old_index < old_len && new_index < new_len {
                let next = match_dp[old_index + 1][new_index + 1];
                improve(
                    &mut match_dp[old_index][new_index],
                    (next.0 + scoring.match_score(old_index, new_index), Some(DiffOp::Match), next.2),
                );
            }
            if old_can_change_state[old_index] && new_can_change_state[new_index] {
                improve(
                    &mut match_dp[old_index][new_index],
                    (mismatch_dp[old_index][new_index] + Scoring::CHANGE_STATE_SCORE, None, position),
                );
                let root = match_dp[old_index][new_index].2;
                if root == position {
                    best_leaf_for_root.insert(root, (match_dp[old_index][new_index].0, position));
                }
                let old_value = best_leaf_for_root.get(&root).unwrap().0;
                if match_dp[old_index][new_index].0 > old_value {
                    best_leaf_for_root.insert(root, (match_dp[old_index][new_index].0, position));
                }

                mismatch_dp[old_index][new_index] = f64::max(
                    mismatch_dp[old_index][new_index],
                    match_dp[old_index][new_index].0 + Scoring::CHANGE_STATE_SCORE,
                );
            }
        }
    }

    let mut result = vec![];
    for (root, (score, leaf)) in best_leaf_for_root.iter() {
        if *score - match_dp[root.0][root.1].0 < Scoring::SCORE_THRESHOLD {
            continue;
        }
        let mut alignment = Vec::<DiffOp>::new();
        let mut prefix_scores = vec![0.0];
        let mut word_to_alignment = [vec![], vec![]];
        let (mut old_index, mut new_index) = leaf;
        let mut alignment_to_word = [vec![old_index], vec![new_index]];
        while old_index < root.0 || new_index < root.1 {
            let op = match_dp[old_index][new_index].1.unwrap();
            let score;
            match op {
                DiffOp::Insert => {
                    score = Scoring::gap_score();
                    word_to_alignment[1].push(alignment.len());
                    new_index += 1;
                }
                DiffOp::Delete => {
                    score = Scoring::gap_score();
                    word_to_alignment[0].push(alignment.len());
                    old_index += 1;
                }
                DiffOp::Match => {
                    score = scoring.match_score(old_index, new_index);
                    word_to_alignment[0].push(alignment.len());
                    word_to_alignment[1].push(alignment.len());
                    new_index += 1;
                    old_index += 1;
                }
            }
            prefix_scores.push(prefix_scores.last().unwrap() + score);
            alignment.push(op);
            alignment_to_word[0].push(old_index);
            alignment_to_word[1].push(new_index);
        }
        result.push(OriginalCandidate {
            starts: [leaf.0, leaf.1],
            ends: [root.0, root.1],
            alignment,
            prefix_scores,
            word_to_alignment,
            alignment_to_word,
        });
    }
    result
}
