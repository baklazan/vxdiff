#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DiffOp {
    Match,
    Insert,
    Delete,
}

pub fn diff(old: &[&str], new: &[&str]) -> Vec<DiffOp> {
    let inf = old.len() + new.len() + 1;
    let mut dp: Vec<Vec<(usize, Option<DiffOp>)>> =
        vec![vec![(inf, None); new.len() + 1]; old.len() + 1];
    dp[old.len()][new.len()] = (0, None);
    for old_index in (0..old.len()).rev() {
        dp[old_index][new.len()] = (1 + dp[old_index + 1][new.len()].0, Some(DiffOp::Delete));
    }
    for new_index in (0..new.len()).rev() {
        dp[old.len()][new_index] = (1 + dp[old.len()][new_index + 1].0, Some(DiffOp::Insert));
    }
    for old_index in (0..old.len()).rev() {
        for new_index in (0..new.len()).rev() {
            let proposed_down = dp[old_index + 1][new_index].0 + 1;
            if dp[old_index][new_index].0 > proposed_down {
                dp[old_index][new_index] = (proposed_down, Some(DiffOp::Delete));
            }
            let proposed_right = dp[old_index][new_index + 1].0 + 1;
            if dp[old_index][new_index].0 > proposed_right {
                dp[old_index][new_index] = (proposed_right, Some(DiffOp::Insert));
            }
            let proposed_diagonal = dp[old_index + 1][new_index + 1].0;
            if old[old_index] == new[new_index] && dp[old_index][new_index].0 > proposed_diagonal {
                dp[old_index][new_index] = (proposed_diagonal, Some(DiffOp::Match));
            }
        }
    }
    let mut result = Vec::<DiffOp>::new();
    let mut old_index = 0;
    let mut new_index = 0;
    while old_index < old.len() || new_index < new.len() {
        result.push(dp[old_index][new_index].1.unwrap());
        match dp[old_index][new_index].1.unwrap() {
            DiffOp::Insert => {
                new_index += 1;
            }
            DiffOp::Delete => {
                old_index += 1;
            }
            DiffOp::Match => {
                new_index += 1;
                old_index += 1;
            }
        }
    }
    result
}
