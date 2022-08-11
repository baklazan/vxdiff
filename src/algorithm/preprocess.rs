use super::PartitionedText;

pub fn partition_into_words<'a>(text: &'a str) -> PartitionedText<'a> {
    let mut word_bounds = Vec::new();
    let mut was_last_alphabetic = false;
    let mut was_last_numeric = false;
    for (i, c) in text.char_indices() {
        if c.is_alphabetic() && was_last_alphabetic {
            continue;
        }
        if c.is_numeric() && was_last_numeric {
            continue;
        }
        was_last_alphabetic = c.is_alphabetic();
        was_last_numeric = c.is_numeric();
        word_bounds.push(i);
    }
    word_bounds.push(text.len());
    PartitionedText { text, word_bounds }
}