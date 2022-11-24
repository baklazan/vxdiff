pub fn partition_into_words(text: &str) -> Vec<usize> {
    let mut word_bounds = vec![];
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
    word_bounds
}

pub fn partition_into_lines(text: &str) -> Vec<usize> {
    let mut line_bounds: Vec<usize> = vec![0];
    for (i, c) in text.char_indices() {
        if c == '\n' {
            line_bounds.push(i + 1);
        }
    }
    if *line_bounds.last().unwrap() < text.len() {
        line_bounds.push(text.len());
    }
    line_bounds
}
