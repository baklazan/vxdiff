use std::ops::Range;

pub fn gui_layout<const N: usize>(constraints: [Option<usize>; N], range: Range<usize>) -> [Range<usize>; N] {
    let mut none_count = 0;
    let mut some_sum = 0;
    for &constraint in &constraints {
        match constraint {
            Some(n) => some_sum += n,
            None => none_count += 1,
        }
    }

    let freedom = range.end.saturating_sub(range.start).saturating_sub(some_sum);
    let each_none = freedom / none_count.max(1);

    let mut pos = range.start;
    constraints.map(|constraint| {
        let width = constraint.unwrap_or(each_none);
        pos += width;
        (pos - width)..pos
    })
}
