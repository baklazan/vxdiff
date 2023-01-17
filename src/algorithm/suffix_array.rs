// string must only contain values from 1..=string.len()
// (to call this function on other types of strings,
// first encode them in the 1..=string.len() alphabet)
pub(super) fn suffix_array(string: &[usize]) -> Vec<usize> {
    for &symbol in string {
        if symbol == 0 {
            panic!("string contains a zero");
        }
        if symbol > string.len() {
            panic!("string contains a number greater than its length");
        }
    }

    if string.is_empty() {
        panic!("suffix array called on an empty string!");
    }
    if string.len() == 1 {
        return vec![0];
    }
    if string.len() == 2 {
        if string[0] < string[1] {
            return vec![0, 1];
        } else {
            return vec![1, 0];
        }
    }

    let indices_in_two_thirds: Vec<usize> = (0..=string.len()).filter(|index| index % 3 != 0).collect();
    let read_digit_two_thirds =
        |index: usize, position: usize| -> usize { *string.get(index + position).unwrap_or(&0) };
    let two_thirds_order = radix_sort(&indices_in_two_thirds, 3, string.len(), &read_digit_two_thirds);

    let main_to_two_thirds_index = |main_index: usize| -> usize {
        if main_index % 3 == 1 {
            main_index / 3
        } else {
            (string.len() + 2) / 3 + main_index / 3
        }
    };
    let two_thirds_to_main_index = |two_thirds_index: &usize| -> usize {
        if *two_thirds_index < (string.len() + 2) / 3 {
            two_thirds_index * 3 + 1
        } else {
            (two_thirds_index - (string.len() + 2) / 3) * 3 + 2
        }
    };

    let mut two_thirds_string = vec![0; indices_in_two_thirds.len()];
    let mut next_value = 1;
    for (i, &main_index) in two_thirds_order.iter().enumerate() {
        if i > 0 {
            let previous_main_index = two_thirds_order[i - 1];
            for j in 0..3 {
                if read_digit_two_thirds(main_index, j) > read_digit_two_thirds(previous_main_index, j) {
                    next_value += 1;
                    break;
                }
            }
        }
        two_thirds_string[main_to_two_thirds_index(main_index)] = next_value;
    }

    let two_thirds_suffix_array: Vec<usize> = suffix_array(&two_thirds_string)
        .iter()
        .map(two_thirds_to_main_index)
        .collect();
    let two_thirds_suffix_array = if two_thirds_suffix_array[0] == string.len() {
        &two_thirds_suffix_array[1..]
    } else {
        &two_thirds_suffix_array
    };

    const INVALID: usize = usize::MAX;
    let mut rank = vec![INVALID; string.len()];
    for (i, &main_index) in two_thirds_suffix_array.iter().enumerate() {
        rank[main_index] = i + 1;
    }

    let indices_in_one_third: Vec<usize> = (0..string.len()).filter(|index| index % 3 == 0).collect();

    let read_digit_one_third = |index: usize, position: usize| -> usize {
        if position == 0 {
            string[index]
        } else {
            *rank.get(index + 1).unwrap_or(&0)
        }
    };
    let one_third_suffix_array = radix_sort(&indices_in_one_third, 2, string.len(), &read_digit_one_third);

    let mut result: Vec<usize> = vec![];
    let mut one_third_iter = one_third_suffix_array.iter().peekable();
    let mut two_thirds_iter = two_thirds_suffix_array.iter().peekable();
    'merge: while one_third_iter.peek().is_some() || two_thirds_iter.peek().is_some() {
        if one_third_iter.peek().is_none() {
            result.push(*two_thirds_iter.next().unwrap());
            continue;
        }
        if two_thirds_iter.peek().is_none() {
            result.push(*one_third_iter.next().unwrap());
            continue;
        }
        let one_third_index = **one_third_iter.peek().unwrap();
        let two_thirds_index = **two_thirds_iter.peek().unwrap();
        let raw_comparisons = two_thirds_index % 3;
        for i in 0..raw_comparisons {
            let one_third_symbol = string.get(one_third_index + i).unwrap_or(&0);
            let two_thirds_symbol = string.get(two_thirds_index + i).unwrap_or(&0);
            if one_third_symbol < two_thirds_symbol {
                result.push(*one_third_iter.next().unwrap());
                continue 'merge;
            }
            if two_thirds_symbol < one_third_symbol {
                result.push(*two_thirds_iter.next().unwrap());
                continue 'merge;
            }
        }
        let one_third_rest = rank.get(one_third_index + raw_comparisons).unwrap_or(&0);
        let two_thirds_rest = rank.get(two_thirds_index + raw_comparisons).unwrap_or(&0);
        if one_third_rest < two_thirds_rest {
            result.push(*one_third_iter.next().unwrap());
        } else {
            result.push(*two_thirds_iter.next().unwrap());
        }
    }

    result
}

pub(super) fn longest_common_prefix_array(suffix_array: &[usize], string: &[usize]) -> Vec<usize> {
    let mut rank = vec![0; string.len()];
    for (i, &string_index) in suffix_array.iter().enumerate() {
        rank[string_index] = i;
    }
    let mut result = vec![0; suffix_array.len() - 1];
    let mut matching = 0;
    for start_idx in 0..string.len() {
        if rank[start_idx] + 1 >= suffix_array.len() {
            matching = 0;
            continue;
        }
        let next_start_idx = suffix_array[rank[start_idx] + 1];
        while string.get(start_idx + matching).unwrap_or(&0) == string.get(next_start_idx + matching).unwrap_or(&0) {
            matching += 1;
        }
        result[rank[start_idx]] = matching;
        matching = matching.saturating_sub(1);
    }
    result
}

fn radix_sort(
    array: &[usize],
    width: usize,
    max_digit: usize,
    read_digit: &dyn Fn(usize, usize) -> usize,
) -> Vec<usize> {
    let mut round_input = array;
    let mut last_round_output = vec![];
    for round in (0..width).rev() {
        let mut in_bucket = vec![0; max_digit + 1];
        for &value in round_input {
            in_bucket[read_digit(value, round)] += 1;
        }
        let mut bucket_start = vec![0; max_digit + 1];
        for i in 1..=max_digit {
            bucket_start[i] = bucket_start[i - 1] + in_bucket[i - 1];
        }

        let mut round_output = vec![0; array.len()];
        for &value in round_input {
            let digit = read_digit(value, round);
            round_output[bucket_start[digit]] = value;
            bucket_start[digit] += 1;
        }
        last_round_output = round_output;
        round_input = &last_round_output;
    }
    last_round_output
}

#[cfg(test)]
mod test {
    use super::{longest_common_prefix_array, suffix_array};

    fn assert_str_error(result: &std::thread::Result<()>, expected_error: &str) {
        assert!(result.is_err());
        if let Err(error) = result {
            assert_eq!(error.downcast_ref::<&str>().unwrap(), &expected_error);
        } else {
            panic!();
        }
    }

    #[test]
    fn one_symbol() {
        assert_eq!(suffix_array(&[1]), vec![0]);
    }

    #[test]
    fn zero_in_input() {
        let result = std::panic::catch_unwind(|| {
            suffix_array(&[0, 1, 2]);
        });
        assert_str_error(&result, "string contains a zero");
    }

    #[test]
    fn high_number_in_input() {
        let result = std::panic::catch_unwind(|| {
            suffix_array(&[1, 2, 3, 5]);
        });
        assert_str_error(&result, "string contains a number greater than its length");
    }

    #[test]
    fn suffix_array_banana() {
        assert_eq!(suffix_array(&[2, 1, 3, 1, 3, 1]), vec![5, 3, 1, 0, 4, 2]);
    }

    #[test]
    fn lcp_banana() {
        let string = [2, 1, 3, 1, 3, 1];
        let suffix_array = [5, 3, 1, 0, 4, 2];
        assert_eq!(longest_common_prefix_array(&suffix_array, &string), vec![1, 3, 0, 0, 2]);
    }
}
