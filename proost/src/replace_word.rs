/// Variation of the std replace function that only replace full words
pub fn replace_word(input: &String, from: &str, to: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;
    let input_len = input.len();
    let from_len = from.len();
    for (start, _) in input.match_indices(from) {
        if (start == 0 || input.as_bytes()[start - 1] == b' ')
            && (start + from_len == input_len || input.as_bytes()[start + from_len] == b' ')
        {
            result.push_str(unsafe { input.get_unchecked(last_end..start) });
            result.push_str(to);
            last_end = start + from_len;
        }
    }
    result.push_str(unsafe { input.get_unchecked(last_end..input_len) });
    result
}
