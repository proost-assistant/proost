/// Variation of the std replace function that only replace full words
pub fn replace_word(text: &String, from: &str, to: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;
    let text_len = text.len();
    let from_len = from.len();
    for (start, part) in text.match_indices(from) {
        if (start == 0 || text.as_bytes()[start - 1] == b' ')
            && (start + from_len == text_len || text.as_bytes()[start + from_len] == b' ')
        {
            result.push_str(unsafe { text.get_unchecked(last_end..start) });
            result.push_str(to);
            last_end = start + part.len();
        }
    }
    result.push_str(unsafe { text.get_unchecked(last_end..text.len()) });
    result
}
