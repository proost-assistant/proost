/// Variation of the std replace function that only replace full words
pub fn replace_inplace(input: &mut String, from: &str, to: &str) {
    let mut offset = 0;

    while let Some(pos) = input[offset..].find(from) {
        input.replace_range(offset + pos..from.len(), to);

        offset += pos + (to.len() - from.len());
    }
}
