use alloc::borrow::Cow::{self, Borrowed, Owned};

use colored::Colorize;
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::HistoryHinter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Cmd, ConditionalEventHandler, Context, Event, EventContext, RepeatCount, Result};
use rustyline_derive::{Helper, Hinter};

/// Language keywords that should be highlighted
const KEYWORDS: [&str; 5] = ["check", "def", "eval", "import", "search"];

/// An Helper for a RustyLine Editor that implements:
/// - a standard hinter;
/// - custom validator, completer and highlighter.
#[derive(Helper, Hinter)]
pub struct RustyLineHelper {
    color: bool,
    completer: FilenameCompleter,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl RustyLineHelper {
    pub fn new(color: bool) -> Self {
        Self {
            color,
            completer: FilenameCompleter::new(),
            hinter: HistoryHinter {},
        }
    }
}

/// An Handler for the tab event
pub struct TabEventHandler;
impl ConditionalEventHandler for TabEventHandler {
    fn handle(&self, _: &Event, n: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
        if ctx.line().starts_with("import") {
            return None;
        }
        Some(Cmd::Insert(n, "  ".to_string()))
    }
}

/// A variation of [FilenameCompleter](https://docs.rs/rustyline/latest/rustyline/completion/struct.FilenameCompleter.html):
/// file completion is available only after having typed import
impl Completer for RustyLineHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Result<(usize, Vec<Pair>)> {
        if line.starts_with("import") { self.completer.complete_path(line, pos) } else { Ok(Default::default()) }
    }
}

/// A variation of [MatchingBracketValidator](https://docs.rs/rustyline/latest/rustyline/validate/struct.MatchingBracketValidator.html).
///
/// No validation occurs when entering the import command
impl Validator for RustyLineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
        if ctx.input().starts_with("import") {
            return Ok(ValidationResult::Valid(None));
        }

        Ok(validate_arrows(ctx.input()).or_else(|| validate_brackets(ctx.input())).unwrap_or(ValidationResult::Valid(None)))
    }
}

fn validate_arrows(input: &str) -> Option<ValidationResult> {
    let mut iter = input.as_bytes().iter().rev();

    if let Some(b) = iter.find(|b| !(**b).is_ascii_whitespace()) {
        if *b == b'>' && let Some(b) = iter.next() && (*b == b'-' || *b == b'=') {
            return Some(ValidationResult::Incomplete)
        }
    }

    None
}

fn validate_brackets(input: &str) -> Option<ValidationResult> {
    let mut stack = vec![];

    for c in input.chars() {
        match c {
            '(' => stack.push(c),
            ')' => match stack.pop() {
                Some('(') => {},
                Some(_) => {
                    return Some(ValidationResult::Invalid(Some("\nMismatched brackets: ) is not properly closed".to_string())));
                },
                None => return Some(ValidationResult::Invalid(Some("\nMismatched brackets: ( is unpaired".to_string()))),
            },
            _ => {},
        }
    }

    if stack.is_empty() { None } else { Some(ValidationResult::Incomplete) }
}

/// A variation of [MatchingBracketHighlighter](https://docs.rs/rustyline/latest/rustyline/highlight/struct.MatchingBracketHighlighter.html).
///
/// No check occurs before cursor
impl Highlighter for RustyLineHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        if !self.color {
            return Owned(hint.to_owned());
        }
        Owned(format!("{}", hint.bright_black()))
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        self.color
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        if line.len() <= 1 || !self.color {
            return Borrowed(line);
        }
        let mut copy = line.to_owned();

        if let Some((bracket, pos)) = get_bracket(line, pos)
            && let Some((matching, pos)) = find_matching_bracket(line, pos, bracket) {
                let s = String::from(matching as char);
                copy.replace_range(pos..=pos, &format!("{}", s.blue().bold()));
            }
        KEYWORDS.iter().for_each(|keyword| replace_inplace(&mut copy, keyword, &format!("{}", keyword.blue().bold())));
        Owned(copy)
    }
}

/// Variation of the std replace function that only replace full words
pub fn replace_inplace(input: &mut String, from: &str, to: &str) {
    let mut offset = 0;
    while let Some(pos) = input[offset..].find(from) {
        if (pos == 0 || input.as_bytes()[offset + pos - 1].is_ascii_whitespace())
            && (offset + pos + from.len() == input.len() || input.as_bytes()[offset + pos + from.len()].is_ascii_whitespace())
        {
            input.replace_range((offset + pos)..(offset + pos + from.len()), to);
            offset += pos + to.len();
        } else {
            offset += pos + from.len();
        }
    }
}

fn find_matching_bracket(line: &str, pos: usize, bracket: u8) -> Option<(u8, usize)> {
    let matching_bracket = matching_bracket(bracket);
    let mut to_match = 1;

    let match_bracket = |b: u8| {
        if b == matching_bracket {
            to_match -= 1;
        } else if b == bracket {
            to_match += 1;
        };
        to_match == 0
    };

    if is_open_bracket(bracket) {
        // forward search
        line[pos + 1..].bytes().position(match_bracket).map(|pos2| (matching_bracket, pos2 + pos + 1))
    } else {
        // backward search
        line[..pos].bytes().rev().position(match_bracket).map(|pos2| (matching_bracket, pos - pos2 - 1))
    }
}

/// Check if the cursor is on a bracket
const fn get_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if !line.is_empty() && pos < line.len() {
        let b = line.as_bytes()[pos];
        if is_bracket(b) {
            return Some((b, pos));
        }
    }
    None
}

const fn matching_bracket(bracket: u8) -> u8 {
    match bracket {
        b'(' => b')',
        b')' => b'(',
        _ => unreachable!(),
    }
}

const fn is_bracket(bracket: u8) -> bool {
    matches!(bracket, b'(' | b')')
}

const fn is_open_bracket(bracket: u8) -> bool {
    matches!(bracket, b'(')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_bracket_empty() {
        assert!(get_bracket("", 0).is_none());
    }

    #[test]
    fn get_bracket_out_of_bound() {
        assert!(get_bracket("(())", 4).is_none());
    }

    #[test]
    fn get_bracket_none() {
        assert!(get_bracket("a()[", 0).is_none());
        assert!(get_bracket("a()[", 3).is_none());
    }

    #[test]
    fn get_bracket_some() {
        assert!(get_bracket("a()[", 1).is_some());
        assert!(get_bracket("a()[", 2).is_some());
    }

    #[test]
    fn find_matching_bracket_direction() {
        assert!(find_matching_bracket("  )", 1, b')').is_none());
        assert!(find_matching_bracket("  )", 1, b'(').is_some());
        assert!(find_matching_bracket("(  ", 1, b')').is_some());
        assert!(find_matching_bracket("(  ", 1, b'(').is_none());
    }

    #[test]
    fn find_matching_bracket_matching() {
        assert_eq!(find_matching_bracket("  )", 1, b'('), Some((b')', 2)));
        assert_eq!(find_matching_bracket("(  ", 1, b')'), Some((b'(', 0)));
    }

    #[test]
    fn find_matching_bracket_counter() {
        assert_eq!(find_matching_bracket(" (())))", 0, b'('), Some((b')', 5)));
        assert_eq!(find_matching_bracket("(((()) ", 6, b')'), Some((b'(', 1)));
    }

    #[test]
    fn replace_inplace() {
        let mut message = "mot motus et mots mot mot".to_string();
        super::replace_inplace(&mut message, "mot", "mots");
        assert_eq!(message, "mots motus et mots mots mots".to_string());
    }
}
