//! A collection of function for interactive assistance during a toplevel session

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

/// An Helper for a `RustyLine` Editor that implements:
/// - a standard hinter;
/// - custom validator, completer and highlighter.
#[derive(Helper, Hinter)]
pub struct RustyLineHelper {
    /// Whether colour is displayed
    color: bool,

    /// The completer object
    completer: FilenameCompleter,

    /// The hinter object
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl RustyLineHelper {
    /// Creates a new helper
    pub fn new(color: bool) -> Self {
        Self {
            color,
            completer: FilenameCompleter::new(),
            hinter: HistoryHinter {},
        }
    }
}

/// A cursor that explores a string char by char
/// and traces the last column position
struct Cursor {
    /// Byte position in the string
    global_pos: usize,
    /// Char position in the current line
    current_pos: u16,
    /// Indicates whether the line only
    /// contains whitespaces so far
    line_empty: bool,
    /// Position of the last column found
    last_token: Option<u16>,
    /// Indicates whether the last token is complete
    /// (token = alphanumeric sequence)
    token_complete: bool,
}

impl Cursor {
    /// Create a new Cursor
    const fn new() -> Self {
        // current_pos = 2 due to offset in the first line
        Self {
            global_pos: 0,
            line_empty: true,
            current_pos: 2,
            last_token: None,
            token_complete: true,
        }
    }

    /// Returns the next non-whitespace character, consuming all the whitespace characters between
    /// it and the current position. Returns None if the end of the string is reached without finding
    /// a non-whitespace char.
    fn consume_spaces(&mut self, chars: &mut std::str::Chars, until: usize) -> Option<char> {
        loop {
            if self.global_pos >= until {
                return None;
            }
            match chars.next() {
                Some(c) if c.is_whitespace() => {
                    self.token_complete = true;
                    self.current_pos += 1;
                    if c == '\n' || c == '\r' {
                        self.current_pos = 0;
                        self.line_empty = true;
                    }
                    self.global_pos += c.len_utf8();
                },
                c => {
                    self.current_pos += 1;
                    self.line_empty = self.line_empty && c.is_none();
                    self.global_pos += c.map_or(0, char::len_utf8);
                    return c;
                },
            }
        }
    }

    /// Updates the cursor by continuing reading the string from the current position.
    /// c is None when the cursor is either at the beginning or at the end of the cursor.
    fn process_from(&mut self, mut c: Option<char>, rest: &mut std::str::Chars, until: usize) {
        if c.is_none() {
            c = self.consume_spaces(rest, until); // c may still be None
        }
        match c {
            Some(':') if self.last_token.is_some() => {
                self.token_complete = false;

                let c = self.consume_spaces(rest, until);
                if c.is_none() {
                    self.last_token = None;
                    return;
                }
                // Considering "name : Type" as a single token.
                self.token_complete = false;
                self.process_from(c, rest, until);
            },
            Some('=') => {
                // Resetting column when reading "=>" or ":="
                self.last_token = None;
                self.token_complete = true;
            },
            Some('(') => {
                self.last_token = Some(self.current_pos - 1);
                self.token_complete = true;
            },
            Some(')') => {
                // Resetting column when readin ")"
                // This could be improved
                self.last_token = None;
                self.token_complete = true;
            },
            Some(c) if self.token_complete && c.is_alphanumeric() => {
                // Starting a new token
                self.last_token = Some(self.current_pos - 1);
                self.token_complete = false;
            },
            None => return,
            _ => (),
        }
        let c = self.consume_spaces(rest, until);
        self.process_from(c, rest, until);
    }
}

/// A Handler for the tab event
pub struct TabEventHandler;
impl TabEventHandler {
    /// Get the column at which the tab key should move.
    fn column(mut chars: std::str::Chars, until: usize) -> Cursor {
        let mut cursor = Cursor::new();
        cursor.process_from(None, &mut chars, until);
        if !cursor.line_empty {
            cursor.last_token = None;
        }
        cursor
    }
}
impl ConditionalEventHandler for TabEventHandler {
    fn handle(&self, _: &Event, n: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
        if ctx.line().starts_with("import") {
            return None;
        }

        let cur = Self::column(ctx.line().chars(), ctx.pos());
        match cur.last_token {
            Some(pos) if cur.current_pos < pos => Some(Cmd::Insert(n, " ".repeat((pos - cur.current_pos).into()))),
            _ => Some(Cmd::Insert(n, "  ".to_owned())),
        }
    }
}

/// A variation of [`FilenameCompleter`](https://docs.rs/rustyline/latest/rustyline/completion/struct.FilenameCompleter.html):
/// file completion is available only after having typed import
impl Completer for RustyLineHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Result<(usize, Vec<Pair>)> {
        if line.starts_with("import") { self.completer.complete_path(line, pos) } else { Ok(Default::default()) }
    }
}

/// A variation of [`MatchingBracketValidator`](https://docs.rs/rustyline/latest/rustyline/validate/struct.MatchingBracketValidator.html).
///
/// No validation occurs when entering the import command
impl Validator for RustyLineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
        if ctx.input().starts_with("import") {
            return Ok(ValidationResult::Valid(None));
        }

        Ok(validate_arrows(ctx.input())
            .or_else(|| validate_brackets(ctx.input()))
            .unwrap_or(ValidationResult::Valid(None)))
    }
}

/// Verifies whether the last symbol on the line is an arrow
fn validate_arrows(input: &str) -> Option<ValidationResult> {
    let mut iter = input.as_bytes().iter().rev();

    if let Some(b) = iter.find(|b| !(**b).is_ascii_whitespace()) &&
       *b == b'>' && let Some(b) = iter.next() && (*b == b'-' || *b == b'=') {
            return Some(ValidationResult::Incomplete)
        }

    None
}

/// Verifies whether the given line(s) correspond to a bracket-closed word
fn validate_brackets(input: &str) -> Option<ValidationResult> {
    let mut stack = vec![];

    for c in input.chars() {
        match c {
            '(' => stack.push(c),
            ')' => match stack.pop() {
                Some('(') => {},
                Some(_) => {
                    return Some(ValidationResult::Invalid(Some("\nMismatched brackets: ) is not properly closed".to_owned())));
                },
                None => return Some(ValidationResult::Invalid(Some("\nMismatched brackets: ( is unpaired".to_owned()))),
            },
            _ => {},
        }
    }

    if stack.is_empty() { None } else { Some(ValidationResult::Incomplete) }
}

/// A variation of [`MatchingBracketHighlighter`](https://docs.rs/rustyline/latest/rustyline/highlight/struct.MatchingBracketHighlighter.html).
///
/// No check occurs before cursor
impl Highlighter for RustyLineHelper {
    fn highlight_hint<'input>(&self, hint: &'input str) -> Cow<'input, str> {
        if !self.color {
            return Owned(hint.to_owned());
        }
        Owned(format!("{}", hint.bright_black()))
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        self.color
    }

    fn highlight<'input>(&self, line: &'input str, pos: usize) -> Cow<'input, str> {
        if line.len() <= 1 || !self.color {
            return Borrowed(line);
        }
        let mut copy = line.to_owned();

        if let Some((bracket, pos)) = get_bracket(line, pos)
            && let Some((matching, pos)) = find_matching_bracket(line, pos, bracket) {
                let s = String::from(matching);
                copy.replace_range(pos..=pos, &format!("{}", s.blue().bold()));
            }
        KEYWORDS
            .iter()
            .for_each(|keyword| replace_inplace(&mut copy, keyword, &format!("{}", keyword.blue().bold())));
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

/// Finds the position of the bracket associated to the given position, if any
fn find_matching_bracket(line: &str, pos: usize, bracket: u8) -> Option<(char, usize)> {
    let matching_bracket = matching_bracket(bracket);
    let mut to_match: i32 = 1;

    let match_bracket = |b: u8| {
        if b == matching_bracket.try_into().unwrap_or_else(|_| unreachable!()) {
            to_match -= 1_i32;
        } else if b == bracket {
            to_match += 1_i32;
        };
        to_match == 0_i32
    };

    if is_open_bracket(bracket) {
        // forward search
        line[pos + 1..]
            .bytes()
            .position(match_bracket)
            .map(|pos2| (matching_bracket, pos2 + pos + 1))
    } else {
        // backward search
        line[..pos]
            .bytes()
            .rev()
            .position(match_bracket)
            .map(|pos2| (matching_bracket, pos - pos2 - 1))
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

/// Associates the opening (resp. closing) bracket to its closing (resp. opening) counterpart.
const fn matching_bracket(bracket: u8) -> char {
    match bracket {
        b'(' => ')',
        b')' => '(',
        _ => unreachable!(),
    }
}

/// Tests whether the given symbol is a bracket
const fn is_bracket(bracket: u8) -> bool {
    matches!(bracket, b'(' | b')')
}

/// Tests whether the given symbol is an opening bracket
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
    fn get_bracket_not_found() {
        assert!(get_bracket("a()[", 0).is_none());
        assert!(get_bracket("a()[", 3).is_none());
    }

    #[test]
    fn get_bracket_found() {
        assert_eq!(get_bracket("a()[", 1), Some((b'(', 1)));
        assert_eq!(get_bracket("a()[", 2), Some((b')', 2)));
    }

    #[test]
    fn find_matching_bracket_direction() {
        assert!(find_matching_bracket("  )", 1, b')').is_none());
        assert!(find_matching_bracket("(  ", 1, b'(').is_none());

        assert_eq!(find_matching_bracket("  )", 1, b'('), Some((')', 2)));
        assert_eq!(find_matching_bracket("(  ", 1, b')'), Some(('(', 0)));
    }

    #[test]
    fn find_matching_bracket_counter() {
        assert_eq!(find_matching_bracket(" (())))", 0, b'('), Some((')', 5)));
        assert_eq!(find_matching_bracket("(((()) ", 6, b')'), Some(('(', 1)));
    }

    #[test]
    fn replace_inplace() {
        let mut message = "mot motus et mots mot mot".to_owned();

        super::replace_inplace(&mut message, "mot", "mots");

        assert_eq!(message, "mots motus et mots mots mots".to_owned());
    }
}
