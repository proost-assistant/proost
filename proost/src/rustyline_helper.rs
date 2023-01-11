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

/// The state of the token being read
#[derive(PartialEq, Eq)]
enum TokenState {
    /// Alphanumeric sequence started
    Started,
    /// Alphanumeric sequence complete (terminated by a non-alphanumeric char)
    Complete,
    /// Special case to group "n : T" as a single token
    /// The token is `Awaiting` between char ':' and the first
    /// char of the type name.
    Awaiting,
}

/// A cursor that explores a string char by char
/// and traces the last column position
struct ColumnTracer {
    /// Byte position in the string
    global_pos: usize,
    /// Char position in the current line
    current_pos: u16,
    /// Indicates whether the line only
    /// contains whitespaces so far
    line_empty: bool,
    /// Position of the last column found
    last_column: Option<u16>,
    /// Position of opening parenthesis
    parenthesis: Vec<u16>,
    /// Indicates the state of the last token
    /// (token = alphanumeric sequence, inside of parenthesis, etc.)
    token_state: TokenState,
}

impl ColumnTracer {
    /// Create a new tracer
    const fn new() -> Self {
        // current_pos = 2 due to offset in the first line
        Self {
            global_pos: 0,
            line_empty: true,
            current_pos: 2,
            last_column: None,
            parenthesis: vec![],
            token_state: TokenState::Complete,
        }
    }

    /// Tells whether `c` is a line-termination char.
    const fn is_new_line(c: char) -> bool {
        c == '\n' || c == '\r'
    }

    /// Reads and returns one char from the string, while updating the state of
    /// the tracer.
    /// Returns None if the end of the string is reached.
    fn read_char(&mut self, chars: &mut std::str::Chars, limit: usize) -> Option<char> {
        match chars.next() {
            Some(c) => {
                if self.global_pos >= limit && (!self.line_empty || !c.is_whitespace() || Self::is_new_line(c)) {
                    if !self.line_empty {
                        // Non whitespaces are found before the cursor on the same line
                        // Giving up
                        self.last_column = None;
                    }
                    self.line_empty = self.line_empty && Self::is_new_line(c);
                    return None;
                }

                self.current_pos += 1;
                self.global_pos += c.len_utf8();

                if Self::is_new_line(c) {
                    self.current_pos = 0;
                    self.line_empty = true;
                } else if c.is_whitespace() {
                    if self.token_state != TokenState::Awaiting {
                        self.token_state = TokenState::Complete;
                    }
                } else {
                    self.line_empty = false;
                }

                Some(c)
            },
            None => None,
        }
    }

    /// Returns the next non-whitespace character, consuming all the whitespace characters between
    /// it and the current position. Returns None if the end of the string is reached without finding
    /// a non-whitespace char.
    fn consume_spaces(&mut self, chars: &mut std::str::Chars, limit: usize) -> Option<char> {
        match self.read_char(chars, limit) {
            Some(c) if !c.is_whitespace() => Some(c),
            Some(_) => self.consume_spaces(chars, limit),
            None => None,
        }
    }

    /// Updates the tracer by continuing reading the string from the current position.
    /// limit is the position of the editor's cursor.
    /// When this function returns, if `line_empty` is true, `current_pos` is the position
    /// of the first non-whitespace character on the same line as the cursor (or the length of the
    /// line, if it is blank).
    /// If `line_empty` is false, `current_pos` is the column position of the cursor.
    fn process_chars(&mut self, chars: &mut std::str::Chars, limit: usize) {
        loop {
            match self.consume_spaces(chars, limit) {
                Some(':') if self.last_column.is_some() => {
                    // Considering "n : T" as a single token
                    self.token_state = TokenState::Awaiting;
                },
                Some('=') => {
                    // Resetting column when reading "=>" or ":="
                    self.last_column = None;
                    self.token_state = TokenState::Complete;
                },
                Some('(') => {
                    self.parenthesis.push(self.current_pos - 1);
                    self.last_column = Some(self.current_pos - 1);
                    self.token_state = TokenState::Complete;
                },
                Some(')') => {
                    self.last_column = self.parenthesis.pop();
                    self.token_state = TokenState::Complete;
                },
                Some(c) if c.is_alphanumeric() => {
                    if self.token_state == TokenState::Complete {
                        // Starting a new token
                        self.last_column = Some(self.current_pos - 1);
                    }
                    self.token_state = TokenState::Started;
                },
                None => return,
                _ => (),
            }
        }
    }
}

/// A Handler for the tab event
pub struct TabEventHandler;
impl TabEventHandler {
    /// Get the column at which the tab key should move.
    fn column(ctx: &EventContext) -> ColumnTracer {
        let mut chars = ctx.line().chars();
        let mut cursor = ColumnTracer::new();

        cursor.process_chars(&mut chars, ctx.pos());
        cursor
    }
}
impl ConditionalEventHandler for TabEventHandler {
    fn handle(&self, _: &Event, n: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
        if ctx.line().starts_with("import") {
            return None;
        }
        let cur = Self::column(ctx);

        match cur.last_column {
            _ if cur.global_pos > ctx.pos() => {
                if cur.line_empty {
                    // Line blank
                    // Move the cursor at the end of the line
                    Some(Cmd::Move(rustyline::Movement::EndOfLine))
                } else {
                    // Cursor before the first word (move it to the first word of the line)
                    // case "     •     word        ?" where • is the cursor
                    Some(Cmd::Move(rustyline::Movement::ViFirstPrint))
                }
            },
            // Column found, cursor well-located
            // case "           •ord        ○" where ○ is the position of the column
            Some(pos) if cur.current_pos < pos => Some(Cmd::Insert(1, " ".repeat((pos - cur.current_pos).into()))),
            // Cursor after the beginning of the line, or no column found
            // case "           wo•d        ○"
            // or   "           •ord         "
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
