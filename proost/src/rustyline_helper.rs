use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::HistoryHinter;
use rustyline::validate::{
    MatchingBracketValidator, ValidationContext, ValidationResult, Validator,
};
use rustyline::{Context, Result};
use rustyline_derive::{Helper, Hinter};
use std::borrow::Cow::{self, Borrowed, Owned};
use std::cell::Cell;

/// An Helper for a RustyLine Editor that implements:
/// - a standard hinter
/// - customs validator, completer and highlighter
#[derive(Helper, Hinter)]
pub struct RustyLineHelper {
    bracket: Cell<Option<(u8, usize)>>,
    completer: FilenameCompleter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl RustyLineHelper {
    /// Constructor
    pub fn new() -> Self {
        Self {
            bracket: Cell::new(None),
            completer: FilenameCompleter::new(),
            validator: MatchingBracketValidator::new(),
            hinter: HistoryHinter {},
        }
    }
}

/// A variation of FilenameCompleter:
/// file completion is available only after having typed import
impl Completer for RustyLineHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Result<(usize, Vec<Pair>)> {
        if line.starts_with("import") {
            self.completer.complete_path(line, pos)
        } else {
            Ok((0, vec![]))
        }
    }
}

/// A variation of MatchingBracketValidator:
/// no validation occurs when entering the import command
impl Validator for RustyLineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
        if ctx.input().starts_with("import") {
            Ok(ValidationResult::Valid(None))
        } else {
            self.validator.validate(ctx)
        }
    }
}

/// A variation of MatchingBrackerHighlighter:
/// no check occurs before cursor
/// see: https://docs.rs/rustyline/10.0.0/rustyline/highlight/struct.MatchingBracketHighlighter.html
impl Highlighter for RustyLineHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        if line.len() <= 1 {
            Borrowed(line)
        } else {
            let mut copy = line.to_owned();

            if let Some((bracket, pos)) = self.bracket.get()
            && let Some((matching, pos)) = find_matching_bracket(line, pos, bracket) {
                copy.replace_range(pos..=pos, &format!("\x1b[1;34m{}\x1b[0m", matching as char));
            }
            KEYWORDS
                .iter()
                .for_each(|m| copy = copy.replace(m, &format!("\x1b[34m{}\x1b[0m", m)));
            Owned(copy)
        }
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.bracket.set(check_bracket(line, pos));
        true
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
fn check_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
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
        b => b,
    }
}

const fn is_bracket(bracket: u8) -> bool {
    match bracket {
        b'(' | b')' => true,
        _ => false,
    }
}

const fn is_open_bracket(bracket: u8) -> bool {
    match bracket {
        b'(' => true,
        _ => false,
    }
}

/// Language keywords that should be highligted
const KEYWORDS: [&str; 5] = ["def ", "import ", "eval ", "check ", " fun "];
