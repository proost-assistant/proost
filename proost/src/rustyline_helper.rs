use crate::replace_word::*;
use colored::*;
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::HistoryHinter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Context, Result};
use rustyline_derive::{Helper, Hinter};
use std::borrow::Cow::{self, Borrowed, Owned};

/// An Helper for a RustyLine Editor that implements:
/// - a standard hinter
/// - customs validator, completer and highlighter
#[derive(Helper, Hinter)]
pub struct RustyLineHelper {
    completer: FilenameCompleter,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl RustyLineHelper {
    pub fn new() -> Self {
        Self {
            completer: FilenameCompleter::new(),
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
            Ok(Default::default())
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
            Ok(validate_arrows(ctx.input()).unwrap_or(validate_brackets(ctx.input())))
        }
    }
}

fn validate_arrows(input: &str) -> Option<ValidationResult> {
    let mut iter = input.as_bytes().into_iter().rev();
    if let Some(b) = iter.find(|b| **b != b' ') {
        if *b == b'>' && let Some(b) = iter.next() && (*b == b'-' || *b == b'=') {
            return Some(ValidationResult::Incomplete)
        }
    }
    None
}

fn validate_brackets(input: &str) -> ValidationResult {
    let mut stack = vec![];
    for c in input.chars() {
        match c {
            '(' => stack.push(c),
            ')' => match stack.pop() {
                Some('(') => {}
                Some(_) => {
                    return ValidationResult::Invalid(Some(
                        "Mismatched brackets: ) is not properly closed".to_string(),
                    ))
                }
                None => {
                    return ValidationResult::Invalid(Some(
                        "Mismatched brackets: ( is unpaired".to_string(),
                    ))
                }
            },
            _ => {}
        }
    }
    if stack.is_empty() {
        ValidationResult::Valid(None)
    } else {
        ValidationResult::Incomplete
    }
}

/// A variation of MatchingBrackerHighlighter:
/// no check occurs before cursor
/// see: https://docs.rs/rustyline/10.0.0/rustyline/highlight/struct.MatchingBracketHighlighter.html
impl Highlighter for RustyLineHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(format!("{}", hint.bold()).to_owned())
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        true
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        if line.len() <= 1 {
            return Borrowed(line);
        }
        let mut copy = line.to_owned();

        if let Some((bracket, pos)) = get_bracket(line, pos)
            && let Some((matching, pos)) = find_matching_bracket(line, pos, bracket) {
                let s = format!("{}", matching as char);
                copy.replace_range(pos..=pos, &format!("{}", s.blue().bold()));
            }
        KEYWORDS
            .iter()
            .for_each(|m| copy = replace_word(&copy, m, &format!("{}", m.blue().bold())));
        Owned(copy)
    }
}

/// Language keywords that should be highligted
const KEYWORDS: [&str; 4] = ["check", "def", "eval", "import"];

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
fn get_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if !line.is_empty() && pos < line.len() {
        let b = line.as_bytes()[pos];
        if is_bracket(b) {
            return Some((b, pos));
        }
    }
    None
}

fn matching_bracket(bracket: u8) -> u8 {
    match bracket {
        b'(' => b')',
        b')' => b'(',
        _ => unreachable!(),
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
