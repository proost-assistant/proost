use kernel::num_bigint::BigUint;
use kernel::{Command, KernelError, Loc, Term};
use pest::error::{Error, LineColLocation};
use pest::iterators::Pair;
use pest::{Parser, Span};
use std::cmp::Ordering;
use std::collections::VecDeque;

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

// convert pest location to kernel location
fn convert_span(span: Span) -> Loc {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();
    Loc::new(x1, y1, x2, y2)
}

fn build_term_from_expr(pair: Pair<Rule>, known_vars: &mut VecDeque<String>) -> Term {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());
    match pair.as_rule() {
        Rule::Prop => Term::Prop,
        Rule::Type => Term::Type(pair.into_inner().fold(BigUint::from(0_u64).into(), |_, x| {
            x.as_str().parse::<BigUint>().unwrap().into()
        })),
        Rule::Var => {
            let name = pair.into_inner().as_str().to_string();
            match known_vars.iter().position(|x| *x == name) {
                Some(i) => Term::Var((i + 1).into()),
                None => Term::Const(name),
            }
        }
        Rule::Prod => {
            let mut iter = pair
                .into_inner()
                .map(|x| build_term_from_expr(x, known_vars))
                .rev();
            let t = iter.next().unwrap();
            iter.fold(t, |acc, x| Term::Prod(box x, box acc))
        }
        Rule::App => {
            let mut iter = pair
                .into_inner()
                .map(|x| build_term_from_expr(x, known_vars));
            let t = iter.next().unwrap();
            iter.fold(t, |acc, x| Term::App(box acc, box x))
        }
        Rule::Abs => {
            let mut iter = pair.into_inner();
            let pair = iter.next_back().unwrap();
            let mut terms = Vec::new();
            for pair in iter {
                let mut iter = pair.into_inner().rev();
                let t = build_term_from_expr(iter.next().unwrap(), known_vars);
                for pair in iter {
                    known_vars.push_front(pair.as_str().to_string());
                    terms.push(t.clone());
                }
            }
            let t = build_term_from_expr(pair, known_vars);
            terms.into_iter().rev().fold(t, |acc, x| {
                known_vars.pop_front();
                Term::Abs(box x, box acc)
            })
        }
        Rule::dProd => {
            let mut iter = pair.into_inner();
            let pair = iter.next_back().unwrap();
            let mut terms = Vec::new();
            for pair in iter {
                let mut iter = pair.into_inner().rev();
                let t = build_term_from_expr(iter.next().unwrap(), known_vars);
                for pair in iter {
                    known_vars.push_front(pair.as_str().to_string());
                    terms.push(t.clone());
                }
            }
            let t = build_term_from_expr(pair, known_vars);
            terms.into_iter().rev().fold(t, |acc, x| {
                known_vars.pop_front();
                Term::Prod(box x, box acc)
            })
        }
        term => unreachable!("Unexpected term: {:?}", term),
    }
}

fn build_command_from_expr(pair: Pair<Rule>) -> Command {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            //T DEBUG
            println!("{}", t);
            Command::GetType(t)
        }
        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            let t2 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            Command::CheckType(t1, t2)
        }
        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let term = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            Command::Define(s, None, term)
        }
        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            let term = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            Command::Define(s, Some(t), term)
        }
        command => unreachable!("Unexpected command: {:?}", command),
    }
}

// convert pest error to kernel error
fn convert_error(err: Error<Rule>) -> KernelError {
    // renaming error messages
    let err = err.renamed_rules(|rule| match *rule {
        Rule::string | Rule::Var => "variable".to_owned(),
        Rule::number => "number".to_owned(),
        Rule::Define => "def var := term".to_owned(),
        Rule::CheckType => "check term : term".to_owned(),
        Rule::GetType => "check term".to_owned(),
        Rule::DefineCheckType => "def var : term := term".to_owned(),
        Rule::Abs => "abstraction".to_owned(),
        Rule::dProd => "dependent product".to_owned(),
        Rule::Prod => "product".to_owned(),
        Rule::App => "application".to_owned(),
        Rule::Prop => "Prop".to_owned(),
        Rule::Type => "Type".to_owned(),
        _ => "".to_owned(),
    });

    // extracting the location from the pest output
    let loc = match err.line_col {
        LineColLocation::Pos((x, y)) => {
            let mut right = y;
            let mut left = 1;
            let chars = err.line().chars();
            let mut i = 0;
            for c in chars {
                i += 1;
                if char::is_whitespace(c) {
                    match i.cmp(&y) {
                        Ordering::Less => left = i + 1,
                        Ordering::Equal => left = y,
                        Ordering::Greater => break,
                    }
                } else {
                    right = i
                }
            }
            if i < y {
                left = y;
                right = y;
            }
            Loc::new(x, left, x, right)
        }
        LineColLocation::Span((x1, y1), (x2, y2)) => Loc::new(x1, y1, x2, y2),
    };

    // extracting the message from the pest output
    let message = err.to_string();
    let mut chars = message.lines().next_back().unwrap().chars();
    for _ in 0..4 {
        chars.next();
    }
    KernelError::CannotParse(loc, chars.as_str().to_string())
}

/// Parse a text input and try to convert it into a command.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_line(line: &str) -> Result<Command, KernelError> {
    match CommandParser::parse(Rule::command, line) {
        Ok(mut pairs) => Ok(build_command_from_expr(pairs.next().unwrap())),
        Err(err) => Err(convert_error(err)),
    }
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_file(file: &str) -> Result<Vec<Command>, KernelError> {
    match CommandParser::parse(Rule::file, file) {
        Ok(pairs) => Ok(pairs.into_iter().map(build_command_from_expr).collect()),
        Err(err) => Err(convert_error(err)),
    }
}
