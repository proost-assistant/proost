use crate::error::{Error, ErrorKind, Result};
use kernel::{Command, Location, Term};
use num_bigint::BigUint;
use pest::error::LineColLocation;
use pest::iterators::Pair;
use pest::{Parser, Span};
use std::collections::VecDeque;

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

/// convert pest locations to kernel locations
fn convert_span(span: Span) -> Location {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();

    ((x1, y1), (x2, y2)).into()
}

/// build terms from errorless pest's output
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
                let mut iter = pair.into_inner();
                let old_pair = iter.next_back().unwrap();
                for pair in iter {
                    terms.push(build_term_from_expr(old_pair.clone(), known_vars));
                    known_vars.push_front(pair.as_str().to_string());
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
                let mut iter = pair.into_inner();
                let old_pair = iter.next_back().unwrap();
                for pair in iter {
                    terms.push(build_term_from_expr(old_pair.clone(), known_vars));
                    known_vars.push_front(pair.as_str().to_string());
                }
            }
            let t = build_term_from_expr(pair, known_vars);
            terms.into_iter().rev().fold(t, |acc, x| {
                known_vars.pop_front();
                Term::Prod(box x, box acc)
            })
        }
        Rule::Prod => {
            let mut iter = pair.into_inner();
            let pair = iter.next_back().unwrap();
            let mut terms = Vec::new();
            for pair in iter {
                let t = build_term_from_expr(pair, known_vars);
                known_vars.push_front("_".to_string());
                terms.push(t);
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

/// build commands from errorless pest's output
fn build_command_from_expr(pair: Pair<Rule>) -> Command {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
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

/// convert pest error to kernel error
fn convert_error(err: pest::error::Error<Rule>) -> Error {
    // renaming error messages
    let err = err.renamed_rules(|rule| match *rule {
        Rule::string | Rule::Var => "variable".to_owned(),
        Rule::number => "universe level".to_owned(),
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
        _ => unreachable!("low level rules cannot appear in error messages"),
    });

    // extracting the location from the pest output
    let loc = match err.line_col {
        LineColLocation::Pos((x, y)) => {
            let mut right = y;
            let mut left = 1;
            let bytes = err.line().bytes();
            let mut i = 0;
            for c in bytes {
                i += 1;
                if c == b' ' {
                    if i < y {
                        left = i + 1
                    } else {
                        break;
                    }
                } else {
                    right = i
                }
            }
            if i < y {
                left = y;
                right = y;
            }
            Location::new((x, left).into(), (x, right).into())
        }

        LineColLocation::Span(start, end) => Location::new(start.into(), end.into()),
    };

    // extracting the message from the pest output
    let message = err.to_string();
    let mut chars = message.lines().next_back().unwrap().chars();
    for _ in 0..4 {
        chars.next();
    }
    Error {
        kind: ErrorKind::CannotParse(chars.as_str().to_string()),
        location: loc,
    }
}

/// Parse a text input and try to convert it into a command.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_line(line: &str) -> Result<Command> {
    CommandParser::parse(Rule::command, line)
        .map(|mut pairs| build_command_from_expr(pairs.next().unwrap()))
        .map_err(convert_error)
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_file(file: &str) -> Result<Vec<Command>> {
    CommandParser::parse(Rule::file, file)
        .map(|pairs| pairs.into_iter().map(build_command_from_expr).collect())
        .map_err(convert_error)
}

#[cfg(test)]
mod tests {
    use super::Command::*;
    use super::Term::*;
    use super::*;

    /// Error messages
    const COMMAND_ERR: &str =
        "expected def var := term, def var : term := term, check term : term, or check term";
    const TERM_ERR: &str =
        "expected variable, abstraction, dependent product, application, product, Prop, or Type";
    const SIMPLE_TERM_ERR: &str = "expected variable, abstraction, Prop, or Type";
    const UNIVERSE_ERR: &str = "expected universe level, variable, abstraction, Prop, or Type";

    #[test]
    fn failure_universe_level() {
        assert_eq!(
            parse_line("check fun x : Prop -> Type"),
            Err(Error {
                kind: ErrorKind::CannotParse(UNIVERSE_ERR.to_string()),
                location: Location::new((1, 27).into(), (1, 27).into()),
            })
        );
    }

    #[test]
    fn successful_definechecktype() {
        assert_eq!(
            parse_line("def x : Type := Prop"),
            Ok(Define(
                "x".to_string(),
                Some(Type(BigUint::from(0_u64).into())),
                Prop
            ))
        );
    }

    #[test]
    fn successful_define() {
        assert_eq!(
            parse_line("def x := Prop"),
            Ok(Define("x".to_string(), None, Prop))
        );
    }

    #[test]
    fn successful_checktype() {
        assert_eq!(
            parse_line("check Prop : Type"),
            Ok(CheckType(Prop, Type(BigUint::from(0_u64).into())))
        );
    }

    #[test]
    fn successful_gettype_prop() {
        assert_eq!(parse_line("check Prop"), Ok(GetType(Prop)));
    }

    #[test]
    fn successful_var() {
        assert_eq!(parse_line("check A"), Ok(GetType(Const("A".to_string()))));
        assert_eq!(
            parse_line("check fun A:Prop => A"),
            Ok(GetType(Abs(box Prop, box Var(1.into()))))
        );
    }

    #[test]
    fn successful_type() {
        assert_eq!(
            parse_line("check Type"),
            Ok(GetType(Type(BigUint::from(0_u64).into())))
        );
        assert_eq!(
            parse_line("check Type 0"),
            Ok(GetType(Type(BigUint::from(0_u64).into())))
        );
        assert_eq!(
            parse_line("check Type 1"),
            Ok(GetType(Type(BigUint::from(1_u64).into())))
        );
    }

    #[test]
    fn successful_app() {
        assert_eq!(
            parse_line("check A B C"),
            Ok(GetType(App(
                box App(box Const("A".to_string()), box Const("B".to_string())),
                box Const("C".to_string())
            )))
        );
        assert_eq!(
            parse_line("check (A B) C"),
            Ok(GetType(App(
                box App(box Const("A".to_string()), box Const("B".to_string())),
                box Const("C".to_string())
            )))
        );
        assert_eq!(
            parse_line("check A (B C)"),
            Ok(GetType(App(
                box Const("A".to_string()),
                box App(box Const("B".to_string()), box Const("C".to_string()))
            )))
        );
    }

    #[test]
    fn successful_prod() {
        assert_eq!(
            parse_line("check A -> B -> C"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Const("C".to_string()))
            )))
        );
        assert_eq!(
            parse_line("check A -> (B -> C)"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Const("C".to_string()))
            )))
        );
        assert_eq!(
            parse_line("check (A -> B) -> C"),
            Ok(GetType(Prod(
                box Prod(box Const("A".to_string()), box Const("B".to_string())),
                box Const("C".to_string())
            )))
        );
    }

    #[test]
    fn successful_dprod() {
        assert_eq!(
            parse_line("check (x:A) -> (y:B) -> x"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Var(2.into()))
            )))
        );
        assert_eq!(
            parse_line("check (x:A) -> ((y:B) -> x)"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Var(2.into()))
            )))
        );
    }

    #[test]
    fn successful_abs() {
        assert_eq!(
            parse_line("check fun w x : Prop, y z : Prop => x"),
            Ok(GetType(Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(box Prop, box Abs(box Prop, box Var(3.into())))
                )
            )))
        );
    }

    #[test]
    fn failed_dprod() {
        assert_eq!(
            parse_line("check (x:A)"),
            Err(Error {
                kind: ErrorKind::CannotParse(SIMPLE_TERM_ERR.to_string()),
                location: Location::new((1, 7).into(), (1, 11).into()),
            })
        );
        assert_eq!(
            parse_line("check (x:A) -> (y:B)"),
            Err(Error {
                kind: ErrorKind::CannotParse(SIMPLE_TERM_ERR.to_string()),
                location: Location::new((1, 16).into(), (1, 20).into()),
            })
        );
    }

    #[test]
    fn context_for_abs_args() {
        assert_eq!(
            parse_line("check fun x : Prop, x : x, x : x => x"),
            Ok(GetType(Abs(
                box Prop,
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check fun x : Prop, x x : x => x"),
            Ok(GetType(Abs(
                box Prop,
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check fun x : Prop, y z : x => z"),
            Ok(GetType(Abs(
                box Prop,
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(2.into()), box Var(1.into()))
                )
            )))
        );
    }

    #[test]
    fn context_for_dprod_args() {
        assert_eq!(
            parse_line("check (x : Prop, x : x, x : x) -> x"),
            Ok(GetType(Prod(
                box Prop,
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check (x : Prop, x x : x) -> x"),
            Ok(GetType(Prod(
                box Prop,
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check (x : Prop, y z : x) -> z"),
            Ok(GetType(Prod(
                box Prop,
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(2.into()), box Var(1.into()))
                )
            )))
        );
    }

    #[test]
    fn parenthesis_in_abs() {
        assert_eq!(
            parse_line("check fun (((w x : Prop))), y z : Prop => x"),
            Ok(GetType(Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(box Prop, box Abs(box Prop, box Var(3.into())))
                )
            )))
        );
    }

    #[test]
    fn parenthesis_in_prod() {
        assert_eq!(
            parse_line("check (((A))) -> (((B -> C)))"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Const("C".to_string()))
            )))
        );
    }

    #[test]
    fn parenthesis_in_dprod() {
        assert_eq!(
            parse_line("check (((x:A))) -> ((((y:B) -> x)))"),
            Ok(GetType(Prod(
                box Const("A".to_string()),
                box Prod(box Const("B".to_string()), box Var(2.into()))
            )))
        );
    }

    #[test]
    fn parenthesis_in_app() {
        assert_eq!(
            parse_line("check ((((((A))) (((B C))))))"),
            Ok(GetType(App(
                box Const("A".to_string()),
                box App(box Const("B".to_string()), box Const("C".to_string()))
            )))
        );
    }

    #[test]
    fn successful_parsers() {
        let file = r#"
            def x := Prop -> Prop

            // this is a comment
            check fun x:Prop => x
        "#;

        assert_eq!(
            parse_file(file).unwrap()[0],
            parse_line("def x := Prop -> Prop").unwrap()
        );
        assert_eq!(
            parse_file(file).unwrap()[1],
            parse_line("check fun x:Prop => x").unwrap()
        );
    }

    #[test]
    fn successful_convert_error() {
        assert_eq!(
            parse_line("chehk 2x"),
            Err(Error {
                kind: ErrorKind::CannotParse(COMMAND_ERR.to_string()),
                location: Location::new((1, 1).into(), (1, 5).into()),
            })
        );
        assert_eq!(
            parse_line("check 2x"),
            Err(Error {
                kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                location: Location::new((1, 7).into(), (1, 8).into()),
            })
        );
        assert_eq!(
            parse_line("check x:"),
            Err(Error {
                kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                location: Location::new((1, 9).into(), (1, 9).into()),
            })
        );
    }

    #[test]
    fn failed_parsers() {
        assert_eq!(
            parse_file(
                "def x : Type := Prop -> Prop
                 // this is a comment
                        check .x"
            ),
            Err(Error {
                kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                location: Location::new((3, 31).into(), (3, 32).into()),
            })
        );
    }
}
