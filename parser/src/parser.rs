use pest::error::LineColLocation;
use pest::iterators::Pair;
use pest::{Parser, Span};

use crate::command::Command;
use crate::error::{Error, ErrorKind, Result};
use kernel::error::ResultTerm;
use kernel::location::Location;
use kernel::term::{arena::Arena, builders::Builder};

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

/// convert pest locations to kernel locations
fn convert_span(span: Span) -> Location {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();
    ((x1, y1), (x2, y2)).into()
}

/// Returns a kernel term builder from pest output
fn builder_from_parser(pair: Pair<Rule>) -> Builder {
    use Builder::*;

    // location to be used in a future version
    let _loc = convert_span(pair.as_span());

    match pair.as_rule() {
        Rule::Prop => Prop,

        Rule::Var => Var(pair.into_inner().as_str()),

        Rule::Type => Type(
            pair.into_inner()
                .fold(0, |_, x| x.as_str().parse::<usize>().unwrap()),
        ),

        Rule::App => {
            let mut iter = pair.into_inner().map(builder_from_parser);
            let t = iter.next().unwrap();
            iter.fold(t, |acc, x| App(Box::new(acc), Box::new(x)))
        }

        Rule::Abs => {
            let mut iter = pair.into_inner();
            let body = builder_from_parser(iter.next_back().unwrap());
            iter.flat_map(|pair| {
                let mut pair = pair.into_inner();
                let type_ = Box::new(builder_from_parser(pair.next_back().unwrap()));
                pair.map(move |var| (var.as_str(), type_.clone()))
            })
            .rev()
            .fold(body, |acc, (var, type_)| Abs(var, type_, Box::new(acc)))
        }

        Rule::dProd => {
            let mut iter = pair.into_inner();
            let body = builder_from_parser(iter.next_back().unwrap());
            iter.flat_map(|pair| {
                let mut pair = pair.into_inner();
                let type_ = Box::new(builder_from_parser(pair.next_back().unwrap()));
                pair.map(move |var| (var.as_str(), type_.clone()))
            })
            .rev()
            .fold(body, |acc, (var, type_)| Prod(var, type_, Box::new(acc)))
        }

        Rule::Prod => {
            let mut iter = pair.into_inner();
            let ret = builder_from_parser(iter.next_back().unwrap());
            iter.map(builder_from_parser)
                .rev()
                .fold(ret, |acc, argtype| {
                    Prod("_", Box::new(argtype), Box::new(acc))
                })
        }

        term => unreachable!("Unexpected term: {:?}", term),
    }
}

/// build terms from errorless pest's output
fn build_term_from_expr<'arena>(arena: &mut Arena<'arena>, pair: Pair<Rule>) -> ResultTerm<'arena> {
    builder_from_parser(pair).realise(arena)
}

/// build commands from errorless pest's output
fn build_command_from_expr<'arena, 'build>(
    arena: &mut Arena<'arena>,
    pair: Pair<'build, Rule>,
) -> kernel::error::Result<'arena, Command<'build, 'arena>> {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());

    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = build_term_from_expr(arena, iter.next().unwrap())?;
            Ok(Command::GetType(t))
        }

        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = build_term_from_expr(arena, iter.next().unwrap())?;
            let t2 = build_term_from_expr(arena, iter.next().unwrap())?;
            Ok(Command::CheckType(t1, t2))
        }

        Rule::Define => {
            let mut iter = pair.into_inner();
            let s: &'build str = iter.next().unwrap().as_str();
            let term = build_term_from_expr(arena, iter.next().unwrap())?;
            Ok(Command::Define(s, None, term))
        }

        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s: &'build str = iter.next().unwrap().as_str();
            let t = build_term_from_expr(arena, iter.next().unwrap())?;
            let term = build_term_from_expr(arena, iter.next().unwrap())?;
            Ok(Command::Define(s, Some(t), term))
        }

        Rule::ImportFile => {
            let files = pair
                .into_inner()
                .map(|pair| pair.as_str().to_string())
                .collect();
            Ok(Command::Import(files))
        }

        command => unreachable!("Unexpected command: {:?}", command),
    }
}

/// convert pest error to kernel error
fn convert_error<'arena>(err: pest::error::Error<Rule>) -> Error<'arena> {
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
            let chars = err.line().chars();
            let mut i = 0;
            for c in chars {
                i += 1;
                if char::is_whitespace(c) {
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
pub fn parse_line<'arena, 'build>(
    arena: &mut Arena<'arena>,
    line: &'build str,
) -> Result<'arena, Command<'build, 'arena>> {
    CommandParser::parse(Rule::command, line)
        .map_err(convert_error)
        .and_then(|mut pairs| {
            build_command_from_expr(arena, pairs.next().unwrap()).map_err(|err| Error {
                kind: ErrorKind::EarlyKernelError(err),
                location: Location::default(),
            })
        })
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_file<'arena, 'build>(
    arena: &mut Arena<'arena>,
    file: &'build str,
) -> Result<'arena, Vec<Command<'build, 'arena>>> {
    CommandParser::parse(Rule::file, file)
        .map_err(convert_error)
        .and_then(|pairs| {
            pairs
                .into_iter()
                .map(|line| build_command_from_expr(arena, line))
                .collect::<kernel::error::Result<Vec<Command<'_, '_>>>>()
                .map_err(|err| Error {
                    kind: ErrorKind::EarlyKernelError(err),
                    location: Location::default(),
                })
        })
}

#[cfg(test)]
mod tests {
    use kernel::term::arena::use_arena;
    use kernel::term::builders::*;

    use super::Command::*;
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
        use_arena(|arena| {
            assert_eq!(
                parse_line(arena, "check fun x : Prop -> Type"),
                Err(Error {
                    kind: ErrorKind::CannotParse(UNIVERSE_ERR.to_string()),
                    location: Location::new((1, 27).into(), (1, 27).into()),
                })
            );
        })
    }

    #[test]
    fn successful_definechecktype() {
        use_arena(|arena| {
            let type_0 = arena.build(type_usize(0)).unwrap();
            let prop = arena.build(prop()).unwrap();
            assert_eq!(
                parse_line(arena, "def x : Type := Prop"),
                Ok(Define("x", Some(type_0), prop))
            );
        })
    }

    #[test]
    fn successful_define() {
        use_arena(|arena| {
            assert_eq!(
                parse_line(arena, "def x := Prop"),
                Ok(Define("x", None, arena.build(prop()).unwrap()))
            );
        })
    }

    #[test]
    fn successful_checktype() {
        use_arena(|arena| {
            let type_0 = arena.build(type_usize(0)).unwrap();
            assert_eq!(
                parse_line(arena, "check Prop : Type"),
                Ok(CheckType(arena.build(prop()).unwrap(), type_0))
            );
        })
    }

    #[test]
    fn successful_gettype_prop() {
        use_arena(|arena| {
            let prop = arena.build(prop()).unwrap();
            assert_eq!(parse_line(arena, "check Prop"), Ok(GetType(prop)));
        })
    }

    #[test]
    fn successful_var() {
        use_arena(|arena| {
            let res = arena.build(abs("A", prop(), var("A"))).unwrap();
            assert_eq!(parse_line(arena, "check fun A:Prop => A"), Ok(GetType(res)));
        })
    }

    #[test]
    fn successful_type() {
        use_arena(|arena| {
            let type_0 = arena.build(type_usize(0)).unwrap();
            let type_1 = arena.build(type_usize(1)).unwrap();
            assert_eq!(parse_line(arena, "check Type"), Ok(GetType(type_0)));
            assert_eq!(parse_line(arena, "check Type 0"), Ok(GetType(type_0)));
            assert_eq!(parse_line(arena, "check Type 1"), Ok(GetType(type_1)));
        })
    }

    #[test]
    fn successful_app() {
        use_arena(|arena| {
            let res_left = arena
                .build(abs(
                    "A",
                    prop(),
                    abs(
                        "B",
                        prop(),
                        abs("C", prop(), app(app(var("A"), var("B")), var("C"))),
                    ),
                ))
                .unwrap();
            let res_right = arena
                .build(abs(
                    "A",
                    prop(),
                    abs(
                        "B",
                        prop(),
                        abs("C", prop(), app(var("A"), app(var("B"), var("C")))),
                    ),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => A B C"),
                Ok(GetType(res_left))
            );
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => (A B) C"),
                Ok(GetType(res_left))
            );
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => A (B C)"),
                Ok(GetType(res_right))
            );
        })
    }

    #[test]
    fn successful_prod() {
        use_arena(|arena| {
            let res_left = arena
                .build(abs(
                    "A",
                    prop(),
                    abs(
                        "B",
                        prop(),
                        abs(
                            "C",
                            prop(),
                            prod("_", prod("_", var("A"), var("B")), var("C")),
                        ),
                    ),
                ))
                .unwrap();
            let res_right = arena
                .build(abs(
                    "A",
                    prop(),
                    abs(
                        "B",
                        prop(),
                        abs(
                            "C",
                            prop(),
                            prod("_", var("A"), prod("_", var("B"), var("C"))),
                        ),
                    ),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => A -> B -> C"),
                Ok(GetType(res_right))
            );
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => A -> (B -> C)"),
                Ok(GetType(res_right))
            );
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => (A -> B) -> C"),
                Ok(GetType(res_left))
            );
        })
    }

    #[test]
    fn successful_dprod() {
        use_arena(|arena| {
            let res = arena
                .build(prod("x", type_usize(0), prod("y", type_usize(1), var("x"))))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check (x:Type) -> (y:Type 1) -> x"),
                Ok(GetType(res))
            );
            assert_eq!(
                parse_line(arena, "check (x:Type) -> ((y:Type 1) -> x)"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn successful_abs() {
        use_arena(|arena| {
            let res = arena
                .build(abs(
                    "w",
                    prop(),
                    abs("x", prop(), abs("y", prop(), abs("z", prop(), var("x")))),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun w x: Prop, y z: Prop => x"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn failed_dprod() {
        use_arena(|arena| {
            assert_eq!(
                parse_line(arena, "check (x:A)"),
                Err(Error {
                    kind: ErrorKind::CannotParse(SIMPLE_TERM_ERR.to_string()),
                    location: Location::new((1, 7).into(), (1, 11).into()),
                })
            );
            assert_eq!(
                parse_line(arena, "check (x:A) -> (y:B)"),
                Err(Error {
                    kind: ErrorKind::CannotParse(SIMPLE_TERM_ERR.to_string()),
                    location: Location::new((1, 16).into(), (1, 20).into()),
                })
            );
        })
    }

    #[test]
    fn context_for_abs_args() {
        use_arena(|arena| {
            let res = arena
                .build(abs(
                    "x",
                    prop(),
                    abs("x", var("x"), abs("x", var("x"), var("x"))),
                ))
                .unwrap();
            let res2 = arena
                .build(abs(
                    "x",
                    prop(),
                    abs("y", var("x"), abs("z", var("x"), var("z"))),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun x : Prop, x : x, x : x => x"),
                Ok(GetType(res))
            );
            assert_eq!(
                parse_line(arena, "check fun x : Prop, x x : x => x"),
                Ok(GetType(res))
            );
            assert_eq!(
                parse_line(arena, "check fun x : Prop, y z : x => z"),
                Ok(GetType(res2))
            );
        })
    }

    #[test]
    fn context_for_dprod_args() {
        use_arena(|arena| {
            let res = arena
                .build(prod(
                    "x",
                    prop(),
                    prod("x", var("x"), prod("x", var("x"), var("x"))),
                ))
                .unwrap();
            let res2 = arena
                .build(prod(
                    "x",
                    prop(),
                    prod("y", var("x"), prod("z", var("x"), var("z"))),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check (x : Prop, x : x, x : x) -> x"),
                Ok(GetType(res))
            );
            assert_eq!(
                parse_line(arena, "check (x : Prop, x x : x) -> x"),
                Ok(GetType(res))
            );
            assert_eq!(
                parse_line(arena, "check (x : Prop, y z : x) -> z"),
                Ok(GetType(res2))
            );
        })
    }

    #[test]
    fn parenthesis_in_abs() {
        use_arena(|arena| {
            let res = arena
                .build(abs(
                    "w",
                    prop(),
                    abs("x", prop(), abs("y", prop(), abs("z", prop(), var("x")))),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun (((w x : Prop))), y z : Prop => x"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn parenthesis_in_prod() {
        use_arena(|arena| {
            let res = arena
                .build(prod(
                    "_",
                    type_usize(0),
                    prod("_", type_usize(1), type_usize(2)),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check (((Type))) -> (((Type 1 -> Type 2)))"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn parenthesis_in_dprod() {
        use_arena(|arena| {
            let res = arena
                .build(prod("x", type_usize(0), prod("y", type_usize(1), var("x"))))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check (((x:Type))) -> ((((y:Type 1) -> x)))"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn parenthesis_in_app() {
        use_arena(|arena| {
            let res = arena
                .build(abs(
                    "A",
                    prop(),
                    abs(
                        "B",
                        prop(),
                        abs("C", prop(), app(var("A"), app(var("B"), var("C")))),
                    ),
                ))
                .unwrap();
            assert_eq!(
                parse_line(arena, "check fun A B C: Prop => ((((((A))) (((B C))))))"),
                Ok(GetType(res))
            );
        })
    }

    #[test]
    fn successful_parsers() {
        use_arena(|arena| {
            let file = r#"
            def x := Prop -> Prop

            // this is a comment
            check fun x:Prop => x
        "#;

            assert_eq!(
                parse_file(arena, file).unwrap()[0],
                parse_line(arena, "def x := Prop -> Prop").unwrap()
            );
            assert_eq!(
                parse_file(arena, file).unwrap()[1],
                parse_line(arena, "check fun x:Prop => x").unwrap()
            );
        })
    }

    #[test]
    fn successful_convert_error() {
        use_arena(|arena| {
            assert_eq!(
                parse_line(arena, "chehk 2x"),
                Err(Error {
                    kind: ErrorKind::CannotParse(COMMAND_ERR.to_string()),
                    location: Location::new((1, 1).into(), (1, 5).into()),
                })
            );
            assert_eq!(
                parse_line(arena, "check 2x"),
                Err(Error {
                    kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                    location: Location::new((1, 7).into(), (1, 8).into()),
                })
            );
            assert_eq!(
                parse_line(arena, "check x:"),
                Err(Error {
                    kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                    location: Location::new((1, 9).into(), (1, 9).into()),
                })
            );
        })
    }

    #[test]
    fn failed_parsers() {
        use_arena(|arena| {
            assert_eq!(
                parse_file(
                    arena,
                    "def x : Type := Prop -> Prop
                 // this is a comment
                        check .x"
                ),
                Err(Error {
                    kind: ErrorKind::CannotParse(TERM_ERR.to_string()),
                    location: Location::new((3, 31).into(), (3, 32).into()),
                })
            );
        })
    }
}
