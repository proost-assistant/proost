use kernel::memory::declaration::builder as declaration;
use kernel::memory::level::builder as level;
use kernel::memory::term::builder as term;
use pest::error::LineColLocation;
use pest::iterators::Pair;
use pest::{Parser, Span};
use utils::location::Location;

use crate::command::Command;
use crate::error;
use crate::error::Error;

#[derive(Parser)]
#[grammar = "command/grammar.pest"]
struct CommandParser;

/// Convert pest location [`Span`] to our location [`Location`].
fn convert_span(span: Span) -> Location {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();

    Location::new((x1, y1), (x2, y2))
}

/// build universe level from errorless pest's output
fn parse_level(pair: Pair<Rule>) -> level::Builder {
    use level::Builder::{Const, IMax, Max, Plus, Var};

    match pair.as_rule() {
        Rule::Num => {
            let n = pair.into_inner().as_str().parse().unwrap();
            Const(n)
        },

        Rule::Max => {
            let mut iter = pair.into_inner();
            let univ1 = parse_level(iter.next().unwrap());
            let univ2 = parse_level(iter.next().unwrap());
            Max(box univ1, box univ2)
        },

        Rule::IMax => {
            let mut iter = pair.into_inner();
            let univ1 = parse_level(iter.next().unwrap());
            let univ2 = parse_level(iter.next().unwrap());
            IMax(box univ1, box univ2)
        },

        Rule::Plus => {
            let mut iter = pair.into_inner();
            let univ = parse_level(iter.next().unwrap());
            let n = iter.map(|x| x.as_str().parse::<usize>().unwrap()).sum();
            Plus(box univ, n)
        },

        Rule::string => {
            let name = pair.as_str();
            Var(name)
        },

        univ => unreachable!("unexpected universe level: {univ:?}"),
    }
}

/// Returns a kernel term builder from pest output
fn parse_term(pair: Pair<Rule>) -> term::Builder {
    use term::Builder;
    use term::Payload::{Abs, App, Decl, Prod, Prop, Sort, Type, Var};

    let loc = convert_span(pair.as_span());

    match pair.as_rule() {
        Rule::Prop => Builder::new(loc, Prop),

        Rule::Var => Builder::new(loc, Var(pair.into_inner().as_str())),

        Rule::VarDecl => {
            let mut iter = pair.into_inner();
            let name = iter.next().unwrap().as_str();
            let levels = iter.next().unwrap().into_inner().map(parse_level).collect();

            Builder::new(loc, Decl(box declaration::InstantiatedBuilder::Var(name, levels)))
        },

        Rule::Type => pair.into_inner().next_back().map_or_else(
            || Builder::new(loc, Type(box level::Builder::Const(0))),
            |next| Builder::new(loc, Type(box parse_level(next))),
        ),

        Rule::Sort => pair.into_inner().next_back().map_or_else(
            || Builder::new(loc, Sort(box level::Builder::Const(0))),
            |next| Builder::new(loc, Sort(box parse_level(next))),
        ),

        Rule::App => {
            let mut iter = pair.into_inner().map(parse_term);
            let t = iter.next().unwrap();

            iter.fold(t, |acc, x| Builder::new(loc, App(box acc, box x)))
        },

        Rule::Abs => {
            let mut iter = pair.into_inner();
            let body = parse_term(iter.next_back().unwrap());

            iter.flat_map(|pair| {
                let mut pair = pair.into_inner();
                let type_ = box parse_term(pair.next_back().unwrap());

                pair.map(move |var| (var.as_str(), type_.clone()))
            })
            .rev()
            .fold(body, |acc, (var, type_)| Builder::new(loc, Abs(var, type_, box acc)))
        },

        Rule::dProd => {
            let mut iter = pair.into_inner();
            let body = parse_term(iter.next_back().unwrap());
            iter.flat_map(|pair| {
                let mut pair = pair.into_inner();
                let type_ = box parse_term(pair.next_back().unwrap());

                pair.map(move |var| (var.as_str(), type_.clone()))
            })
            .rev()
            .fold(body, |acc, (var, type_)| Builder::new(loc, Prod(var, type_, box acc)))
        },

        Rule::Prod => {
            let mut iter = pair.into_inner();
            let ret = parse_term(iter.next_back().unwrap());
            iter.map(parse_term)
                .rev()
                .fold(ret, |acc, argtype| Builder::new(loc, Prod("_", box argtype, box acc)))
        },

        term => unreachable!("unexpected term: {term:?}"),
    }
}

/// build commands from errorless pest's output
fn parse_expr(pair: Pair<Rule>) -> Command {
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = parse_term(iter.next().unwrap());

            Command::GetType(t)
        },

        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = parse_term(iter.next().unwrap());
            let t2 = parse_term(iter.next().unwrap());

            Command::CheckType(t1, t2)
        },

        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap();
            let term = parse_term(iter.next().unwrap());

            Command::Define((convert_span(s.as_span()), s.as_str()), None, term)
        },

        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap();
            let ty = parse_term(iter.next().unwrap());
            let term = parse_term(iter.next().unwrap());

            Command::Define((convert_span(s.as_span()), s.as_str()), Some(ty), term)
        },

        Rule::Declaration => {
            let mut iter = pair.into_inner();
            let mut string_decl = iter.next().unwrap().into_inner();
            let s = string_decl.next().unwrap();
            let vars: Vec<&str> = string_decl.next().unwrap().into_inner().map(|name| name.as_str()).collect();
            let body = iter.next().map(parse_term).unwrap();

            Command::Declaration((convert_span(s.as_span()), s.as_str()), None, declaration::Builder::Decl(box body, vars))
        },

        Rule::DeclarationCheckType => {
            let mut iter = pair.into_inner();
            let mut string_decl = iter.next().unwrap().into_inner();
            let s = string_decl.next().unwrap();
            let vars: Vec<&str> = string_decl.next().unwrap().into_inner().map(|name| name.as_str()).collect();

            let ty = parse_term(iter.next().unwrap());
            let decl = iter.next().map(parse_term).unwrap();

            let ty = declaration::Builder::Decl(box ty, vars.clone());
            let decl = declaration::Builder::Decl(box decl, vars);

            Command::Declaration((convert_span(s.as_span()), s.as_str()), Some(ty), decl)
        },

        Rule::Eval => {
            let term = parse_term(pair.into_inner().next().unwrap());

            Command::Eval(term)
        },

        Rule::ImportFile => {
            let files = pair.into_inner().map(|pair| (convert_span(pair.as_span()), pair.as_str())).collect();

            Command::Import(files)
        },

        Rule::Search => {
            let s = pair.into_inner().next().unwrap().as_str();

            Command::Search(s)
        },

        command => unreachable!("Unexpected command: {:?}", command),
    }
}

/// convert pest error to parser error
fn convert_error(err: pest::error::Error<Rule>) -> error::Error {
    // renaming error messages
    let err = err.renamed_rules(|rule| match *rule {
        Rule::string | Rule::Var => "variable".to_owned(),
        Rule::number => "number".to_owned(),
        Rule::Define => "def var := term".to_owned(),
        Rule::Declaration => "def decl.{ vars, ... } := term".to_owned(),
        Rule::DeclarationCheckType => "def decl.{ vars, ... } : term := term".to_owned(),
        Rule::CheckType => "check term : term".to_owned(),
        Rule::GetType => "check term".to_owned(),
        Rule::DefineCheckType => "def var : term := term".to_owned(),
        Rule::Abs => "abstraction".to_owned(),
        Rule::dProd => "dependent product".to_owned(),
        Rule::Prod => "product".to_owned(),
        Rule::App => "application".to_owned(),
        Rule::Prop => "Prop".to_owned(),
        Rule::Type => "Type".to_owned(),
        Rule::Sort => "Sort".to_owned(),
        Rule::Eval => "eval term".to_owned(),
        Rule::filename => "path_to_file".to_owned(),
        Rule::ImportFile => "import path_to_file".to_owned(),
        Rule::Search => "search var".to_owned(),
        Rule::Max => "max".to_owned(),
        Rule::Plus => "plus".to_owned(),
        Rule::IMax => "imax".to_owned(),
        Rule::arg_univ => "universe argument".to_owned(),
        Rule::univ_decl => "universe declaration".to_owned(),
        _ => {
            unreachable!("low level rules cannot appear in error messages")
        },
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
                        left = i + 1;
                    } else {
                        break;
                    }
                } else {
                    right = i;
                }
            }

            if i < y {
                left = y;
                right = y;
            }

            Location::new((x, left), (x, right))
        },

        LineColLocation::Span(start, end) => Location::new(start, end),
    };

    // extracting the message from the pest output
    let message = err.to_string();
    let mut chars = message.lines().next_back().unwrap().chars();

    (0_i32..4_i32).for_each(|_| {
        chars.next();
    });

    Error {
        kind: error::Kind::CannotParse(chars.as_str().to_owned()),
        loc,
    }
}

/// Parse a text input and try to convert it into a command.
///
/// if unsuccessful, a box containing the first error that was encountered is returned.
#[inline]
pub fn line(line: &str) -> error::Result<Command> {
    CommandParser::parse(Rule::command, line)
        .map_err(convert_error)
        .map(|mut pairs| parse_expr(pairs.next().unwrap_or_else(|| unreachable!())))
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// if unsuccessful, a box containing the first error that was encountered is returned.
#[inline]
pub fn file(file: &str) -> error::Result<Vec<Command>> {
    CommandParser::parse(Rule::file, file)
        .map_err(convert_error)
        .map(|pairs| pairs.into_iter().map(parse_expr).collect())
}

#[cfg(test)]
mod tests {
    use error::{Error, Kind};
    use term::Builder;
    use term::Payload::*;

    use super::Command::*;
    use super::*;

    /// Error messages
    const COMMAND_ERR: &str = "expected def var := term, def var : term := term, def decl.{ vars, ... } := term, def decl.{ vars, ... } : term := term, check term : term, check term, eval term, import path_to_file, or search var";
    const TERM_ERR: &str = "expected variable, abstraction, dependent product, application, product, Prop, Type, or Sort";
    const SIMPLE_TERM_ERR: &str = "expected variable, abstraction, Prop, Type, Sort, or universe argument";
    const UNIVERSE_ERR: &str = "expected number, variable, abstraction, Prop, Type, Sort, plus, max, or imax";

    #[test]
    fn failure_universe_level() {
        assert_eq!(
            line("check fun x : Prop -> Type"),
            Err(Error {
                kind: Kind::CannotParse(UNIVERSE_ERR.to_owned()),
                loc: Location::new((1, 27), (1, 27)),
            })
        );
    }

    #[test]
    fn successful_define_with_type_annotation() {
        assert_eq!(
            line("def x : Type := Prop"),
            Ok(Define(
                (Location::new((1, 5), (1, 6)), "x"),
                Some(Builder::new(Location::new((1, 9), (1, 14)), Type(box level::Builder::Const(0)))),
                Builder::new(Location::new((1, 17), (1, 21)), Prop)
            ))
        );
    }

    #[test]
    fn successful_declare_with_type_annotation() {
        assert_eq!(
            line("def x.{u} : Type u := foo.{u}"),
            Ok(Declaration(
                (Location::new((1, 5), (1, 6)), "x"),
                Some(declaration::Builder::Decl(
                    box Builder::new(Location::new((1, 13), (1, 19)), Type(box level::Builder::Var("u"))),
                    ["u"].to_vec()
                )),
                declaration::Builder::Decl(
                    box Builder::new(
                        Location::new((1, 23), (1, 30)),
                        Decl(box declaration::InstantiatedBuilder::Var("foo", [level::Builder::Var("u")].to_vec()))
                    ),
                    ["u"].to_vec()
                )
            ))
        );
    }

    #[test]
    fn successful_import() {
        assert_eq!(line("import "), Ok(Import(vec![])));

        assert_eq!(
            line("import file1 dir/file2"),
            Ok(Import(vec![(Location::new((1, 8), (1, 13)), "file1"), (Location::new((1, 14), (1, 23)), "dir/file2")]))
        );
    }

    #[test]
    fn successful_search() {
        assert_eq!(line("search variable1"), Ok(Search("variable1")));
    }

    #[test]
    fn successful_eval() {
        assert_eq!(line("eval Prop"), Ok(Eval(Builder::new(Location::new((1, 6), (1, 10)), Prop))));
    }

    #[test]
    fn successful_define() {
        assert_eq!(
            line("def x := Prop"),
            Ok(Define((Location::new((1, 5), (1, 6)), "x"), None, Builder::new(Location::new((1, 10), (1, 14)), Prop)))
        );
    }

    #[test]
    fn successful_declare() {
        assert_eq!(
            line("def x.{} := Prop"),
            Ok(Declaration(
                (Location::new((1, 5), (1, 6)), "x"),
                None,
                declaration::Builder::Decl(box Builder::new(Location::new((1, 13), (1, 17)), Prop), vec![])
            ))
        );
    }

    #[test]
    fn successful_checktype() {
        assert_eq!(
            line("check Prop : Type"),
            Ok(CheckType(
                Builder::new(Location::new((1, 7), (1, 11)), Prop),
                Builder::new(Location::new((1, 14), (1, 18)), Type(box level::Builder::Const(0)))
            ))
        );
    }

    #[test]
    fn successful_gettype_prop() {
        assert_eq!(line("check Prop"), Ok(GetType(Builder::new(Location::new((1, 7), (1, 11)), Prop))));
    }

    #[test]
    fn successful_gettype_sort() {
        assert_eq!(
            line("check Sort"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 11)), Sort(box level::Builder::Const(0)))))
        );
    }

    #[test]
    fn successful_var() {
        assert_eq!(
            line("check fun A: Prop => A"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 23)),
                Abs(
                    "A",
                    Box::new(Builder::new(Location::new((1, 14), (1, 18)), Prop)),
                    Box::new(Builder::new(Location::new((1, 22), (1, 23)), Var("A")))
                )
            )))
        );
    }

    #[test]
    fn successful_type() {
        assert_eq!(
            line("check Type"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 11)), Type(box level::Builder::Const(0)))))
        );

        assert_eq!(
            line("check Type 0"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 13)), Type(box level::Builder::Const(0)))))
        );

        assert_eq!(
            line("check Type 1"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 13)), Type(box level::Builder::Const(1)))))
        );
    }

    #[test]
    fn successful_sort() {
        assert_eq!(
            line("check Sort"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 11)), Sort(box level::Builder::Const(0)))))
        );

        assert_eq!(
            line("check Sort 0"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 13)), Sort(box level::Builder::Const(0)))))
        );

        assert_eq!(
            line("check Sort 1"),
            Ok(GetType(Builder::new(Location::new((1, 7), (1, 13)), Sort(box level::Builder::Const(1)))))
        );

        assert_eq!(
            line("check Sort (0 + 1)"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 19)),
                Sort(box level::Builder::Plus(box level::Builder::Const(0), 1))
            )))
        );

        assert_eq!(
            line("check Sort max 0 0"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 19)),
                Sort(box level::Builder::Max(box level::Builder::Const(0), box level::Builder::Const(0)))
            )))
        );

        assert_eq!(
            line("check Sort imax 0 0"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 20)),
                Sort(box level::Builder::IMax(box level::Builder::Const(0), box level::Builder::Const(0)))
            )))
        );
    }

    #[test]
    fn successful_app() {
        assert_eq!(
            line("check A B C"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 12)),
                App(
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 12)),
                        App(
                            Box::new(Builder::new(Location::new((1, 7), (1, 8)), Var("A"))),
                            Box::new(Builder::new(Location::new((1, 9), (1, 10)), Var("B"))),
                        ),
                    )),
                    Box::new(Builder::new(Location::new((1, 11), (1, 12)), Var("C"))),
                ),
            )))
        );

        assert_eq!(
            line("check (A B) C"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 14)),
                App(
                    Box::new(Builder::new(
                        Location::new((1, 8), (1, 11)),
                        App(
                            Box::new(Builder::new(Location::new((1, 8), (1, 9)), Var("A"))),
                            Box::new(Builder::new(Location::new((1, 10), (1, 11)), Var("B"))),
                        ),
                    )),
                    Box::new(Builder::new(Location::new((1, 13), (1, 14)), Var("C"))),
                ),
            )))
        );

        assert_eq!(
            line("check A (B C)"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 14)),
                App(
                    Box::new(Builder::new(Location::new((1, 7), (1, 8)), Var("A"))),
                    Box::new(Builder::new(
                        Location::new((1, 10), (1, 13)),
                        App(
                            Box::new(Builder::new(Location::new((1, 10), (1, 11)), Var("B"))),
                            Box::new(Builder::new(Location::new((1, 12), (1, 13)), Var("C"))),
                        ),
                    )),
                ),
            )))
        );
    }

    #[test]
    fn successful_prod() {
        assert_eq!(
            line("check A -> B -> C"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 18)),
                Prod(
                    "_",
                    Box::new(Builder::new(Location::new((1, 7), (1, 8)), Var("A"))),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 18)),
                        Prod(
                            "_",
                            Box::new(Builder::new(Location::new((1, 12), (1, 13)), Var("B"))),
                            Box::new(Builder::new(Location::new((1, 17), (1, 18)), Var("C"))),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check A -> (B -> C)"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 20)),
                Prod(
                    "_",
                    Box::new(Builder::new(Location::new((1, 7), (1, 8)), Var("A"))),
                    Box::new(Builder::new(
                        Location::new((1, 13), (1, 19)),
                        Prod(
                            "_",
                            Box::new(Builder::new(Location::new((1, 13), (1, 14)), Var("B"))),
                            Box::new(Builder::new(Location::new((1, 18), (1, 19)), Var("C"))),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check (A -> B) -> C"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 20)),
                Prod(
                    "_",
                    Box::new(Builder::new(
                        Location::new((1, 8), (1, 14)),
                        Prod(
                            "_",
                            Box::new(Builder::new(Location::new((1, 8), (1, 9)), Var("A"))),
                            Box::new(Builder::new(Location::new((1, 13), (1, 14)), Var("B"))),
                        ),
                    )),
                    Box::new(Builder::new(Location::new((1, 19), (1, 20)), Var("C"))),
                ),
            )))
        );
    }

    #[test]
    fn successful_dprod() {
        assert_eq!(
            line("check (x: Type) -> (y: Type 1) -> x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 36)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 11), (1, 15)), Type(box level::Builder::Const(0)))),
                    Box::new(Builder::new(
                        Location::new((1, 20), (1, 36)),
                        Prod(
                            "y",
                            Box::new(Builder::new(Location::new((1, 24), (1, 30)), Type(box level::Builder::Const(1)))),
                            Box::new(Builder::new(Location::new((1, 35), (1, 36)), Var("x"))),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check (x: Type) -> ((y: Type 1) -> x)"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 38)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 11), (1, 15)), Type(box level::Builder::Const(0)))),
                    Box::new(Builder::new(
                        Location::new((1, 21), (1, 37)),
                        Prod(
                            "y",
                            Box::new(Builder::new(Location::new((1, 25), (1, 31)), Type(box level::Builder::Const(1)))),
                            Box::new(Builder::new(Location::new((1, 36), (1, 37)), Var("x"))),
                        ),
                    )),
                ),
            )))
        );
    }

    #[test]
    fn successful_abs() {
        assert_eq!(
            line("check fun w x: Prop, y z: Prop => x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 36)),
                Abs(
                    "w",
                    Box::new(Builder::new(Location::new((1, 16), (1, 20)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 36)),
                        Abs(
                            "x",
                            Box::new(Builder::new(Location::new((1, 16), (1, 20)), Prop)),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 36)),
                                Abs(
                                    "y",
                                    Box::new(Builder::new(Location::new((1, 27), (1, 31)), Prop)),
                                    Box::new(Builder::new(
                                        Location::new((1, 7), (1, 36)),
                                        Abs(
                                            "z",
                                            Box::new(Builder::new(Location::new((1, 27), (1, 31)), Prop)),
                                            Box::new(Builder::new(Location::new((1, 35), (1, 36)), Var("x"))),
                                        ),
                                    )),
                                ),
                            )),
                        ),
                    )),
                )
            )))
        );
    }

    #[test]
    fn failed_dprod() {
        assert_eq!(
            line("check (x:A)"),
            Err(Error {
                kind: Kind::CannotParse(SIMPLE_TERM_ERR.to_owned()),
                loc: Location::new((1, 7), (1, 11)),
            })
        );
        assert_eq!(
            line("check (x:A) -> (y:B)"),
            Err(Error {
                kind: Kind::CannotParse(SIMPLE_TERM_ERR.to_owned()),
                loc: Location::new((1, 16), (1, 20)),
            })
        );
    }

    #[test]
    fn context_for_abs_args() {
        assert_eq!(
            line("check fun x : Prop, x : x, x : x => x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 38)),
                Abs(
                    "x",
                    Box::new(Builder::new(Location::new((1, 15), (1, 19)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 38)),
                        Abs(
                            "x",
                            Box::new(Builder::new(Location::new((1, 25), (1, 26)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 38)),
                                Abs(
                                    "x",
                                    Box::new(Builder::new(Location::new((1, 32), (1, 33)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 37), (1, 38)), Var("x"))),
                                ),
                            )),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check fun x : Prop, x x : x => x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 33)),
                Abs(
                    "x",
                    Box::new(Builder::new(Location::new((1, 15), (1, 19)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 33)),
                        Abs(
                            "x",
                            Box::new(Builder::new(Location::new((1, 27), (1, 28)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 33)),
                                Abs(
                                    "x",
                                    Box::new(Builder::new(Location::new((1, 27), (1, 28)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 32), (1, 33)), Var("x"))),
                                ),
                            )),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check fun x : Prop, y z : x => z"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 33)),
                Abs(
                    "x",
                    Box::new(Builder::new(Location::new((1, 15), (1, 19)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 33)),
                        Abs(
                            "y",
                            Box::new(Builder::new(Location::new((1, 27), (1, 28)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 33)),
                                Abs(
                                    "z",
                                    Box::new(Builder::new(Location::new((1, 27), (1, 28)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 32), (1, 33)), Var("z")))
                                )
                            )),
                        )
                    ))
                )
            )))
        );
    }

    #[test]
    fn context_for_dprod_args() {
        assert_eq!(
            line("check (x : Prop, x : x, x : x) -> x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 36)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 12), (1, 16)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 36)),
                        Prod(
                            "x",
                            Box::new(Builder::new(Location::new((1, 22), (1, 23)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 36)),
                                Prod(
                                    "x",
                                    Box::new(Builder::new(Location::new((1, 29), (1, 30)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 35), (1, 36)), Var("x"))),
                                ),
                            )),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check (x : Prop, x x : x) -> x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 31)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 12), (1, 16)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 31)),
                        Prod(
                            "x",
                            Box::new(Builder::new(Location::new((1, 24), (1, 25)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 31)),
                                Prod(
                                    "x",
                                    Box::new(Builder::new(Location::new((1, 24), (1, 25)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 30), (1, 31)), Var("x"))),
                                ),
                            )),
                        ),
                    )),
                ),
            )))
        );

        assert_eq!(
            line("check (x : Prop, y z : x) -> z"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 31)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 12), (1, 16)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 31)),
                        Prod(
                            "y",
                            Box::new(Builder::new(Location::new((1, 24), (1, 25)), Var("x"))),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 31)),
                                Prod(
                                    "z",
                                    Box::new(Builder::new(Location::new((1, 24), (1, 25)), Var("x"))),
                                    Box::new(Builder::new(Location::new((1, 30), (1, 31)), Var("z")))
                                )
                            )),
                        )
                    ))
                )
            )))
        );
    }

    #[test]
    fn parenthesis_in_abs() {
        assert_eq!(
            line("check fun (((w x : Prop))), y z : Prop => x"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 44)),
                Abs(
                    "w",
                    Box::new(Builder::new(Location::new((1, 20), (1, 24)), Prop)),
                    Box::new(Builder::new(
                        Location::new((1, 7), (1, 44)),
                        Abs(
                            "x",
                            Box::new(Builder::new(Location::new((1, 20), (1, 24)), Prop)),
                            Box::new(Builder::new(
                                Location::new((1, 7), (1, 44)),
                                Abs(
                                    "y",
                                    Box::new(Builder::new(Location::new((1, 35), (1, 39)), Prop)),
                                    Box::new(Builder::new(
                                        Location::new((1, 7), (1, 44)),
                                        Abs(
                                            "z",
                                            Box::new(Builder::new(Location::new((1, 35), (1, 39)), Prop)),
                                            Box::new(Builder::new(Location::new((1, 43), (1, 44)), Var("x"))),
                                        ),
                                    )),
                                ),
                            )),
                        ),
                    )),
                ),
            )))
        );
    }

    #[test]
    fn parenthesis_in_prod() {
        assert_eq!(
            line("check (((Type))) -> (((Type 1 -> Type 2)))"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 43)),
                Prod(
                    "_",
                    Box::new(Builder::new(Location::new((1, 10), (1, 14)), Type(box level::Builder::Const(0)))),
                    Box::new(Builder::new(
                        Location::new((1, 24), (1, 40)),
                        Prod(
                            "_",
                            Box::new(Builder::new(Location::new((1, 24), (1, 30)), Type(box level::Builder::Const(1)))),
                            Box::new(Builder::new(Location::new((1, 34), (1, 40)), Type(box level::Builder::Const(2))))
                        )
                    )),
                )
            )))
        );
    }

    #[test]
    fn parenthesis_in_dprod() {
        assert_eq!(
            line("check (((x:Type))) -> ((((y:Type 1) -> x)))"),
            Ok(GetType(Builder::new(
                Location::new((1, 7), (1, 44)),
                Prod(
                    "x",
                    Box::new(Builder::new(Location::new((1, 12), (1, 16)), Type(box level::Builder::Const(0)))),
                    Box::new(Builder::new(
                        Location::new((1, 26), (1, 41)),
                        Prod(
                            "y",
                            Box::new(Builder::new(Location::new((1, 29), (1, 35)), Type(box level::Builder::Const(1)))),
                            Box::new(Builder::new(Location::new((1, 40), (1, 41)), Var("x")))
                        )
                    ),)
                )
            )))
        );
    }

    #[test]
    fn parenthesis_in_app() {
        assert_eq!(
            line("check ((((((A))) (((B C))))))"),
            Ok(GetType(Builder::new(
                Location::new((1, 10), (1, 27)),
                App(
                    Box::new(Builder::new(Location::new((1, 13), (1, 14)), Var("A"))),
                    Box::new(Builder::new(
                        Location::new((1, 21), (1, 24)),
                        App(
                            Box::new(Builder::new(Location::new((1, 21), (1, 22)), Var("B"))),
                            Box::new(Builder::new(Location::new((1, 23), (1, 24)), Var("C")))
                        )
                    ))
                )
            )))
        );
    }

    #[test]
    fn successful_parsers() {
        let input = r#"
            def x := Prop -> Prop

            // this is a comment
            check fun x:Prop => x
        "#;

        // Since the location will differ, we just check that the kind is correct by displaying output
        assert_eq!(format!("{}", file(input).unwrap()[0]), format!("{}", line("def x := Prop -> Prop").unwrap()));
        assert_eq!(format!("{}", file(input).unwrap()[1]), format!("{}", line("check fun x:Prop => x").unwrap()));
    }

    #[test]
    fn successful_convert_error() {
        assert_eq!(
            line("chehk 2x"),
            Err(Error {
                kind: Kind::CannotParse(COMMAND_ERR.to_owned()),
                loc: Location::new((1, 1), (1, 5)),
            })
        );
        assert_eq!(
            line("check 2x"),
            Err(Error {
                kind: Kind::CannotParse(TERM_ERR.to_owned()),
                loc: Location::new((1, 7), (1, 8)),
            })
        );
        assert_eq!(
            line("check x:"),
            Err(Error {
                kind: Kind::CannotParse(TERM_ERR.to_owned()),
                loc: Location::new((1, 9), (1, 9)),
            })
        );
    }

    #[test]
    fn failed_parsers() {
        assert_eq!(
            file(
                "def x : Type := Prop -> Prop
                 // this is a comment
                        check .x"
            ),
            Err(Error {
                kind: Kind::CannotParse(TERM_ERR.to_owned()),
                loc: Location::new((3, 31), (3, 32)),
            })
        );
    }
}
