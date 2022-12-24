use kernel::location::Location;
use kernel::memory::declaration::builder as declaration;
use kernel::memory::level::builder as level;
use kernel::memory::term::builder as term;
use pest::error::LineColLocation;
use pest::iterators::Pair;
use pest::{Parser, Span};

use crate::command::Command;
use crate::error;

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

/// convert pest locations to kernel locations
fn convert_span(span: Span) -> Location {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();
    ((x1, y1), (x2, y2)).into()
}

/// build universe level from errorless pest's output
fn parse_level(pair: Pair<Rule>) -> level::Builder {
    use level::Builder::*;

    // location to be used in a future version
    let _loc = convert_span(pair.as_span());

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
    use term::Builder::*;

    // location to be used in a future version
    let _loc = convert_span(pair.as_span());

    match pair.as_rule() {
        Rule::Prop => Prop,

        Rule::Var => Var(pair.into_inner().as_str()),

        Rule::VarDecl => {
            let mut iter = pair.into_inner();
            let name = iter.next().unwrap().as_str();
            let levels = iter.next().unwrap().into_inner().map(parse_level).collect();
            Decl(box declaration::InstantiatedBuilder::Var(name, levels))
        },

        Rule::Type => {
            if let Some(next) = pair.into_inner().next_back() {
                Type(box parse_level(next))
            } else {
                Type(box level::Builder::Const(0))
            }
        },

        Rule::Sort => {
            if let Some(next) = pair.into_inner().next_back() {
                Sort(box parse_level(next))
            } else {
                Sort(box level::Builder::Const(0))
            }
        },

        Rule::App => {
            let mut iter = pair.into_inner().map(parse_term);
            let t = iter.next().unwrap();
            iter.fold(t, |acc, x| App(box acc, box x))
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
            .fold(body, |acc, (var, type_)| Abs(var, type_, box acc))
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
            .fold(body, |acc, (var, type_)| Prod(var, type_, box acc))
        },

        Rule::Prod => {
            let mut iter = pair.into_inner();
            let ret = parse_term(iter.next_back().unwrap());
            iter.map(parse_term).rev().fold(ret, |acc, argtype| Prod("_", box argtype, box acc))
        },

        term => unreachable!("unexpected term: {term:?}"),
    }
}

/// build commands from errorless pest's output
fn parse_expr(pair: Pair<Rule>) -> Command {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());

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
            let s = iter.next().unwrap().as_str();
            let term = parse_term(iter.next().unwrap());
            Command::Define(s, None, term)
        },

        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str();
            let ty = parse_term(iter.next().unwrap());
            let term = parse_term(iter.next().unwrap());

            Command::Define(s, Some(ty), term)
        },

        Rule::Declaration => {
            let mut iter = pair.into_inner();
            let mut string_decl = iter.next().unwrap().into_inner();
            let s = string_decl.next().unwrap().as_str();
            let vars: Vec<&str> = string_decl.next().unwrap().into_inner().map(|name| name.as_str()).collect();
            let body = iter.next().map(parse_term).unwrap();

            Command::Declaration(s, None, declaration::Builder::Decl(box body, vars))
        },

        Rule::DeclarationCheckType => {
            let mut iter = pair.into_inner();
            let mut string_decl = iter.next().unwrap().into_inner();
            let s = string_decl.next().unwrap().as_str();
            let vars: Vec<&str> = string_decl.next().unwrap().into_inner().map(|name| name.as_str()).collect();

            let ty = parse_term(iter.next().unwrap());
            let decl = iter.next().map(parse_term).unwrap();

            let ty = declaration::Builder::Decl(box ty, vars.clone());
            let decl = declaration::Builder::Decl(box decl, vars);
            Command::Declaration(s, Some(ty), decl)
        },

        Rule::Eval => {
            let term = parse_term(pair.into_inner().next().unwrap());
            Command::Eval(term)
        },

        Rule::ImportFile => {
            let files = pair.into_inner().map(|pair| pair.as_str()).collect();
            Command::Import(files)
        },

        Rule::Search => {
            let s = pair.into_inner().next().unwrap().as_str();
            Command::Search(s)
        },

        command => unreachable!("Unexpected command: {:?}", command),
    }
}

/// convert pest error to kernel error
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

            Location::new((x, left).into(), (x, right).into())
        },

        LineColLocation::Span(start, end) => Location::new(start.into(), end.into()),
    };

    // extracting the message from the pest output
    let message = err.to_string();
    let mut chars = message.lines().next_back().unwrap().chars();
    for _ in 0..4 {
        chars.next();
    }
    error::Error {
        kind: error::ErrorKind::CannotParse(chars.as_str().to_string()),
        location: loc,
    }
}

/// Parse a text input and try to convert it into a command.
///
/// if unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_line(line: &str) -> error::Result<Command> {
    CommandParser::parse(Rule::command, line).map_err(convert_error).map(|mut pairs| parse_expr(pairs.next().unwrap()))
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// if unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_file(file: &str) -> error::Result<Vec<Command>> {
    CommandParser::parse(Rule::file, file).map_err(convert_error).map(|pairs| pairs.into_iter().map(parse_expr).collect())
}

#[cfg(test)]
mod tests {

    use error::{Error, ErrorKind};
    use kernel::memory::term::builder as term;
    use term::Builder::*;

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
            parse_line("check fun x : Prop -> Type"),
            Err(Error {
                kind: ErrorKind::CannotParse(UNIVERSE_ERR.to_string()),
                location: Location::new((1, 27).into(), (1, 27).into()),
            })
        );
    }

    #[test]
    fn successful_define_with_type_annotation() {
        assert_eq!(parse_line("def x : Type := Prop"), Ok(Define("x", Some(Type(box level::Builder::Const(0))), Prop)));
    }

    #[test]
    fn successful_declare_with_type_annotation() {
        assert_eq!(
            parse_line("def x.{u} : Type u := foo.{u}"),
            Ok(Declaration(
                "x",
                Some(declaration::Builder::Decl(box Type(box level::Builder::Var("u")), ["u"].to_vec())),
                declaration::Builder::Decl(
                    box kernel::memory::term::builder::Builder::Decl(box declaration::InstantiatedBuilder::Var(
                        "foo",
                        [level::Builder::Var("u")].to_vec()
                    )),
                    ["u"].to_vec()
                )
            ))
        );
    }

    #[test]
    fn successful_import() {
        assert_eq!(parse_line("import file1 dir/file2"), Ok(Import(["file1", "dir/file2"].to_vec())));
        assert_eq!(parse_line("import "), Ok(Import(Vec::new())));
    }

    #[test]
    fn successful_search() {
        assert_eq!(parse_line("search variable1"), Ok(Search("variable1")));
    }

    #[test]
    fn successful_eval() {
        assert_eq!(parse_line("eval Prop"), Ok(Eval(Prop)));
    }

    #[test]
    fn successful_define() {
        assert_eq!(parse_line("def x := Prop"), Ok(Define("x", None, Prop)));
    }

    #[test]
    fn successful_declare() {
        assert_eq!(parse_line("def x.{} := Prop"), Ok(Declaration("x", None, declaration::Builder::Decl(box Prop, Vec::new()))));
    }

    #[test]
    fn successful_checktype() {
        assert_eq!(parse_line("check Prop : Type"), Ok(CheckType(Prop, Type(box level::Builder::Const(0)))));
    }

    #[test]
    fn successful_gettype_prop() {
        assert_eq!(parse_line("check Prop"), Ok(GetType(Prop)));
    }

    #[test]
    fn successful_gettype_sort() {
        assert_eq!(parse_line("check Sort"), Ok(GetType(Sort(box level::Builder::Const(0)))));
    }

    #[test]
    fn successful_var() {
        assert_eq!(parse_line("check fun A: Prop => A"), Ok(GetType(Abs("A", Box::new(Prop), Box::new(Var("A"))))));
    }

    #[test]
    fn successful_type() {
        assert_eq!(parse_line("check Type"), Ok(GetType(Type(box level::Builder::Const(0)))));
        assert_eq!(parse_line("check Type 0"), Ok(GetType(Type(box level::Builder::Const(0)))));
        assert_eq!(parse_line("check Type 1"), Ok(GetType(Type(box level::Builder::Const(1)))));
    }

    #[test]
    fn successful_sort() {
        assert_eq!(parse_line("check Sort"), Ok(GetType(Sort(box level::Builder::Const(0)))));
        assert_eq!(parse_line("check Sort 0"), Ok(GetType(Sort(box level::Builder::Const(0)))));
        assert_eq!(parse_line("check Sort 1"), Ok(GetType(Sort(box level::Builder::Const(1)))));
        assert_eq!(parse_line("check Sort (0 + 1)"), Ok(GetType(Sort(box level::Builder::Plus(box level::Builder::Const(0), 1)))));
        assert_eq!(
            parse_line("check Sort max 0 0"),
            Ok(GetType(Sort(box level::Builder::Max(box level::Builder::Const(0), box level::Builder::Const(0)))))
        );
        assert_eq!(
            parse_line("check Sort imax 0 0"),
            Ok(GetType(Sort(box level::Builder::IMax(box level::Builder::Const(0), box level::Builder::Const(0)))))
        );
    }

    #[test]
    fn successful_app() {
        let res_left = Ok(GetType(App(Box::new(App(Box::new(Var("A")), Box::new(Var("B")))), Box::new(Var("C")))));
        let res_right = Ok(GetType(App(Box::new(Var("A")), Box::new(App(Box::new(Var("B")), Box::new(Var("C")))))));
        assert_eq!(parse_line("check A B C"), res_left);
        assert_eq!(parse_line("check (A B) C"), res_left);
        assert_eq!(parse_line("check A (B C)"), res_right);
    }

    #[test]
    fn successful_prod() {
        let res_left = Ok(GetType(Prod("_", Box::new(Prod("_", Box::new(Var("A")), Box::new(Var("B")))), Box::new(Var("C")))));
        let res_right = Ok(GetType(Prod("_", Box::new(Var("A")), Box::new(Prod("_", Box::new(Var("B")), Box::new(Var("C")))))));
        assert_eq!(parse_line("check A -> B -> C"), res_right);
        assert_eq!(parse_line("check A -> (B -> C)"), res_right);
        assert_eq!(parse_line("check (A -> B) -> C"), res_left);
    }

    #[test]
    fn successful_dprod() {
        let res = Ok(GetType(Prod(
            "x",
            Box::new(Type(box level::Builder::Const(0))),
            Box::new(Prod("y", Box::new(Type(box level::Builder::Const(1))), Box::new(Var("x")))),
        )));
        assert_eq!(parse_line("check (x:Type) -> (y:Type 1) -> x"), res);
        assert_eq!(parse_line("check (x:Type) -> ((y:Type 1) -> x)"), res);
    }

    #[test]
    fn successful_abs() {
        let res = Abs(
            "w",
            Box::new(Prop),
            Box::new(Abs(
                "x",
                Box::new(Prop),
                Box::new(Abs("y", Box::new(Prop), Box::new(Abs("z", Box::new(Prop), Box::new(Var("x")))))),
            )),
        );
        assert_eq!(parse_line("check fun w x: Prop, y z: Prop => x"), Ok(GetType(res)));
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
        let res = Ok(GetType(Abs(
            "x",
            Box::new(Prop),
            Box::new(Abs("x", Box::new(Var("x")), Box::new(Abs("x", Box::new(Var("x")), Box::new(Var("x")))))),
        )));
        let res2 = Ok(GetType(Abs(
            "x",
            Box::new(Prop),
            Box::new(Abs("y", Box::new(Var("x")), Box::new(Abs("z", Box::new(Var("x")), Box::new(Var("z")))))),
        )));
        assert_eq!(parse_line("check fun x : Prop, x : x, x : x => x"), res);
        assert_eq!(parse_line("check fun x : Prop, x x : x => x"), res);
        assert_eq!(parse_line("check fun x : Prop, y z : x => z"), res2);
    }

    #[test]
    fn context_for_dprod_args() {
        let res = Ok(GetType(Prod(
            "x",
            Box::new(Prop),
            Box::new(Prod("x", Box::new(Var("x")), Box::new(Prod("x", Box::new(Var("x")), Box::new(Var("x")))))),
        )));
        let res2 = Ok(GetType(Prod(
            "x",
            Box::new(Prop),
            Box::new(Prod("y", Box::new(Var("x")), Box::new(Prod("z", Box::new(Var("x")), Box::new(Var("z")))))),
        )));
        assert_eq!(parse_line("check (x : Prop, x : x, x : x) -> x"), res);
        assert_eq!(parse_line("check (x : Prop, x x : x) -> x"), res);
        assert_eq!(parse_line("check (x : Prop, y z : x) -> z"), res2);
    }

    #[test]
    fn parenthesis_in_abs() {
        let res = Abs(
            "w",
            Box::new(Prop),
            Box::new(Abs(
                "x",
                Box::new(Prop),
                Box::new(Abs("y", Box::new(Prop), Box::new(Abs("z", Box::new(Prop), Box::new(Var("x")))))),
            )),
        );
        assert_eq!(parse_line("check fun (((w x : Prop))), y z : Prop => x"), Ok(GetType(res)));
    }

    #[test]
    fn parenthesis_in_prod() {
        let res = Prod(
            "_",
            Box::new(Type(box level::Builder::Const(0))),
            Box::new(Prod("_", Box::new(Type(box level::Builder::Const(1))), Box::new(Type(box level::Builder::Const(2))))),
        );
        assert_eq!(parse_line("check (((Type))) -> (((Type 1 -> Type 2)))"), Ok(GetType(res)));
    }

    #[test]
    fn parenthesis_in_dprod() {
        let res = Prod(
            "x",
            Box::new(Type(box level::Builder::Const(0))),
            Box::new(Prod("y", Box::new(Type(box level::Builder::Const(1))), Box::new(Var("x")))),
        );
        assert_eq!(parse_line("check (((x:Type))) -> ((((y:Type 1) -> x)))"), Ok(GetType(res)));
    }

    #[test]
    fn parenthesis_in_app() {
        let res = App(Box::new(Var("A")), Box::new(App(Box::new(Var("B")), Box::new(Var("C")))));
        assert_eq!(parse_line("check ((((((A))) (((B C))))))"), Ok(GetType(res)));
    }

    #[test]
    fn successful_parsers() {
        let file = r#"
            def x := Prop -> Prop

            // this is a comment
            check fun x:Prop => x
        "#;

        assert_eq!(parse_file(file).unwrap()[0], parse_line("def x := Prop -> Prop").unwrap());
        assert_eq!(parse_file(file).unwrap()[1], parse_line("check fun x:Prop => x").unwrap());
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
