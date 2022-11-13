use kernel::{Command, KernelError, Loc, Term, UniverseLevel};
use pest::error::{Error, LineColLocation};
use pest::iterators::Pair;
use pest::{Parser, Span};
use std::collections::{HashMap, VecDeque};

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

/// build universe level from errorless pest's output
fn build_universe_level_from_expr(
    pair: Pair<Rule>,
    univ_var_map: &HashMap<String, usize>,
) -> UniverseLevel {
    match pair.as_rule() {
        Rule::Num => {
            let n = pair.into_inner().as_str().parse().unwrap();
            let mut univ = UniverseLevel::Zero;
            for _ in 0..n {
                univ = UniverseLevel::Succ(box univ);
            }
            univ
        }
        Rule::Max => {
            let mut iter = pair.into_inner();
            let univ1 = build_universe_level_from_expr(iter.next().unwrap(), univ_var_map);
            let univ2 = build_universe_level_from_expr(iter.next().unwrap(), univ_var_map);
            UniverseLevel::Max(box univ1, box univ2)
        }
        Rule::Plus => {
            let mut iter = pair.into_inner();
            let mut univ = build_universe_level_from_expr(iter.next().unwrap(), univ_var_map);
            let n = iter.map(|x| x.as_str().parse::<u64>().unwrap()).sum();
            for _ in 0..n {
                univ = UniverseLevel::Succ(box univ);
            }
            univ
        }
        Rule::string => {
            let name = pair.as_str().to_string();
            match univ_var_map.get(&name) {
                Some(n) => UniverseLevel::Var(*n),
                None => panic!("Universe level {:?} is unbound", name),
            }
        }
        univ => unreachable!("Unexpected universe level: {:?}", univ),
    }
}

/// convert pest locations to kernel locations
fn convert_span(span: Span) -> Loc {
    let (x1, y1) = span.start_pos().line_col();
    let (x2, y2) = span.end_pos().line_col();
    Loc::new(x1, y1, x2, y2)
}

fn build_univ_map_from_expr(pair: Pair<Rule>) -> HashMap<String, usize> {
    let iter = pair.into_inner();
    let mut map = HashMap::new();
    for (i, pair) in iter.enumerate() {
        let str = pair.as_str();
        if map.insert(str.to_string(), i).is_some() {
            panic!("Duplicate universe variable {}", str);
        }
    }
    map
}

fn build_univ_bindings_from_expr(
    pair: Pair<Rule>,
    univ_var_map: &HashMap<String, usize>,
) -> Vec<UniverseLevel> {
    let iter = pair.into_inner();
    let mut vec = Vec::new();
    for pair in iter {
        vec.push(build_universe_level_from_expr(pair, univ_var_map));
    }
    vec
}

/// build terms from errorless pest's output
fn build_term_from_expr(
    pair: Pair<Rule>,
    known_vars: &mut VecDeque<String>,
    univ_var_map: &HashMap<String, usize>,
) -> Term {
    // location to be used in a future version
    let _loc = convert_span(pair.as_span());
    match pair.as_rule() {
        Rule::Prop => Term::Sort(0.into()),
        Rule::Type => match pair.into_inner().next() {
            Some(pair) => Term::Sort(build_universe_level_from_expr(pair, univ_var_map) + 1),
            None => Term::Sort(UniverseLevel::Zero + 1),
        },
        Rule::Sort => match pair.into_inner().next() {
            Some(pair) => Term::Sort(build_universe_level_from_expr(pair, univ_var_map)),
            None => Term::Sort(UniverseLevel::Zero),
        },
        Rule::Var => {
            let mut iter = pair.into_inner();
            let name = iter.next().unwrap().as_str().to_string();
            match known_vars.iter().position(|x| *x == name) {
                Some(i) => Term::Var((i + 1).into()),
                None => Term::Const(
                    name,
                    iter.next()
                        .map(|x| build_univ_bindings_from_expr(x, univ_var_map))
                        .unwrap_or_default(),
                ),
            }
        }
        Rule::App => {
            let mut iter = pair
                .into_inner()
                .map(|x| build_term_from_expr(x, known_vars, univ_var_map));
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
                    terms.push(build_term_from_expr(
                        old_pair.clone(),
                        known_vars,
                        univ_var_map,
                    ));
                    known_vars.push_front(pair.as_str().to_string());
                }
            }
            let t = build_term_from_expr(pair, known_vars, univ_var_map);
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
                    terms.push(build_term_from_expr(
                        old_pair.clone(),
                        known_vars,
                        univ_var_map,
                    ));
                    known_vars.push_front(pair.as_str().to_string());
                }
            }
            let t = build_term_from_expr(pair, known_vars, univ_var_map);
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
                let t = build_term_from_expr(pair, known_vars, univ_var_map);
                known_vars.push_front("_".to_string());
                terms.push(t);
            }
            let t = build_term_from_expr(pair, known_vars, univ_var_map);
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
            let t =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            Command::GetType(t)
        }
        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            let t2 =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            Command::CheckType(t1, t2)
        }
        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            //let _univ_params = iter.next();
            let next = iter.next();
            let term = {
                if matches!(
                    next.clone().map(|x| x.as_rule()),
                    None | Some(Rule::univ_decl)
                ) {
                    let univs = next.map(build_univ_map_from_expr).unwrap_or_default();
                    build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &univs)
                } else {
                    build_term_from_expr(next.unwrap(), &mut VecDeque::new(), &HashMap::new())
                }
            };
            Command::Define(s, None, term)
        }
        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            let term =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            Command::Define(s, Some(t), term)
        }
        Rule::Eval => {
            let mut iter = pair.into_inner();
            let t =
                build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new(), &HashMap::new());
            Command::Eval(t)
        }

        command => unreachable!("Unexpected command: {:?}", command),
    }
}

/// convert pest error to kernel error
fn convert_error(err: Error<Rule>) -> KernelError {
    // renaming error messages
    let err = err.renamed_rules(|rule| match *rule {
        Rule::string | Rule::Var => "variable".to_owned(),
        Rule::number => "number".to_owned(),
        Rule::Define => "def var := term".to_owned(),
        Rule::CheckType => "check term : term".to_owned(),
        Rule::GetType => "check term".to_owned(),
        Rule::Eval => "eval term".to_owned(),
        Rule::DefineCheckType => "def var : term := term".to_owned(),
        Rule::Abs => "abstraction".to_owned(),
        Rule::dProd => "dependent product".to_owned(),
        Rule::Prod => "product".to_owned(),
        Rule::App => "application".to_owned(),
        Rule::Prop => "Prop".to_owned(),
        Rule::Sort => "Sort".to_owned(),
        Rule::Type => "Type".to_owned(),
        Rule::Max => "max".to_owned(),
        Rule::Plus => "plus".to_owned(),
        Rule::arg_univ => "universe argument".to_owned(),
        Rule::univ_decl => "universe declaration".to_owned(),
        rule => {
            println!("{:?}", rule);
            unreachable!("low level rules cannot appear in error messages")
        }
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
            Loc::new(x, left, x, right)
        }
        // unreachable in the current pest version but may change in the future
        // if this is the case, this line should be used in place of an unreachable macro:
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

#[cfg(test)]
mod tests {
    use super::Command::*;
    use super::KernelError::*;
    use super::Term::*;
    use super::*;

    /// Error messages
    const COMMAND_ERR: &str =
        "expected eval term, def var := term, def var : term := term, check term : term, or check term";
    const TERM_ERR: &str =
        "expected variable, abstraction, dependent product, application, product, Prop, Type, or Sort";
    const SIMPLE_TERM_ERR: &str =
        "expected variable, abstraction, Prop, Type, Sort, or universe argument";
    const UNIVERSE_ERR: &str =
        "expected number, variable, abstraction, Prop, Type, Sort, plus, or max";

    #[test]
    fn failure_universe_level() {
        assert_eq!(
            parse_line("check fun x : Prop -> Type"),
            Err(CannotParse(
                Loc::new(1, 27, 1, 27),
                UNIVERSE_ERR.to_string()
            ))
        )
    }

    #[test]
    fn successful_definechecktype() {
        assert_eq!(
            parse_line("def x : Type := Prop"),
            Ok(Define(
                "x".to_string(),
                Some(Sort(1.into())),
                Sort(0.into())
            ))
        )
    }

    #[test]
    fn successful_define() {
        assert_eq!(
            parse_line("def x := Prop"),
            Ok(Define("x".to_string(), None, Sort(0.into())))
        )
    }

    #[test]
    fn successful_checktype() {
        assert_eq!(
            parse_line("check Prop : Type"),
            Ok(CheckType(Sort(0.into()), Sort(1.into())))
        )
    }

    #[test]
    fn successful_gettype_prop() {
        assert_eq!(parse_line("check Prop"), Ok(GetType(Sort(0.into()))))
    }

    #[test]
    fn successful_var() {
        assert_eq!(
            parse_line("check A"),
            Ok(GetType(Const("A".to_string(), Vec::new())))
        );
        assert_eq!(
            parse_line("check fun A:Prop => A"),
            Ok(GetType(Abs(box Sort(0.into()), box Var(1.into()))))
        )
    }

    #[test]
    fn successful_type() {
        assert_eq!(parse_line("check Type"), Ok(GetType(Sort(1.into()))));
        assert_eq!(parse_line("check Type 0"), Ok(GetType(Sort(1.into()))));
        assert_eq!(parse_line("check Type 1"), Ok(GetType(Sort(2.into()))))
    }

    #[test]
    fn successful_app() {
        assert_eq!(
            parse_line("check A B C"),
            Ok(GetType(App(
                box App(
                    box Const("A".to_string(), Vec::new()),
                    box Const("B".to_string(), Vec::new())
                ),
                box Const("C".to_string(), Vec::new())
            )))
        );
        assert_eq!(
            parse_line("check (A B) C"),
            Ok(GetType(App(
                box App(
                    box Const("A".to_string(), Vec::new()),
                    box Const("B".to_string(), Vec::new())
                ),
                box Const("C".to_string(), Vec::new())
            )))
        );
        assert_eq!(
            parse_line("check A (B C)"),
            Ok(GetType(App(
                box Const("A".to_string(), Vec::new()),
                box App(
                    box Const("B".to_string(), Vec::new()),
                    box Const("C".to_string(), Vec::new())
                )
            )))
        )
    }

    #[test]
    fn successful_prod() {
        assert_eq!(
            parse_line("check A -> B -> C"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(
                    box Const("B".to_string(), Vec::new()),
                    box Const("C".to_string(), Vec::new())
                )
            )))
        );
        assert_eq!(
            parse_line("check A -> (B -> C)"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(
                    box Const("B".to_string(), Vec::new()),
                    box Const("C".to_string(), Vec::new())
                )
            )))
        );
        assert_eq!(
            parse_line("check (A -> B) -> C"),
            Ok(GetType(Prod(
                box Prod(
                    box Const("A".to_string(), Vec::new()),
                    box Const("B".to_string(), Vec::new())
                ),
                box Const("C".to_string(), Vec::new())
            )))
        )
    }

    #[test]
    fn successful_dprod() {
        assert_eq!(
            parse_line("check (x:A) -> (y:B) -> x"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(box Const("B".to_string(), Vec::new()), box Var(2.into()))
            )))
        );
        assert_eq!(
            parse_line("check (x:A) -> ((y:B) -> x)"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(box Const("B".to_string(), Vec::new()), box Var(2.into()))
            )))
        )
    }

    #[test]
    fn successful_abs() {
        assert_eq!(
            parse_line("check fun w x : Prop, y z : Prop => x"),
            Ok(GetType(Abs(
                box Sort(0.into()),
                box Abs(
                    box Sort(0.into()),
                    box Abs(
                        box Sort(0.into()),
                        box Abs(box Sort(0.into()), box Var(3.into()))
                    )
                )
            )))
        )
    }

    #[test]
    fn failed_dprod() {
        assert_eq!(
            parse_line("check (x:A)"),
            Err(CannotParse(
                Loc::new(1, 7, 1, 11),
                SIMPLE_TERM_ERR.to_string()
            ))
        );
        assert_eq!(
            parse_line("check (x:A) -> (y:B)"),
            Err(CannotParse(
                Loc::new(1, 16, 1, 20),
                SIMPLE_TERM_ERR.to_string()
            ))
        )
    }

    #[test]
    fn context_for_abs_args() {
        assert_eq!(
            parse_line("check fun x : Prop, x : x, x : x => x"),
            Ok(GetType(Abs(
                box Sort(0.into()),
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check fun x : Prop, x x : x => x"),
            Ok(GetType(Abs(
                box Sort(0.into()),
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check fun x : Prop, y z : x => z"),
            Ok(GetType(Abs(
                box Sort(0.into()),
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
                box Sort(0.into()),
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check (x : Prop, x x : x) -> x"),
            Ok(GetType(Prod(
                box Sort(0.into()),
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(1.into()), box Var(1.into()))
                )
            )))
        );
        assert_eq!(
            parse_line("check (x : Prop, y z : x) -> z"),
            Ok(GetType(Prod(
                box Sort(0.into()),
                box Prod(
                    box Var(1.into()),
                    box Prod(box Var(2.into()), box Var(1.into()))
                )
            )))
        )
    }

    #[test]
    fn parenthesis_in_abs() {
        assert_eq!(
            parse_line("check fun (((w x : Prop))), y z : Prop => x"),
            Ok(GetType(Abs(
                box Sort(0.into()),
                box Abs(
                    box Sort(0.into()),
                    box Abs(
                        box Sort(0.into()),
                        box Abs(box Sort(0.into()), box Var(3.into()))
                    )
                )
            )))
        )
    }

    #[test]
    fn parenthesis_in_prod() {
        assert_eq!(
            parse_line("check (((A))) -> (((B -> C)))"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(
                    box Const("B".to_string(), Vec::new()),
                    box Const("C".to_string(), Vec::new())
                )
            )))
        )
    }

    #[test]
    fn parenthesis_in_dprod() {
        assert_eq!(
            parse_line("check (((x:A))) -> ((((y:B) -> x)))"),
            Ok(GetType(Prod(
                box Const("A".to_string(), Vec::new()),
                box Prod(box Const("B".to_string(), Vec::new()), box Var(2.into()))
            )))
        )
    }

    #[test]
    fn parenthesis_in_app() {
        assert_eq!(
            parse_line("check ((((((A))) (((B C))))))"),
            Ok(GetType(App(
                box Const("A".to_string(), Vec::new()),
                box App(
                    box Const("B".to_string(), Vec::new()),
                    box Const("C".to_string(), Vec::new())
                )
            )))
        )
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
        )
    }

    #[test]
    fn successful_convert_error() {
        assert_eq!(
            parse_line("chehk 2x"),
            Err(CannotParse(Loc::new(1, 1, 1, 5), COMMAND_ERR.to_string()))
        );
        assert_eq!(
            parse_line("check 2x"),
            Err(CannotParse(Loc::new(1, 7, 1, 8), TERM_ERR.to_string()))
        );
        assert_eq!(
            parse_line("check x:"),
            Err(CannotParse(Loc::new(1, 9, 1, 9), TERM_ERR.to_string()))
        )
    }

    #[test]
    fn failed_parsers() {
        assert_eq!(
            parse_file(
                "def x : Type := Prop-> Prop
                 // this is a comment
                        check .x"
            ),
            Err(CannotParse(Loc::new(3, 31, 3, 32), TERM_ERR.to_string()))
        )
    }
}
