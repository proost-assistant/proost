use kernel::num_bigint::BigUint;
use kernel::{Command, Term};
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use pest::Parser;
use std::collections::VecDeque;

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

fn build_term_from_expr(
    pair: Pair<Rule>,
    known_vars: &mut VecDeque<String>,
    //defined_vars: issue #18 TODO use a hash map of known variables
) -> Result<Term, Box<Error<Rule>>> {
    match pair.as_rule() {
        Rule::Prop => Ok(Term::Prop),
        Rule::Type => Ok(Term::Type(
            pair.into_inner()
                .as_str()
                .parse::<BigUint>()
                .unwrap()
                .into(),
        )),

        Rule::Var => {
            //issue #18 TODO use a hash map of known variables
            let span = pair.as_span();
            let name = pair.into_inner().as_str().to_string();
            match known_vars.iter().position(|x| *x == name) {
                Some(i) => Ok(Term::Var((i + 1).into())),
                None => Err(box Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: String::from("free variable: ") + &name,
                    },
                    span,
                )),
            }
        }
        Rule::Prod => {
            let mut iter = pair
                .into_inner()
                .map(|x| build_term_from_expr(x, known_vars))
                .rev();
            let t = iter.next().unwrap()?;
            iter.try_fold(t, |acc, x| Ok(Term::Prod(box x?, box acc)))
        }
        Rule::App => {
            let mut iter = pair
                .into_inner()
                .map(|x| build_term_from_expr(x, known_vars))
                .rev();
            let t = iter.next().unwrap()?;
            iter.try_fold(t, |acc, x| Ok(Term::App(box x?, box acc)))
        }
        Rule::Abs => {
            let iter = pair.into_inner();
            let mut iter2 = iter.clone().rev();
            for pair in iter {
                if pair.as_rule() == Rule::arg_abs {
                    let name = pair.into_inner().next().unwrap().as_str().to_string();
                    known_vars.push_front(name);
                }
            }
            let t = build_term_from_expr(iter2.next().unwrap(), known_vars)?;
            iter2.try_fold(t, |acc, x| {
                known_vars.pop_front();
                let mut iter3 = x.into_inner();
                iter3.next();
                let t = build_term_from_expr(iter3.next().unwrap(), known_vars)?;
                Ok(Term::Abs(box t, box acc))
            })
        }
        Rule::dProd => {
            let iter = pair.into_inner();
            let mut iter2 = iter.clone().rev();
            for pair in iter {
                if pair.as_rule() == Rule::arg_dprod {
                    let name = pair.into_inner().next().unwrap().as_str().to_string();
                    known_vars.push_front(name);
                }
            }
            let t = build_term_from_expr(iter2.next().unwrap(), known_vars)?;
            iter2.try_fold(t, |acc, x| {
                known_vars.pop_front();
                let mut iter3 = x.into_inner();
                iter3.next();
                let t = build_term_from_expr(iter3.next().unwrap(), known_vars)?;
                Ok(Term::Prod(box t, box acc))
            })
        }
        term => panic!("Unexpected term: {:?}", term),
    }
}

fn build_command_from_expr(pair: Pair<Rule>) -> Result<Command, Box<Error<Rule>>> {
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            Ok(Command::GetType(t))
        }
        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            let t2 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            Ok(Command::CheckType(t1, t2))
        }
        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            Ok(Command::Define(s, t))
        }
        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            let t2 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new())?;
            Ok(Command::DefineCheckType(s, t1, t2))
        }
        Rule::error_dot => Err(box Error::new_from_pos(
            ErrorVariant::CustomError {
                message: String::from("missing dot"),
            },
            pair.as_span().end_pos(),
        )),
        command => panic!("Unexpected command: {:?}", command),
    }
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_commands(file: &str) -> Result<Vec<Command>, Box<Error<Rule>>> {
    CommandParser::parse(Rule::File, file)?
        .map(build_command_from_expr)
        .collect()
}
