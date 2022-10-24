use kernel::num_bigint::BigUint;
use kernel::{Command, KernelError, Term};
use pest::iterators::Pair;
use pest::Parser;
use std::collections::VecDeque;

#[derive(Parser)]
#[grammar = "term.pest"]
struct CommandParser;

fn build_term_from_expr(pair: Pair<Rule>, known_vars: &mut VecDeque<String>) -> Term {
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
            let mut iter = pair.into_inner().rev();
            let pair = iter.next().unwrap();
            let mut names = Vec::new();
            let mut terms = Vec::new();
            for pair in iter {
                let mut iter = pair.into_inner().rev();
                let t = build_term_from_expr(iter.next().unwrap(), known_vars);
                for pair in iter {
                    names.push(pair.as_str().to_string());
                    terms.push(t.clone());
                }
            }
            names
                .into_iter()
                .rev()
                .for_each(|x| known_vars.push_front(x));
            let t = build_term_from_expr(pair, known_vars);
            terms.into_iter().fold(t, |acc, x| {
                known_vars.pop_front();
                Term::Abs(box x, box acc)
            })
        }
        Rule::dProd => {
            let mut iter = pair.into_inner().rev();
            let pair = iter.next().unwrap();
            let mut names = Vec::new();
            let mut terms = Vec::new();
            for pair in iter {
                let mut iter = pair.into_inner().rev();
                let t = build_term_from_expr(iter.next().unwrap(), known_vars);
                for pair in iter {
                    names.push(pair.as_str().to_string());
                    terms.push(t.clone());
                }
            }
            names
                .into_iter()
                .rev()
                .for_each(|x| known_vars.push_front(x));
            let t = build_term_from_expr(pair, known_vars);
            terms.into_iter().fold(t, |acc, x| {
                known_vars.pop_front();
                Term::Prod(box x, box acc)
            })
        }
        term => unreachable!("Unexpected term: {:?}", term),
    }
}

fn build_command_from_expr(pair: Pair<Rule>) -> Command {
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
            let t = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            Command::Define(s, t)
        }
        Rule::DefineCheckType => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            let t2 = build_term_from_expr(iter.next().unwrap(), &mut VecDeque::new());
            Command::DefineCheckType(s, t1, t2)
        }
        command => unreachable!("Unexpected command: {:?}", command),
    }
}

/// Parse a text input and try to convert it into a command.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_line(line: &str) -> Result<Command, KernelError> {
    match CommandParser::parse(Rule::command, line) {
        Ok(mut pairs) => Ok(build_command_from_expr(pairs.next().unwrap())),
        //TODO
        Err(err) => Err(KernelError::CannotParse(err.to_string())),
    }
}

/// Parse a text input and try to convert it into a vector of commands.
///
/// If unsuccessful, a box containing the first error that was encountered is returned.
pub fn parse_file(file: &str) -> Result<Vec<Command>, KernelError> {
    match CommandParser::parse(Rule::file, file) {
        Ok(pairs) => Ok(pairs.into_iter().map(build_command_from_expr).collect()),
        // TODO
        Err(err) => Err(KernelError::CannotParse(err.to_string())),
    }
}
