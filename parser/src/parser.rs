use core::{ClassicTerm, Command, Term};
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pair;
use pest::{Parser, Position};

#[derive(Parser)]
#[grammar = "classic_term.pest"]
struct CommandParser;

fn build_classic_term_from_expr(pair: Pair<Rule>) -> ClassicTerm {
    match pair.as_rule() {
        Rule::Prop => ClassicTerm::Prop,
        Rule::Var => ClassicTerm::Var(pair.into_inner().as_str().to_string()),
        Rule::Type => ClassicTerm::Type(pair.into_inner().as_str().parse().unwrap()),
        Rule::App => {
            let mut iter = pair.into_inner();
            let t1 = build_classic_term_from_expr(iter.next().unwrap());
            let t2 = build_classic_term_from_expr(iter.next().unwrap());
            ClassicTerm::App(box t1, box t2)
        }
        Rule::Abs => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_classic_term_from_expr(iter.next().unwrap());
            let t2 = build_classic_term_from_expr(iter.next().unwrap());
            ClassicTerm::Abs(s, box t1, box t2)
        }
        Rule::Prod => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_classic_term_from_expr(iter.next().unwrap());
            let t2 = build_classic_term_from_expr(iter.next().unwrap());
            ClassicTerm::Prod(s, box t1, box t2)
        }
        term => panic!("Unexpected term: {:?}", term),
    }
}

fn build_term_from_expr(pair: Pair<Rule>) -> Result<Term, String> {
    Term::try_from(build_classic_term_from_expr(pair))
}

fn build_command_from_expr(pair: Pair<Rule>) -> Result<Command, String> {
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = build_term_from_expr(iter.next().unwrap())?;
            Ok(Command::GetType(t))
        }
        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = build_term_from_expr(iter.next().unwrap())?;
            let t2 = build_term_from_expr(iter.next().unwrap())?;
            Ok(Command::CheckType(t1, t2))
        }
        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t = build_term_from_expr(iter.next().unwrap())?;
            Ok(Command::Define(s, t))
        }
        command => panic!("Unexpected command: {:?}", command),
    }
}

pub fn parse_term(file: &str) -> Result<Term, Box<Error<Rule>>> {
    let pair = CommandParser::parse(Rule::Term, file)?.next().unwrap();
    match build_term_from_expr(pair) {
        Ok(t) => Ok(t),
        Err(s) => Err(box Error::new_from_pos(
            ErrorVariant::CustomError {
                message: String::from("Free variable: ") + &s,
            },
            Position::from_start(""),
        )),
    }
}

pub fn parse_command(file: &str) -> Result<Command, Box<Error<Rule>>> {
    let pair = CommandParser::parse(Rule::Command, file)?.next().unwrap();
    match build_command_from_expr(pair) {
        Ok(t) => Ok(t),
        Err(s) => Err(box Error::new_from_pos(
            ErrorVariant::CustomError {
                message: String::from("Free variable: ") + &s,
            },
            Position::from_start(""),
        )),
    }
}

// TODO
// unsatisfactory behavior
pub fn parse_file(file: &str) -> Result<Vec<Command>, Box<Error<Rule>>> {
    let mut vec = Vec::new();
    for pair in CommandParser::parse(Rule::File, file)?
        .next()
        .unwrap()
        .into_inner()
    {
        match pair.as_rule() {
            Rule::EOI => (),
            _ => {
                if let Ok(t) = build_command_from_expr(pair) {
                    vec.push(t)
                }
            }
        }
    }
    Ok(vec)
}
