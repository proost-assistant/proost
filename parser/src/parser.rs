use core::{ClassicTerm, Command, Term};
use pest::error::Error;
use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "classic_term.pest"]
struct CommandParser;

fn build_term_from_expr(pair: Pair<Rule>) -> ClassicTerm {
    match pair.as_rule() {
        Rule::Prop => ClassicTerm::Prop,
        Rule::Var => ClassicTerm::Var(pair.into_inner().as_str().to_string()),
        Rule::Type => ClassicTerm::Type(pair.into_inner().as_str().parse().unwrap()),
        Rule::App => {
            let mut iter = pair.into_inner();
            let t1 = build_term_from_expr(iter.next().unwrap());
            let t2 = build_term_from_expr(iter.next().unwrap());
            ClassicTerm::App(Box::new(t1), Box::new(t2))
        }
        Rule::Abs => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_term_from_expr(iter.next().unwrap());
            let t2 = build_term_from_expr(iter.next().unwrap());
            ClassicTerm::Abs(s, Box::new(t1), Box::new(t2))
        }
        Rule::Prod => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t1 = build_term_from_expr(iter.next().unwrap());
            let t2 = build_term_from_expr(iter.next().unwrap());
            ClassicTerm::Prod(s, Box::new(t1), Box::new(t2))
        }
        term => panic!("Unexpected term: {:?}", term),
    }
}

fn build_command_from_expr(pair: Pair<Rule>) -> Command {
    match pair.as_rule() {
        Rule::GetType => {
            let mut iter = pair.into_inner();
            let t = Term::from(build_term_from_expr(iter.next().unwrap()));
            Command::GetType(t)
        }
        Rule::CheckType => {
            let mut iter = pair.into_inner();
            let t1 = Term::from(build_term_from_expr(iter.next().unwrap()));
            let t2 = Term::from(build_term_from_expr(iter.next().unwrap()));
            Command::CheckType(t1, t2)
        }
        Rule::Define => {
            let mut iter = pair.into_inner();
            let s = iter.next().unwrap().as_str().to_string();
            let t = Term::from(build_term_from_expr(iter.next().unwrap()));
            Command::Define(s, t)
        }
        command => panic!("Unexpected command: {:?}", command),
    }
}

pub fn parse_term(file: &str) -> Result<ClassicTerm, Box<Error<Rule>>> {
    let pair = CommandParser::parse(Rule::Term, file)?.next().unwrap();
    Ok(build_term_from_expr(pair))
}

pub fn parse_command(file: &str) -> Result<Command, Box<Error<Rule>>> {
    let pair = CommandParser::parse(Rule::Command, file)?.next().unwrap();
    Ok(build_command_from_expr(pair))
}

pub fn parse_file(file: &str) -> Result<Vec<Command>, Box<Error<Rule>>> {
    let mut vec = Vec::new();
    for pair in CommandParser::parse(Rule::File, file)?
        .next()
        .unwrap()
        .into_inner()
    {
        match pair.as_rule() {
            Rule::EOI => (),
            _ => vec.push(build_command_from_expr(pair)),
        }
    }
    Ok(vec)
}
