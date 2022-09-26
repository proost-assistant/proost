#[macro_use]
extern crate pest_derive;

use core::Command;
use core::Term;
use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct CommandParser;

pub fn parse_term(file: &str) -> Result<Term, Error<Rule>> {
    let _term = CommandParser::parse(Rule::term, file)?;

    Ok(Term::Prop)
}

//TODO
pub fn parse_command(_file: &str) -> Result<Command, Error<Rule>> {
    Ok(Command::GetType(Term::Prop))
}

//TODO
pub fn parse_file(_file: &str) -> Result<Vec<Command>, Error<Rule>> {
    Ok(Vec::new())
}
