#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::error::Error;
use core::Term;
use core::Command;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct CommandParser;

pub fn parse_term(file: &str) -> Result<Term, Error<Rule>> {
    let term = CommandParser::parse(Rule::term, file)?;

    Ok(Term::Prop)
}

//TODO
pub fn parse_command(file: &str) -> Result<Command, Error<Rule>> {
    
    Ok(Command::GetType(Term::Prop))
}

//TODO
pub fn parse_file(file: &str) -> Result<Vec<Command>, Error<Rule>> {

    Ok(Vec::new())
}
