#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::error::Error;
use core::Term;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct CommandParser;

enum Command {
    Define(String,Term),
    CheckType(Term,Term),
    GetType(Term,Term),
}

pub fn parse_term(file: &str) -> Result<Term, Error<Rule>> {
    let term = CommandParser::parse(Rule::term, file)?;
}

pub fn parse_command(file: &str) -> Result<Command, Error<Rule>> {}

pub fn parse_file(file: &str) -> Result<Vec<Command>, Error<Rule>> {}
