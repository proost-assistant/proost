#![feature(box_syntax)]

#[macro_use]
extern crate pest_derive;

mod classic_term;
mod parser;

pub use self::parser::{parse_command, parse_file, parse_term};
