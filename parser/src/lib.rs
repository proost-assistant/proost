#![feature(box_syntax)]

#[macro_use]
extern crate pest_derive;

mod parser;

pub use self::parser::parse_file;
