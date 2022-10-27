//! Fast parser for λ-terms and commands using pest.
//!
//! Provides the functions to parse both files and single commands.

#![feature(box_syntax)]

#[macro_use]
extern crate pest_derive;

mod parser;

pub use self::parser::{parse_file, parse_line};
