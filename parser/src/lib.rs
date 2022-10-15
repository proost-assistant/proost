//! Fast parser for λ-terms and commands using pest.
//!
//! Provides a single function `parse_file` to parse both files and commands.

#![feature(box_syntax)]

#[macro_use]
extern crate pest_derive;

mod parser;

pub use self::parser::parse_file;
