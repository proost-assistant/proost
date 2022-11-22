//! Fast parser for λ-terms and commands using pest.
//!
//! Provides the functions to parse both files and single commands.

#![feature(box_syntax)]
#![feature(result_flattening)]

#[macro_use]
extern crate pest_derive;

mod error;
mod parser;

pub use self::error::{Error, ErrorKind};
pub use self::parser::{parse_file, parse_line};
