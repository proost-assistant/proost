//! Fast parser for λ-terms and commands using pest.
//!
//! Provides the functions to parse both files and single commands.

#![feature(box_syntax)]
#![feature(result_flattening)]

#[macro_use]
extern crate pest_derive;

pub mod error;
mod parser;

pub use parser::*;
