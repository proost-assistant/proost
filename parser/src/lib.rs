#![doc(html_logo_url = "https://gitlab.crans.org/loutr/proost/-/raw/main/docs/media/logo.png")]

//! Fast parser for λ-terms and commands using pest.
//! Provides functions to parse files and single commands.

#![feature(box_syntax)]
#![feature(result_flattening)]

#[macro_use]
extern crate pest_derive;

pub mod command;
pub mod error;
mod parser;

pub use self::parser::*;
