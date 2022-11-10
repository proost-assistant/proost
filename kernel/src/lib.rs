//! TODO: Make a documentation (#15)

#![feature(once_cell)]
#![feature(trait_alias)]

mod command;
mod error;
mod location;
pub mod term;
mod type_checker;

pub use self::command::Command;
pub use self::location::{Location, Position};
pub use self::term::Term;
