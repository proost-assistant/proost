//! TODO: Make a documentation (#15)

#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![feature(trait_alias)]

mod command;
mod environment;
mod error;
mod location;
mod term;
mod type_checker;

pub use self::command::Command;
pub use self::environment::Environment;
pub use self::location::{Location, Position};
pub use self::term::Term;
