//! TODO: Make a documentation (#15)

#![feature(box_patterns)]
#![feature(box_syntax)]

mod command;
mod environment;
mod error;
mod location;
mod term;
mod type_checker;

pub use derive_more;
pub use num_bigint;

pub use self::command::Command;
pub use self::environment::Environment;
pub use self::location::{Location, Position};
pub use self::term::Term;
