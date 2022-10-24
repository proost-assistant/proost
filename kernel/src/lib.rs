//! TODO: Make a documentation (#15)

#![feature(box_patterns)]
#![feature(box_syntax)]

mod command;
mod environment;
mod error;
mod term;
mod type_checker;

pub use command::Command;
pub use derive_more;
pub use environment::Environment;
pub use error::{KernelError, Pos};
pub use num_bigint;
pub use term::Term;
