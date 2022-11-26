//! TODO: Make a documentation (#15)

#![feature(once_cell)]
#![feature(trait_alias)]

mod command;
mod error;
mod location;
mod term;
mod type_checker;
mod declaration;
mod universe;

pub use self::command::Command;
pub use self::location::{Location, Position};
pub use self::term::arena::{use_arena, Arena, Term};
pub use self::term::builders::extern_ as builders;
pub use declaration::Declaration;
pub use universe::UniverseLevel;
