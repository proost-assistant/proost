//! A kernel for the calculus of constructions.
//!
//! Terms can be built with functions from the [`term`] module. This module also provides essential
//! manipulation functions from lambda-calculus, while the [`type_checker`] module provides typed
//! interactions.

#![feature(once_cell)]
#![feature(trait_alias)]

pub mod error;
pub mod location;
pub mod term;
pub mod type_checker;
