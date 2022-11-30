//! A kernel for the calculus of constructions.
//!
//! Terms can be built with functions from the [`term`] module. Then, the kernel can be queried in
//! two different ways: the [`type_checker`] module provides low-level interactions, while the
//! [`command`] module provides higher-level ones.

#![feature(once_cell)]
#![feature(trait_alias)]

pub mod command;
pub mod error;
pub mod location;
pub mod term;
pub mod type_checker;
