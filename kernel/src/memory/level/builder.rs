//! A collection of safe functions to build [`Level`]s.
//!
//! This module provides a way of building levels via closures: users can manipulate closures and
//! create bigger ones which, when [built](Arena::build_level), provide the expected level.
//!
//! The overall syntax remains transparent to the user. This means the user focuses on the
//! structure of the term they want to build, while the [closures](`BuilderTrait`) internally build
//! an appropriate logic: converting regular universe variables names into their corresponding
//! variable numbers.

use std::collections::HashMap;

use derive_more::Display;

use super::Level;
use crate::error::{Error, Result, ResultLevel};
use crate::memory::arena::Arena;

/// The kind of errors that can occur when building a [`Level`].
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind<'arena> {
    /// Trying to build an universe too large
    // TODO (#94): Must be use only in this file. Currently, it is used in elaboration.
    #[display(fmt = "universe {_0} too large to be built")]
    UniverseTooLarge(usize),

    /// Unknown universe variable
    #[display(fmt = "unknown universe variable {_0}")]
    VarNotFound(&'arena str),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type).
pub type Environment<'build> = HashMap<&'build str, usize>;

/// The trait of closures which build levels with an adequate logic.
///
/// A call with a couple of arguments `(arena, env)` of a closure with this trait should
/// build a definite level in the [`Arena`] `arena`.
#[allow(clippy::module_name_repetitions)]
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>, &Environment<'build>) -> ResultLevel<'arena>;

/// The trait of closures which build a vector of levels.
pub trait VecBuilderTrait<'build> =
    for<'arena> FnOnce(&mut Arena<'arena>, &Environment<'build>) -> Result<'arena, Vec<Level<'arena>>>;

impl<'arena> Arena<'arena> {
    /// Returns the level built from the given closure, provided with a Level environment, which binds names to `usize`s
    ///
    /// # Errors
    /// If the level could not be built, yields an error indicating the reason.
    #[inline]
    pub fn build_level<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultLevel<'arena> {
        f(self, &Environment::new())
    }

    /// Returns the vector of levels built from the given closure.
    ///
    /// # Errors
    /// If one level could not be built, yields an error indicating the reason.
    #[inline]
    pub fn build_vec_level<'build, F: VecBuilderTrait<'build>>(&mut self, f: F) -> Result<'arena, Vec<Level<'arena>>> {
        f(self, &Environment::new())
    }
}

/// Returns a closure building a universe variable associated to `name`.
#[inline]
#[must_use]
pub const fn var(name: &str) -> impl BuilderTrait<'_> {
    move |arena, env| {
        env.get(name)
            .map(|lvl| Level::var(*lvl, arena))
            .ok_or_else(|| Error::new(ErrorKind::VarNotFound(arena.store_name(name)).into()))
    }
}

/// Returns a closure building the 0 level.
#[inline]
#[must_use]
pub const fn zero<'build>() -> impl BuilderTrait<'build> {
    |arena, _| Ok(Level::zero(arena))
}

/// Returns a closure building a constant level.
#[inline]
#[must_use]
pub const fn const_<'build>(n: usize) -> impl BuilderTrait<'build> {
    move |arena, _| Ok(Level::from(n, arena))
}

/// Returns a closure building the sum of `u` and a constant `n`.
#[inline]
#[no_coverage]
pub const fn plus<'build, F: BuilderTrait<'build>>(u: F, n: usize) -> impl BuilderTrait<'build> {
    move |arena, env| Ok(u(arena, env)?.add(n, arena))
}

/// Returns a closure building the successor of a level built from the given closure `u1`.
#[inline]
#[no_coverage]
pub const fn succ<'build, F1: BuilderTrait<'build>>(u1: F1) -> impl BuilderTrait<'build> {
    |arena, env| Ok(u1(arena, env)?.succ(arena))
}

/// Returns a closure building the max of two levels built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn max<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env| Ok(u1(arena, env)?.max(u2(arena, env)?, arena))
}

/// Returns a closure building the imax of two levels built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn imax<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env| Ok(u1(arena, env)?.imax(u2(arena, env)?, arena))
}

/// Returns the empty level vector.
#[inline]
#[no_coverage]
#[must_use]
pub const fn nil<'build>() -> impl VecBuilderTrait<'build> {
    |_, _| Ok(Vec::new())
}

/// Returns a closure appending the level built from `elt` to the vector built from `vec`.
#[inline]
#[no_coverage]
pub const fn append<'build, H: BuilderTrait<'build>, T: VecBuilderTrait<'build>>(vec: T, elt: H) -> impl VecBuilderTrait<'build> {
    |arena, env| {
        let mut v = vec(arena, env)?;
        v.push(elt(arena, env)?);
        Ok(v)
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub trait BuilderTrait = for<'arena> FnOnce(&mut Arena<'arena>) -> Level<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_level_raw<F: BuilderTrait>(&mut self, f: F) -> Level<'arena> {
            f(self)
        }
    }

    pub const fn var(id: usize) -> impl BuilderTrait {
        move |arena| Level::var(id, arena)
    }

    pub const fn zero() -> impl BuilderTrait {
        |arena| Level::zero(arena)
    }

    pub const fn succ<F1: BuilderTrait>(u1: F1) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            u1.succ(arena)
        }
    }

    pub const fn max<F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.max(u2, arena)
        }
    }

    pub const fn imax<F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.imax(u2, arena)
        }
    }
}
