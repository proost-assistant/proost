//! Collection of safe functions to build Levels
//!
//! This module provides two main ways of building levels. The first one is via closures: users can
//! manipulate closures and create bigger ones which, when [built](Arena::build_level), provide the expected
//! level.
//!
//! The overall syntax remains transparent to the user. This means the user focuses on the
//! structure of the level they want to build, while the [closures](`LevelBuilderTrait`) internally build an appropriate
//! logic: converting regular universe names into their corresponding variable numbers
//!
//! The other way to proceed is built on top of the latter. Users can also manipulate a sort of
//! *high-level level* or *template*, described by the public enumeration [`Builder`], and at any
//! moment, [realise](LevelBuilder::realise) it.

use derive_more::Display;
use std::collections::HashMap;

use super::super::arena::Arena;
use super::Level;
use crate::error::{Error, ResultLevel};

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum LevelError<'arena> {
    #[display(fmt = "unknown universe variable {}", _0)]
    VarNotFound(&'arena str),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type)
pub type Environment<'build> = HashMap<&'build str, usize>;

/// The trait of closures which build levels with an adequate logic.
///
/// A call with a couple of arguments `(arena, env)` of a closure with this trait should
/// build a definite level in the [`Arena`] `arena`.
///
/// Please note that this is just a trait alias, meaning it enforces very little constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>, &Environment<'build>) -> ResultLevel<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the level built from the given closure, provided with a Level environment, which binds names to `usize`s
    #[inline]
    pub fn build_level<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultLevel<'arena> {
        f(self, &Environment::new())
    }
}

/// Returns a closure building a universe variable associated to the name `name`
#[inline]
pub const fn var(name: &str) -> impl BuilderTrait<'_> {
    move |arena, env| {
        env.get(name).map(|lvl| Level::var(*lvl, arena)).ok_or(Error {
            kind: LevelError::VarNotFound(arena.store_name(name)).into(),
        })
    }
}

/// Returns a closure building the 0 level.
#[inline]
pub const fn zero<'build>() -> impl BuilderTrait<'build> {
    |arena, _| Ok(Level::zero(arena))
}

#[inline]
pub const fn const_<'build>(n: usize) -> impl BuilderTrait<'build> {
    move |arena, _| Ok(Level::from(n, arena))
}

/// Returns a closure building the successor of a level built from the given closure `u1`
#[inline]
#[no_coverage]
pub const fn succ<'build, F1: BuilderTrait<'build>>(u1: F1) -> impl BuilderTrait<'build> {
    |arena, env| {
        let u1 = u1(arena, env)?;
        Ok(u1.succ(arena))
    }
}

/// Returns a closure building the max of two levels built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn max<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env| {
        let u1 = u1(arena, env)?;
        let u2 = u2(arena, env)?;
        Ok(u1.max(u2, arena))
    }
}

/// Returns a closure building the imax of two levels built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn imax<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env| {
        let u1 = u1(arena, env)?;
        let u2 = u2(arena, env)?;
        Ok(u1.imax(u2, arena))
    }
}

/// Template of levels.
///
/// A LevelBuilder describes a level in a naive but easy to build manner. It strongly resembles the
/// [level](`crate::memory::level::Level`) type, except that the `Var` constructor
/// include a name, as in the syntactic way of writing levels.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Builder<'builder> {
    #[display(fmt = "0")]
    Zero,

    Const(usize),

    #[display(fmt = "S({})", _0)]
    Succ(Box<Builder<'builder>>),

    #[display(fmt = "max({},{})", _0, _1)]
    Max(Box<Builder<'builder>>, Box<Builder<'builder>>),

    #[display(fmt = "imax({},{})", _0, _1)]
    IMax(Box<Builder<'builder>>, Box<Builder<'builder>>),

    Var(&'builder str),
}

impl<'build> Builder<'build> {
    /// Build a levels from a [`Builder`]. This internally uses functions described in the
    /// [builders](`crate::level::builder`) module.
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultLevel<'arena> {
        arena.build_level(self.partial_application())
    }

    pub fn partial_application(&self) -> impl BuilderTrait<'build> + '_ {
        |arena, env| self.realise_in_context(arena, env)
    }

    pub(in super::super) fn realise_in_context<'arena>(
        &self,
        arena: &mut Arena<'arena>,
        env: &Environment<'build>,
    ) -> ResultLevel<'arena> {
        use Builder::*;
        match *self {
            Zero => zero()(arena, env),
            Const(c) => const_(c)(arena, env),
            Succ(ref l) => succ(l.partial_application())(arena, env),
            Max(ref l, ref r) => max(l.partial_application(), r.partial_application())(arena, env),
            IMax(ref l, ref r) => imax(l.partial_application(), r.partial_application())(arena, env),
            Var(s) => var(s)(arena, env),
        }
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

    pub const fn zero<'arena>() -> impl BuilderTrait {
        |arena| Level::zero(arena)
    }

    pub const fn succ<'arena, F1: BuilderTrait>(u1: F1) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            u1.succ(arena)
        }
    }

    pub const fn max<'arena, F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.max(u2, arena)
        }
    }

    pub const fn imax<'arena, F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.imax(u2, arena)
        }
    }
}
