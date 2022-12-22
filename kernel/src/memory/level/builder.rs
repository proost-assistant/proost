//! A collection of safe functions to build [`Level`]s.
//!
//! This module provides two main ways of building terms. The first one is via closures: users can
//! manipulate closures and create bigger ones which, when [built](Arena::build_level), provide the expected
//! level.
//!
//! The overall syntax remains transparent to the user. This means the user focuses on the
//! structure of the term they want to build, while the [closures](`BuilderTrait`) internally build an appropriate
//! logic: converting regular universe variables names into their corresponding variable numbers.
//!
//! The other way to proceed is built on top of the latter. Users can also manipulate a sort of
//! *high-level level* or *template*, described by the public enumeration [`Builder`], and at any
//! moment, [realise](Builder::realise) it.

use std::collections::HashMap;

use derive_more::Display;

use super::Level;
use crate::error::{Error, ResultLevel};
use crate::memory::arena::Arena;

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
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>, &Environment<'build>) -> ResultLevel<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the level built from the given closure, provided with a Level environment, which binds names to `usize`s
    #[inline]
    pub fn build_level<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultLevel<'arena> {
        f(self, &Environment::new())
    }
}

/// Returns a closure building a universe variable associated to `name`
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

/// Returns a closure building a constant level.
#[inline]
pub const fn const_<'build>(n: usize) -> impl BuilderTrait<'build> {
    move |arena, _| Ok(Level::from(n, arena))
}

/// Returns a closure building the sum of `u` and a constant `n`.
#[inline]
pub const fn plus<'build, F: BuilderTrait<'build>>(u: F, n: usize) -> impl BuilderTrait<'build> {
    move |arena, env| Ok(u(arena, env)?.add(n, arena))
}

/// Returns a closure building the successor of a level built from the given closure `u1`
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

/// Template of levels.
///
/// A [`Builder`] describes a level in a naive but easy to build manner. It strongly resembles the
/// [`Level`] type, except that the `Var` constructor include a name, as in the syntactic way of
/// writing levels. Because its purpose is to provide an easy way to build terms, even through the
/// API, it offers different ways to build some terms, for convenience.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Builder<'builder> {
    #[display(fmt = "0")]
    Zero,

    Const(usize),

    #[display(fmt = "({_0}) + {_1}")]
    Plus(Box<Builder<'builder>>, usize),

    #[display(fmt = "S({_0})")]
    Succ(Box<Builder<'builder>>),

    #[display(fmt = "max({_0}, {_1})")]
    Max(Box<Builder<'builder>>, Box<Builder<'builder>>),

    #[display(fmt = "imax({_0}, {_1})")]
    IMax(Box<Builder<'builder>>, Box<Builder<'builder>>),

    Var(&'builder str),
}

impl<'build> Builder<'build> {
    /// Realise a builder into a [`Level`]. This internally uses functions described in
    /// the [builder](`crate::memory::level::builder`) module.
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultLevel<'arena> {
        arena.build_level(self.partial_application())
    }

    pub(in super::super) fn partial_application(&self) -> impl BuilderTrait<'build> + '_ {
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
            Plus(ref u, n) => plus(u.partial_application(), n)(arena, env),
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
