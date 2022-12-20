//! Collection of safe functions to build Terms
//!
//! This module provides two main ways of building terms. The first one is via closures: users can
//! manipulate closures and create bigger ones which, when [built](Arena::build), provide the expected
//! term.
//!
//! The overall syntax remains transparent to the user. This means the user focuses on the
//! structure of the term they want to build, while the [closures](`BuilderTrait`) internally build an appropriate
//! logic: converting regular terms into de Bruijn-compatible ones, assigning types to variables,
//! etc.
//!
//! The other way to proceed is built on top of the latter. Users can also manipulate a sort of
//! *high-level term* or *template*, described by the public enumeration [`Builder`], and at any
//! moment, [realise](Builder::realise) it.

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

/// The trait of closures which build terms with an adequate logic.
///
/// A call with a triplet of arguments `(arena, env, index)` of a closure with this trait should
/// build a definite term in the [`Arena`] `arena`, knowing the bindings declared in `environment`,
/// provided that the term is built at a current depth `index`.
///
/// Please note that this is just a trait alias, meaning it enforces very little constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>, &Environment<'build>) -> ResultLevel<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build_level<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultLevel<'arena> {
        f(self, &Environment::new())
    }
}

/// Returns a closure building a variable associated to the name `name`
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

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn succ<'build, F1: BuilderTrait<'build>>(u1: F1) -> impl BuilderTrait<'build> {
    |arena, env| {
        let u1 = u1(arena, env)?;
        Ok(u1.succ(arena))
    }
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
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

#[inline]
#[no_coverage]
pub const fn imax<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env| {
        let u1 = u1(arena, env)?;
        let u2 = u2(arena, env)?;
        Ok(u1.imax(u2, arena))
    }
}

/// Template of terms.
///
/// A Builder describes a term in a naive but easy to build manner. It strongly resembles the
/// [payload](`crate::term::arena::Payload`) type, except that `Var`, `Abs` and `Prod` constructors
/// include a name, as in the classic way of writing lambda-terms (i.e. no de Bruijn indices
/// involved).
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
    /// Build a terms from a [`Builder`]. This internally uses functions described in the
    /// [builders](`crate::term::builders`) module.
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

    pub const fn var<'arena, F: BuilderTrait>(lvl: F) -> impl BuilderTrait {
        move |env| lvl(env)
    }

    pub const fn zero<'arena>() -> impl BuilderTrait {
        |env| Level::zero(env)
    }

    pub const fn succ<'arena, F1: BuilderTrait>(u1: F1) -> impl BuilderTrait {
        |env| {
            let u1 = u1(env);
            u1.succ(env)
        }
    }

    pub const fn max<'arena, F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |env| {
            let u1 = u1(env);
            let u2 = u2(env);
            u1.max(u2, env)
        }
    }

    pub const fn imax<'arena, F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |env| {
            let u1 = u1(env);
            let u2 = u2(env);
            u1.imax(u2, env)
        }
    }
}
