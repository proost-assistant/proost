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
use im_rc::hashmap::HashMap as ImHashMap;

use super::arena::Arena;
use super::level::Level;
use crate::error::{Error, ResultLevel};

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum LevelError<'arena> {
    #[display(fmt = "unknown universe variable {}", _0)]
    VarNotFound(&'arena str),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type)
pub type LevelEnvironment<'build, 'arena> = ImHashMap<&'build str, Level<'arena>>;

/// The trait of closures which build terms with an adequate logic.
///
/// A call with a triplet of arguments `(arena, env, index)` of a closure with this trait should
/// build a definite term in the [`Arena`] `arena`, knowing the bindings declared in `environment`,
/// provided that the term is built at a current depth `index`.
///
/// Please note that this is just a trait alias, meaning it enforces very little constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
pub trait LevelBuilderTrait<'build, 'arena> = FnOnce(&mut Arena<'arena>, &LevelEnvironment<'build, 'arena>) -> ResultLevel<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build_level<'build, F: LevelBuilderTrait<'build, 'arena>>(&mut self, f: F) -> ResultLevel<'arena>
    where
        'arena: 'build,
    {
        f(self, &LevelEnvironment::new())
    }
}

/// Returns a closure building a variable associated to the name `name`
#[inline]
pub const fn var<'build, 'arena>(name: &'build str) -> impl LevelBuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>| {
        env.get(name).map(|lvl| *lvl).ok_or(Error {
            kind: LevelError::VarNotFound(arena.store_name(name)).into(),
        })
    }
}

/// Returns a closure building the 0 level.
#[inline]
pub const fn zero<'build, 'arena>() -> impl LevelBuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, _: &LevelEnvironment<'build, 'arena>| Ok(Level::zero(arena))
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn succ<'build, 'arena, F1: LevelBuilderTrait<'build, 'arena>>(u1: F1) -> impl LevelBuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>| {
        let u1 = u1(arena, env)?;
        Ok(u1.succ(arena))
    }
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn max<'build, 'arena, F1: LevelBuilderTrait<'build, 'arena>, F2: LevelBuilderTrait<'build, 'arena>>(
    u1: F1,
    u2: F2,
) -> impl LevelBuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>| {
        let u1 = u1(arena, env)?;
        let u2 = u2(arena, env)?;
        Ok(u1.max(u2, arena))
    }
}

#[inline]
#[no_coverage]
pub const fn imax<'build, 'arena, F1: LevelBuilderTrait<'build, 'arena>, F2: LevelBuilderTrait<'build, 'arena>>(
    u1: F1,
    u2: F2,
) -> impl LevelBuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>| {
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
pub enum LevelBuilder<'r> {
    #[display(fmt = "0")]
    Zero,

    #[display(fmt = "S({})", _0)]
    Succ(Box<LevelBuilder<'r>>),

    #[display(fmt = "max({},{})", _0, _1)]
    Max(Box<LevelBuilder<'r>>, Box<LevelBuilder<'r>>),

    #[display(fmt = "imax({},{})", _0, _1)]
    IMax(Box<LevelBuilder<'r>>, Box<LevelBuilder<'r>>),

    Var(&'r str),
}

impl<'build> LevelBuilder<'build> {
    /// Build a terms from a [`Builder`]. This internally uses functions described in the
    /// [builders](`crate::term::builders`) module.
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultLevel<'arena> {
        arena.build_level(self.partial_application())
    }

    pub fn partial_application<'arena>(&self) -> impl LevelBuilderTrait<'build, 'arena> + '_ {
        |arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>| self.realise_in_context(arena, env)
    }

    fn realise_in_context<'arena>(&self, arena: &mut Arena<'arena>, env: &LevelEnvironment<'build, 'arena>) -> ResultLevel<'arena> {
        use LevelBuilder::*;
        match *self {
            Var(s) => var(s)(arena, env),
            Zero => zero()(arena, env),
            Succ(ref l) => succ(l.partial_application())(arena, env),
            Max(ref l, ref r) => max(l.partial_application(), r.partial_application())(arena, env),
            IMax(ref l, ref r) => imax(l.partial_application(), r.partial_application())(arena, env),
        }
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub trait LevelBuilderTrait<'arena> = FnOnce(&mut Arena<'arena>) -> Level<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_level_raw<F: LevelBuilderTrait<'arena>>(&mut self, f: F) -> Level<'arena> {
            f(self)
        }
    }

    impl<'arena> Level<'arena> {
        pub(crate) const fn into(self) -> impl LevelBuilderTrait<'arena> {
            move |_: &mut Arena<'arena>| self
        }
    }

    pub const fn var<'arena, F: LevelBuilderTrait<'arena>>(lvl: F) -> impl LevelBuilderTrait<'arena> {
        move |env: &mut Arena<'arena>| lvl(env)
    }

    pub const fn zero<'arena>() -> impl LevelBuilderTrait<'arena> {
        |env: &mut Arena<'arena>| Level::zero(env)
    }

    pub const fn succ<'arena, F1: LevelBuilderTrait<'arena>>(u1: F1) -> impl LevelBuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            u1.succ(env)
        }
    }

    pub const fn max<'arena, F1: LevelBuilderTrait<'arena>, F2: LevelBuilderTrait<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl LevelBuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            u1.max(u2, env)
        }
    }

    pub const fn imax<'arena, F1: LevelBuilderTrait<'arena>, F2: LevelBuilderTrait<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl LevelBuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            u1.imax(u2, env)
        }
    }
}
