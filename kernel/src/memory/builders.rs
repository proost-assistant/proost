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
use super::level_builders::{LevelBuilder, LevelEnvironment};
use super::term::{DeBruijnIndex, Term};
use crate::error::{Error, ResultTerm};

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DefinitionError<'arena> {
    #[display(fmt = "unknown identifier {}", _0)]
    ConstNotFound(&'arena str),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type)
pub type Environment<'build, 'arena> = ImHashMap<&'build str, (DeBruijnIndex, Term<'arena>)>;

/// The trait of closures which build terms with an adequate logic.
///
/// A call with a triplet of arguments `(arena, env, index)` of a closure with this trait should
/// build a definite term in the [`Arena`] `arena`, knowing the bindings declared in `environment`,
/// provided that the term is built at a current depth `index`.
///
/// Please note that this is just a trait alias, meaning it enforces very little constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
pub trait BuilderTrait<'build, 'arena> = FnOnce(
    &mut Arena<'arena>,
    &Environment<'build, 'arena>,
    &LevelEnvironment<'build>,
    DeBruijnIndex,
) -> ResultTerm<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build<'build, F: BuilderTrait<'build, 'arena>>(
        &mut self,
        lvl_env: &LevelEnvironment<'build>,
        f: F,
    ) -> ResultTerm<'arena>
    where
        'arena: 'build,
    {
        f(self, &Environment::new(), lvl_env, 0.into())
    }
}

/// Returns a closure building a variable associated to the name `name`
#[inline]
pub const fn var<'build, 'arena>(name: &'build str) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, _ : &LevelEnvironment<'build>, depth| {
        env.get(name)
            .map(|(bind_depth, term)| {
                // This is arguably an eager computation, it could be worth making it lazy,
                // or at least memoizing it so as to not compute it again
                let var_type = term.shift(usize::from(depth - *bind_depth), 0, arena);
                Term::var(depth - *bind_depth, var_type, arena)
            })
            .or_else(|| arena.get_binding(name))
            .ok_or(Error {
                kind: DefinitionError::ConstNotFound(arena.store_name(name)).into(),
            })
    }
}

/// Returns a closure building the Prop term.
#[inline]
pub const fn prop<'build, 'arena>() -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _: &LevelEnvironment<'build>, _| Ok(Term::prop(arena))
}

/// Returns a closure building the Type `i` term, where `i` is an integer
#[inline]
pub const fn type_usize<'build, 'arena>(level: usize) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _: &LevelEnvironment<'build>, _| {
        Ok(Term::type_usize(level, arena))
    }
}

/// Returns a closure building the Sort `level` term.
#[inline]
pub const fn sort<'build, 'arena>(level: Level<'arena>) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _: &LevelEnvironment<'build>, _| {
        Ok(Term::sort(level, arena))
    }
}

/// Returns a closure building the Sort `level` term (indirection from `usize`).
#[inline]
pub const fn sort_usize<'build, 'arena>(level: usize) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _: &LevelEnvironment<'build>, _| {
        Ok(Term::sort_usize(level, arena))
    }
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn app<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    u1: F1,
    u2: F2,
) -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, lvl_env: &LevelEnvironment<'build>, depth| {
        let u1 = u1(arena, env, lvl_env, depth)?;
        let u2 = u2(arena, env, lvl_env, depth)?;
        Ok(u1.app(u2, arena))
    }
}

/// Returns a closure building the lambda-abstraction with a body built from `body` and an argument
/// type from `arg_type`.
#[inline]
#[no_coverage]
pub const fn abs<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, lvl_env: &LevelEnvironment<'build>, depth| {
        let arg_type = arg_type(arena, env, lvl_env, depth)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, lvl_env, depth + 1.into())?;
        Ok(arg_type.abs(body, arena))
    }
}

/// Returns a closure building the dependant product of a term built from `body` over all elements
/// of the type built from `arg_type`.
#[inline]
#[no_coverage]
pub const fn prod<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, lvl_env: &LevelEnvironment<'build>, depth| {
        let arg_type = arg_type(arena, env, lvl_env, depth)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, lvl_env, depth + 1.into())?;
        Ok(arg_type.prod(body, arena))
    }
}

/// Template of terms.
///
/// A Builder describes a term in a naive but easy to build manner. It strongly resembles the
/// [payload](`crate::term::arena::Payload`) type, except that `Var`, `Abs` and `Prod` constructors
/// include a name, as in the classic way of writing lambda-terms (i.e. no de Bruijn indices
/// involved).
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum TermBuilder<'r> {
    #[display(fmt = "{}", _0)]
    Var(&'r str),

    #[display(fmt = "Sort {}", _0)]
    Sort(LevelBuilder<'r>),

    #[display(fmt = "{} {}", _0, _1)]
    App(Box<TermBuilder<'r>>, Box<TermBuilder<'r>>),

    #[display(fmt = "\u{003BB} {}: {} \u{02192} {}", _0, _1, _2)]
    Abs(&'r str, Box<TermBuilder<'r>>, Box<TermBuilder<'r>>),

    #[display(fmt = "\u{03A0} {}: {} \u{02192} {}", _0, _1, _2)]
    Prod(&'r str, Box<TermBuilder<'r>>, Box<TermBuilder<'r>>),
}

impl<'build> TermBuilder<'build> {
    /// Build a terms from a [`Builder`]. This internally uses functions described in the
    /// [builders](`crate::term::builders`) module.
    pub fn realise<'arena>(&self, lvl_env: &LevelEnvironment<'build>, arena: &mut Arena<'arena>) -> ResultTerm<'arena> where
    'arena: 'build{
        arena.build(lvl_env, self.partial_application())
    }

    fn partial_application<'arena>(&self) -> impl BuilderTrait<'build, 'arena> + '_ {
        |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, lvl_env: &LevelEnvironment<'build>, depth| {
            self.realise_in_context(arena, env, lvl_env, depth)
        }
    }

    fn realise_in_context<'arena>(
        &self,
        arena: &mut Arena<'arena>,
        env: &Environment<'build, 'arena>,
        lvl_env: &LevelEnvironment<'build>,
        depth: DeBruijnIndex,
    ) -> ResultTerm<'arena> {
        use TermBuilder::*;
        match &*self {
            Var(s) => var(s)(arena, env, lvl_env, depth),
            Sort(level) => sort(LevelBuilder::partial_application(&level)(arena, lvl_env)?)(arena, env, lvl_env, depth),
            App(ref l, ref r) => app(l.partial_application(), r.partial_application())(arena, env, lvl_env, depth),
            Abs(s, ref arg, ref body) => abs(s, arg.partial_application(), body.partial_application())(arena, env, lvl_env, depth),
            Prod(s, ref arg, ref body) => {
                prod(s, arg.partial_application(), body.partial_application())(arena, env, lvl_env, depth)
            },
        }
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub trait BuilderTrait<'arena> = FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_term_raw<F: BuilderTrait<'arena>>(&mut self, f: F) -> Term<'arena> {
            f(self)
        }
    }

    impl<'arena> Term<'arena> {
        pub(crate) const fn into(self) -> impl BuilderTrait<'arena> {
            move |_: &mut Arena<'arena>| self
        }
    }

    pub const fn var<'arena, F: BuilderTrait<'arena>>(index: DeBruijnIndex, type_: F) -> impl BuilderTrait<'arena> {
        move |arena: &mut Arena<'arena>| {
            let ty = type_(arena);
            Term::var(index, ty, arena)
        }
    }

    pub const fn prop<'arena>() -> impl BuilderTrait<'arena> {
        |arena: &mut Arena<'arena>| Term::prop(arena)
    }

    pub const fn sort_<'arena>(level: Level<'arena>) -> impl BuilderTrait<'arena> {
        move |arena: &mut Arena<'arena>| Term::sort(level, arena)
    }

    pub const fn app<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |arena: &mut Arena<'arena>| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.app(u1, arena)
        }
    }

    pub const fn abs<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |arena: &mut Arena<'arena>| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.abs(u2, arena)
        }
    }

    pub const fn prod<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |arena: &mut Arena<'arena>| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.prod(u2, arena)
        }
    }
}
