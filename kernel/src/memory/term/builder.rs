//! A collection of safe functions to build [`Term`]s.
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

use derive_more::{Constructor, Deref, Display};
use im_rc::hashmap::HashMap as ImHashMap;
use utils::error::Error;
use utils::location::Location;
use utils::trace::{Trace, Traceable, TraceableError};

use super::{DeBruijnIndex, Term};
use crate::error::ResultTerm;
use crate::memory::arena::Arena;
use crate::memory::declaration::builder as declaration;
use crate::memory::level::builder as level;

/// The kind of the error that can occur when building a [`Term`].
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind<'arena> {
    /// The identifier is not bound in the given context.
    #[display(fmt = "unknown identifier {_0}")]
    ConstNotFound(&'arena str),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type)
pub type Environment<'build, 'arena> = ImHashMap<&'build str, (DeBruijnIndex, Term<'arena>)>;

/// The trait of closures which build terms with an adequate logic.
///
/// A call with a quadruplet of arguments `(arena, env, lvl_env, index)` of a closure with this
/// trait should build a definite term in the [`Arena`] `arena`, knowing the bindings declared in
/// `env` and `lvl_env`, provided that the term is built at a current depth `index`.
///
/// Please note that this is just a trait alias, meaning it enforces few constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
#[allow(clippy::module_name_repetitions)]
pub trait BuilderTrait<'build> = for<'arena> FnOnce(
    &mut Arena<'arena>,
    &Environment<'build, 'arena>,
    &level::Environment<'build>,
    DeBruijnIndex,
) -> ResultTerm<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultTerm<'arena> {
        f(self, &Environment::new(), &level::Environment::new(), 0.into())
    }
}

/// Returns a closure building a variable associated to the name `name`
#[inline]
#[must_use]
pub const fn var(name: &str) -> impl BuilderTrait<'_> {
    move |arena, env, _, depth| {
        env.get(name)
            .map(|&(bind_depth, term)| {
                // This is arguably an eager computation, it could be worth making it lazy,
                // or at least memoizing it so as to not compute it again
                let var_type = term.shift(usize::from(depth - bind_depth), 0, arena);
                Term::var(depth - bind_depth, var_type, arena)
            })
            .or_else(|| arena.get_binding(name))
            .ok_or_else(|| Error::new(ErrorKind::ConstNotFound(arena.store_name(name)).into()))
    }
}

/// Returns a closure building a variable associated to the name `name`, potentially from a
/// declaration instantiated with the given parameters.
#[inline]
#[must_use]
pub const fn var_instance<'build>(name: &'build str, levels: &'build [level::Builder<'build>]) -> impl BuilderTrait<'build> {
    move |arena, env, lvl_env, depth| {
        if levels.len() == 0 {
            var(name)(arena, env, lvl_env, depth)
        } else {
            decl(declaration::var(name, levels))(arena, env, lvl_env, depth)
        }
    }
}

/// Returns a closure building the Prop term.
#[inline]
#[must_use]
pub const fn prop<'build>() -> impl BuilderTrait<'build> {
    |arena, _, _, _| Ok(Term::prop(arena))
}

/// Returns a closure building the Type `level` term.
#[inline]
#[no_coverage]
pub const fn type_<'build, F: level::BuilderTrait<'build>>(level: F) -> impl BuilderTrait<'build> {
    move |arena, _, lvl_env, _| Ok(Term::sort(level(arena, lvl_env)?.succ(arena), arena))
}

/// Returns a closure building the Type `level` term (indirection from `usize`).
#[inline]
#[must_use]
pub const fn type_usize<'build>(level: usize) -> impl BuilderTrait<'build> {
    move |arena, _, _, _| Ok(Term::type_usize(level, arena))
}

/// Returns a closure building the Sort `level` term.
#[inline]
#[no_coverage]
pub const fn sort<'build, F: level::BuilderTrait<'build>>(level: F) -> impl BuilderTrait<'build> {
    move |arena, _, lvl_env, _| Ok(Term::sort(level(arena, lvl_env)?, arena))
}

/// Returns a closure building the Sort `level` term (indirection from `usize`).
#[inline]
#[must_use]
pub const fn sort_usize<'build>(level: usize) -> impl BuilderTrait<'build> {
    move |arena, _, _, _| Ok(Term::sort_usize(level, arena))
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn app<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(u1: F1, u2: F2) -> impl BuilderTrait<'build> {
    |arena, env, lvl_env, depth| {
        let u1 = u1(arena, env, lvl_env, depth).trace_err(Trace::Left)?;
        let u2 = u2(arena, env, lvl_env, depth).trace_err(Trace::Right)?;
        Ok(u1.app(u2, arena))
    }
}

/// Returns a closure building the lambda-abstraction with a body built from `body` and an argument
/// type from `arg_type`.
#[inline]
#[no_coverage]
pub const fn abs<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build> {
    move |arena, env, lvl_env, depth| {
        let arg_type = arg_type(arena, env, lvl_env, depth).trace_err(Trace::Left)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, lvl_env, depth + 1.into()).trace_err(Trace::Right)?;
        Ok(arg_type.abs(body, arena))
    }
}

/// Returns a closure building the dependant product of a term built from `body` over all elements
/// of the type built from `arg_type`.
#[inline]
#[no_coverage]
pub const fn prod<'build, F1: BuilderTrait<'build>, F2: BuilderTrait<'build>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build> {
    move |arena, env, lvl_env, depth| {
        let arg_type = arg_type(arena, env, lvl_env, depth).trace_err(Trace::Left)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, lvl_env, depth + 1.into()).trace_err(Trace::Right)?;
        Ok(arg_type.prod(body, arena))
    }
}

/// Returns a closure building the term associated to the instantiated declaration `decl`.
#[inline]
#[no_coverage]
pub const fn decl<'build, F: declaration::InstantiatedBuilderTrait<'build>>(decl: F) -> impl BuilderTrait<'build> {
    move |arena, _, lvl_env, _| Ok(Term::decl(decl(arena, lvl_env)?, arena))
}

/// Wrapper template of [`Payload`], including [`Location`].
#[derive(Clone, Constructor, Debug, Deref, Display, PartialEq, Eq)]
#[display(fmt = "{payload}")]
pub struct Builder<'build> {
    /// Location of the term.
    location: Location,

    /// Term's effective builder.
    #[deref]
    payload: Payload<'build>,
}

/// Template of terms.
///
/// A Builder describes a term in a naive but easy to build manner. It strongly resembles the
/// [payload](`crate::memory::term::Payload`) type, except that `Var`, `Abs` and `Prod` constructors
/// include a name, as in the classic way of writing lambda-terms (i.e. no de Bruijn indices
/// involved). Because its purpose is to provide an easy way to build terms, even through the API,
/// it offers different ways to build some terms, for convenience.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Payload<'build> {
    #[display(fmt = "Prop")]
    Prop,

    /// A regular variable
    Var(&'build str),

    /// A variable that may or may not be an instantiated declaration
    #[display(fmt = "{_0}")]
    VarInstance(&'build str, Vec<level::Builder<'build>>),

    #[display(fmt = "Type {_0}")]
    Type(Box<level::Builder<'build>>),

    #[display(fmt = "Sort {_0}")]
    Sort(Box<level::Builder<'build>>),

    #[display(fmt = "{_0} {_1}")]
    App(Box<Builder<'build>>, Box<Builder<'build>>),

    #[display(fmt = "\u{003BB} {_0}: {_1} \u{02192} {_2}")]
    Abs(&'build str, Box<Builder<'build>>, Box<Builder<'build>>),

    #[display(fmt = "\u{03A0} {_0}: {_1} \u{02192} {_2}")]
    Prod(&'build str, Box<Builder<'build>>, Box<Builder<'build>>),

    Decl(Box<declaration::InstantiatedBuilder<'build>>),
}

impl<'build> Traceable for Builder<'build> {
    #[inline]
    fn apply_trace(&self, trace: &[Trace]) -> Location {
        let mut builder: &Builder<'build> = self;

        for trace in trace.iter().rev() {
            builder = match (trace, &builder.payload) {
                (Trace::Left, Payload::App(lhs, _)) => lhs,
                (Trace::Right, Payload::App(_, rhs)) => rhs,

                (Trace::Left, Payload::Abs(_, lhs, _)) => lhs,
                (Trace::Right, Payload::Abs(_, _, rhs)) => rhs,

                (Trace::Left, Payload::Prod(_, lhs, _)) => lhs,
                (Trace::Right, Payload::Prod(_, _, rhs)) => rhs,

                _ => unreachable!("invalid trace"),
            };
        }

        builder.location
    }
}

impl<'build> Builder<'build> {
    /// Realise a builder into a [`Term`]. This internally uses functions described in
    /// the [builder](`crate::memory::term::builder`) module.
    #[inline]
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultTerm<'arena> {
        arena.build(self.partial_application())
    }

    pub(in crate::memory) fn partial_application(&'build self) -> impl BuilderTrait<'build> {
        |arena, env, lvl_env, depth| self.realise_in_context(arena, env, lvl_env, depth)
    }

    fn realise_in_context<'arena>(
        &'build self,
        arena: &mut Arena<'arena>,
        env: &Environment<'build, 'arena>,
        lvl_env: &level::Environment<'build>,
        depth: DeBruijnIndex,
    ) -> ResultTerm<'arena> {
        match **self {
            Payload::Prop => prop()(arena, env, lvl_env, depth),
            Payload::Var(s) => var(s)(arena, env, lvl_env, depth),
            Payload::VarInstance(name, ref levels) => var_instance(name, levels)(arena, env, lvl_env, depth),
            Payload::Type(ref level) => type_(level.partial_application())(arena, env, lvl_env, depth),
            Payload::Sort(ref level) => sort(level.partial_application())(arena, env, lvl_env, depth),
            Payload::App(ref l, ref r) => app(l.partial_application(), r.partial_application())(arena, env, lvl_env, depth),
            Payload::Abs(s, ref arg, ref body) => {
                abs(s, arg.partial_application(), body.partial_application())(arena, env, lvl_env, depth)
            },
            Payload::Prod(s, ref arg, ref body) => {
                prod(s, arg.partial_application(), body.partial_application())(arena, env, lvl_env, depth)
            },
            Payload::Decl(ref decl_builder) => decl(decl_builder.partial_application())(arena, env, lvl_env, depth),
        }
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub trait BuilderTrait = for<'arena> FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_term_raw<F: BuilderTrait>(&mut self, f: F) -> Term<'arena> {
            f(self)
        }
    }

    pub const fn var<F: BuilderTrait>(index: DeBruijnIndex, type_: F) -> impl BuilderTrait {
        move |arena| {
            let ty = type_(arena);
            Term::var(index, ty, arena)
        }
    }

    pub const fn prop() -> impl BuilderTrait {
        |arena| Term::prop(arena)
    }

    pub const fn type_usize(level: usize) -> impl BuilderTrait {
        move |arena| Term::type_usize(level, arena)
    }

    pub const fn sort_<F: level::raw::BuilderTrait>(level: F) -> impl BuilderTrait {
        move |arena| Term::sort(level(arena), arena)
    }

    pub const fn app<F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.app(u2, arena)
        }
    }

    pub const fn abs<F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.abs(u2, arena)
        }
    }

    pub const fn prod<F1: BuilderTrait, F2: BuilderTrait>(u1: F1, u2: F2) -> impl BuilderTrait {
        |arena| {
            let u1 = u1(arena);
            let u2 = u2(arena);
            u1.prod(u2, arena)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder_trace() {
        // This following term is completely ill-typed, location do not have any meaning.
        // We want to test that the trace is correctly applied.
        let builder = Builder::new(
            Location::new((1, 1), (1, 1)),
            Payload::Abs(
                "x",
                Box::new(Builder::new(
                    Location::new((2, 2), (2, 2)),
                    Payload::App(
                        Box::new(Builder::new(Location::new((3, 3), (3, 3)), Payload::Prop)),
                        Box::new(Builder::new(Location::new((4, 4), (4, 4)), Payload::Prop)),
                    ),
                )),
                Box::new(Builder::new(
                    Location::new((5, 5), (5, 5)),
                    Payload::Prod(
                        "x",
                        Box::new(Builder::new(Location::new((6, 6), (6, 6)), Payload::Prop)),
                        Box::new(Builder::new(Location::new((7, 7), (7, 7)), Payload::Prop)),
                    ),
                )),
            ),
        );

        // Beware, the trace has to be applied in the reverse order (depth-first).
        assert_eq!(builder.apply_trace(&[]), Location::new((1, 1), (1, 1)));

        assert_eq!(builder.apply_trace(&[Trace::Left]), Location::new((2, 2), (2, 2)));
        assert_eq!(builder.apply_trace(&[Trace::Left, Trace::Left]), Location::new((3, 3), (3, 3)));
        assert_eq!(builder.apply_trace(&[Trace::Right, Trace::Left]), Location::new((4, 4), (4, 4)));

        assert_eq!(builder.apply_trace(&[Trace::Right]), Location::new((5, 5), (5, 5)));
        assert_eq!(builder.apply_trace(&[Trace::Left, Trace::Right]), Location::new((6, 6), (6, 6)));
        assert_eq!(builder.apply_trace(&[Trace::Right, Trace::Right]), Location::new((7, 7), (7, 7)));
    }
}
