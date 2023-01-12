//! Builder types for [`Term`]s.
//!
//! This is a naive description of terms. It can be transformed into concrete declarations
//! through the [`BuiderTrait`](kernel::memory::term::builder::BuilderTrait) declared in
//! the kernel.

use derive_more::{Constructor, Deref, Display};
use kernel::error::ResultTerm;
use kernel::memory::arena::Arena;
use kernel::memory::declaration::builder as builder_declaration;
use kernel::memory::term::builder::{abs, app, decl, prod, prop, sort, type_, var, BuilderTrait};
use kernel::memory::term::Term;
use kernel::trace::{Trace, Traceable};

use super::Buildable;
use crate::builder::{declaration, level};
use crate::location::Location;

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
/// A Builder describes a term in a naive but easy-to-build manner.
///
/// Please refer to the item descriptions in [terms](crate::memory::term::Payload) for a
/// description of the corresponding items. Please understand that there are still differences,
/// most notably, these fields correspond to a classic way of writing lambda-terms (i.e. no de
/// Bruijn indices involved).
///
/// Because the purpose of a builder is to provide an easy way to build terms, even through the
/// API, it offers different ways to build some terms, for convenience.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum Payload<'build> {
    Prop,

    /// A regular variable.
    Var(&'build str),

    /// A variable that has not been sanitized
    #[display(fmt = "{}", "_0.join(\"::\")")]
    PreVar(Vec<&'build str>),

    /// A variable denoted by a unique usize identifier
    #[display(fmt = "{_0}")]
    VarId(usize),

    /// A variable that may or may not be an instantiated declaration.
    #[display(fmt = "{_0}")]
    VarInstance(&'build str, Vec<level::Builder<'build>>),

    /// A non sanitize variable that may or may not be an instantiated declaration.
    #[display(fmt = "{}", "_0.join(\"::\")")]
    PreVarInstance(Vec<&'build str>, Vec<level::Builder<'build>>),

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

impl<'build> Traceable<Location> for Builder<'build> {
    #[inline]
    fn apply_trace(&self, trace: &[Trace]) -> Location {
        let builder = trace.iter().rev().fold(self, |builder, trace| match (trace, &builder.payload) {
            (Trace::Left, Payload::App(lhs, _)) => lhs,
            (Trace::Right, Payload::App(_, rhs)) => rhs,

            (Trace::Left, Payload::Abs(_, lhs, _)) => lhs,
            (Trace::Right, Payload::Abs(_, _, rhs)) => rhs,

            (Trace::Left, Payload::Prod(_, lhs, _)) => lhs,
            (Trace::Right, Payload::Prod(_, _, rhs)) => rhs,

            _ => unreachable!("invalid trace"),
        });

        builder.location
    }
}

impl<'build> Buildable<'build> for Builder<'build> {
    type Output<'arena> = Term<'arena>;

    type Closure = impl BuilderTrait<'build>;

    /// Realise a builder into a [`Term`]. This internally uses functions described in
    /// the [builder](`crate::memory::term::builder`) module.
    ///
    /// # Errors
    /// If the term could not be built, yields an error indicating the reason.
    #[inline]
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultTerm<'arena> {
        arena.build(self.as_closure())
    }

    /// Associates a builder to a builder trait.
    #[inline]
    fn as_closure(&'build self) -> Self::Closure {
        |arena, env, lvl_env, depth| match **self {
            Payload::Prop => prop()(arena, env, lvl_env, depth),
            Payload::Var(s) => var(s)(arena, env, lvl_env, depth),
            Payload::VarInstance(name, ref levels) => {
                decl(builder_declaration::var(name, levels.as_closure()))(arena, env, lvl_env, depth)
            },
            Payload::Type(ref level) => type_(level.as_closure())(arena, env, lvl_env, depth),
            Payload::Sort(ref level) => sort(level.as_closure())(arena, env, lvl_env, depth),
            Payload::App(ref l, ref r) => app(l.as_closure(), r.as_closure())(arena, env, lvl_env, depth),
            Payload::Abs(s, ref arg, ref body) => abs(s, arg.as_closure(), body.as_closure())(arena, env, lvl_env, depth),
            Payload::Prod(s, ref arg, ref body) => prod(s, arg.as_closure(), body.as_closure())(arena, env, lvl_env, depth),
            Payload::Decl(ref decl_builder) => decl(decl_builder.as_closure())(arena, env, lvl_env, depth),
            _ => unreachable!("TODO explanation"),
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
