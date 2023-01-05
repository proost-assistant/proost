//! A collection of safe functions to build [`Declaration`]s. and [`InstantiatedDeclaration`]s.
//!
//! This module provides two main ways of building these. The first one is via closures: users can
//! manipulate closures and create bigger ones which, when [built](Arena::build), provide the expected
//! term.
//!
//! The overall syntax remains transparent to the user. This means the user focuses on the
//! structure of the term they want to build, while the [closures](`BuilderTrait`) internally build an appropriate
//! logic; namely verifying the correct amount of variables have been supplied or bound.

use derive_more::Display;
use utils::error::Error;
use utils::location::Location;
use utils::trace::{Trace, Traceable};

use super::{Declaration, InstantiatedDeclaration};
use crate::error::{Result, ResultDecl, ResultInstantiatedDecl};
use crate::memory::arena::Arena;
use crate::memory::level::builder as level;
use crate::memory::term::builder as term;

/// The kind of the error that can occur when building a [`Declaration`].
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum ErrorKind<'arena> {
    /// An incorrect amount of universe variables has been provided
    #[display(fmt = "expected {_0} universe variables, got {_1}")]
    IncorrectVariableNumber(usize, usize),

    /// The declaration is unknown
    #[display(fmt = "unknown declaration {_0}")]
    UnknownDeclaration(&'arena str),
}

/// The trait of builders producing declarations.
///
/// Note that, unlike the other building traits, this one only takes an arena as an argument.
/// This is because declarations cannot be declared locally (that is, within a context where some
/// extra local variables are bound by lambda-abstraction).
#[allow(clippy::module_name_repetitions)]
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>) -> ResultDecl<'arena>;

/// The trait of builders producing instantiated declarations.
pub trait InstantiatedBuilderTrait<'build> =
    for<'arena> FnOnce(&mut Arena<'arena>, &level::Environment<'build>) -> ResultInstantiatedDecl<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the declaration built from the given closure, provided with an empty context, at
    /// depth 0.
    ///
    /// # Errors
    /// If the declaration could not be built, yields an error indicating the reason
    #[inline]
    pub fn build_declaration<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultDecl<'arena> {
        f(self)
    }

    /// Returns the instantiated declaration built from the given closure, provided with an empty
    /// context, at depth 0.
    /// # Errors
    /// If the instantiated declaration could not be built, yields an error indicating the reason
    #[inline]
    pub fn build_instantiated_declaration<'build, F: InstantiatedBuilderTrait<'build>>(
        &mut self,
        f: F,
    ) -> ResultInstantiatedDecl<'arena> {
        f(self, &level::Environment::new())
    }
}

/// Returns a builder creating `term`, where universe variables are described by `vars`.
#[inline]
pub fn declaration<'build, F: term::BuilderTrait<'build>>(term: F, vars: &[&'build str]) -> impl BuilderTrait<'build> {
    let len = vars.len();
    let lvl_env = vars.iter().enumerate().map(|(n, name)| (*name, n)).collect();
    move |arena| Ok(Declaration::new(term(arena, &term::Environment::new(), &lvl_env, 0.into())?, len))
}

/// Template of declarations.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum Builder<'build> {
    #[display(fmt = "{_0}")]
    Decl(Box<term::Builder<'build>>, Vec<&'build str>),
}

impl<'build> Traceable for Builder<'build> {
    #[inline]
    fn apply_trace(&self, trace: &[Trace]) -> Location {
        match *self {
            Builder::Decl(ref term, _) => term.apply_trace(trace),
        }
    }
}

impl<'build> Builder<'build> {
    /// Realise a builder into a [`Declaration`]. This internally uses functions described in
    /// the [builder](`crate::memory::declaration::builder`) module.
    ///
    /// # Errors
    /// If the declaration could not be built, yields an error indicating the reason
    #[inline]
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultDecl<'arena> {
        arena.build_declaration(self.partial_application())
    }

    /// Associates a builder to a builder trait.
    fn partial_application(&self) -> impl BuilderTrait<'build> + '_ {
        |arena| self.realise_in_context(arena)
    }

    /// Provides a correspondence between builder items and functions with the builder trait
    fn realise_in_context<'arena>(&self, arena: &mut Arena<'arena>) -> ResultDecl<'arena> {
        match *self {
            Builder::Decl(ref term, ref vars) => declaration(term.partial_application(), vars.as_slice())(arena),
        }
    }
}

/// Base function for the instantiated declaration builders.
fn try_build_instance<'arena, 'build>(
    decl: Declaration<'arena>,
    levels: &'build [level::Builder<'build>],
    arena: &mut Arena<'arena>,
    env: &level::Environment<'build>,
) -> ResultInstantiatedDecl<'arena> {
    let levels = levels
        .iter()
        .map(|level_builder| level_builder.realise_in_context(arena, env))
        .collect::<Result<Vec<_>>>()?;

    if decl.1 == levels.len() {
        Ok(InstantiatedDeclaration::instantiate(decl, levels.as_slice(), arena))
    } else {
        Err(Error::new(ErrorKind::IncorrectVariableNumber(decl.1, levels.len()).into()))
    }
}

/// Returns a builder creating the declaration built by `decl` instantiated with the universe
/// levels `levels`.
///
/// Please note that this is the only function from the closure API requiring the use of Builders.
/// This is by choice, as opposed to the other possibility, where `levels` would be a slice over
/// `Box<dyn level::BuilderTrait>`, which is not interesting performance-wise.
#[inline]
pub fn instance<'build, F: BuilderTrait<'build>>(
    decl: F,
    levels: &'build [level::Builder<'build>],
) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| try_build_instance(decl(arena)?, levels, arena, env)
}

/// Returns a builder creating the declaration bound by `name`, instantiated with the universe
/// levels `levels`.
#[inline]
#[must_use]
pub fn var<'build>(name: &'build str, levels: &'build [level::Builder<'build>]) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| {
        let decl = arena
            .get_binding_decl(name)
            .ok_or_else(|| Error::new(ErrorKind::UnknownDeclaration(arena.store_name(name)).into()))?;

        try_build_instance(decl, levels, arena, env)
    }
}

/// A builder for instantiated declarations.
///
/// While there is fundamentally only one way to instantiate a declaration, this builder exposes
/// two variants: the [first](`InstantiatedBuilder::Instance`) is to be used typically through the
/// API, in a context where, for instance, the associated declaration is not bound in the arena.
///
/// On the other hand, the [second variant](`InstantiatedBuilder::Var`) is typically used by the
/// parser, as it corresponds to the only possible scenario in a file.
#[allow(clippy::module_name_repetitions)]
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InstantiatedBuilder<'build> {
    Instance(Box<Builder<'build>>, Vec<level::Builder<'build>>),

    Var(&'build str, Vec<level::Builder<'build>>),
}

impl<'arena> core::fmt::Display for InstantiatedBuilder<'arena> {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            InstantiatedBuilder::Instance(ref decl, ref params) => {
                write!(f, "{decl}.{{")?;
                params.iter().try_for_each(|level| write!(f, "{level}, "))?;
                write!(f, "}}")
            },
            InstantiatedBuilder::Var(decl, ref params) => {
                write!(f, "{decl}.{{")?;
                params.iter().try_for_each(|level| write!(f, "{level}, "))?;
                write!(f, "}}")
            },
        }
    }
}
impl<'build> InstantiatedBuilder<'build> {
    /// Realise a builder into an [`InstantiatedDeclaration`]. This internally uses functions described in
    /// the [builder](`crate::memory::declaration::builder`) module.
    ///
    /// # Errors
    /// If the instantiated declaration could not be built, yields an error indicating the reason
    #[inline]
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultInstantiatedDecl<'arena> {
        arena.build_instantiated_declaration(self.partial_application())
    }

    /// Associates a builder to a builder trait.
    pub(in crate::memory) fn partial_application(&'build self) -> impl InstantiatedBuilderTrait<'build> {
        |arena, lvl_env| self.realise_in_context(arena, lvl_env)
    }

    /// Provides a correspondence between builder items and functions with the builder trait
    fn realise_in_context<'arena>(
        &'build self,
        arena: &mut Arena<'arena>,
        lvl_env: &level::Environment<'build>,
    ) -> ResultInstantiatedDecl<'arena> {
        match *self {
            InstantiatedBuilder::Instance(ref decl, ref levels) => instance(decl.partial_application(), levels)(arena, lvl_env),
            InstantiatedBuilder::Var(name, ref levels) => var(name, levels)(arena, lvl_env),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Builder::Decl;
    use super::*;
    use crate::memory::term::builder::{Builder, Payload};

    #[test]
    fn builder_trace() {
        // This following term is completely ill-typed, location do not have any meaning.
        // We want to test that the trace is correctly applied.
        let builder = Decl(
            Box::new(Builder::new(
                Location::new((1, 1), (1, 1)),
                Payload::App(
                    Box::new(Builder::new(Location::new((2, 2), (2, 2)), Payload::Prop)),
                    Box::new(Builder::new(Location::new((3, 3), (3, 3)), Payload::Prop)),
                ),
            )),
            vec![],
        );

        assert_eq!(builder.apply_trace(&[]), Location::new((1, 1), (1, 1)));
    }
}
