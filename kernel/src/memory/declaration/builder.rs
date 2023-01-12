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

use super::{Declaration, InstantiatedDeclaration};
use crate::error::{Error, ResultDecl, ResultInstantiatedDecl};
use crate::memory::arena::{Arena, Id};
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
    UnknownDeclaration(Id<'arena>),
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

/// Base function for the instantiated declaration builders.
fn try_build_instance<'arena, 'build, V: level::VecBuilderTrait<'build>>(
    decl: Declaration<'arena>,
    levels: V,
    arena: &mut Arena<'arena>,
    env: &level::Environment<'build>,
) -> ResultInstantiatedDecl<'arena> {
    let levels = levels(arena, env)?;

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
pub fn instance<'build, F: BuilderTrait<'build>, V: level::VecBuilderTrait<'build>>(
    decl: F,
    levels: V,
) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| try_build_instance(decl(arena)?, levels, arena, env)
}

/// Returns a builder creating the declaration bound by `name`, instantiated with the universe
/// levels `levels`.
#[inline]
#[must_use]
pub fn var<'build, V: level::VecBuilderTrait<'build>>(id: Id<'build>, levels: V) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| {
        let decl = arena
            .get_binding_decl_with_id(&id)
            .ok_or_else(|| Error::new(ErrorKind::UnknownDeclaration(arena.store_id(id)).into()))?;

        try_build_instance(decl, levels, arena, env)
    }
}
