//! TODO
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

use super::super::arena::Arena;
use super::super::level::builder as level;
use super::super::term::builder as term;
use super::{Declaration, InstantiatedDeclaration};
use crate::error::{Error, Result, ResultDecl, ResultInstantiatedDecl};

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DeclarationError<'arena> {
    #[display(fmt = "expected {_0} universe variables, got {_1}")]
    IncorrectVariableNumber(usize, usize),
    UnknownDeclaration(&'arena str),
}

/// The trait of builders producing declarations.
///
/// Note that, unlike the other building traits, this one only takes an arena as an argument.
/// This is because declarations cannot be declared locally (that is, within a context where some
/// extra local variables are bound by lambda-abstraction).
pub trait BuilderTrait<'build> = for<'arena> FnOnce(&mut Arena<'arena>) -> ResultDecl<'arena>;

pub trait InstantiatedBuilderTrait<'build> =
    for<'arena> FnOnce(&mut Arena<'arena>, &level::Environment<'build>) -> ResultInstantiatedDecl<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build_declaration<'build, F: BuilderTrait<'build>>(&mut self, f: F) -> ResultDecl<'arena> {
        f(self)
    }

    #[inline]
    pub fn build_instantiated_declaration<'build, F: InstantiatedBuilderTrait<'build>>(
        &mut self,
        f: F,
    ) -> ResultInstantiatedDecl<'arena> {
        f(self, &level::Environment::new())
    }
}

/// Returns a builder creating `term`, where universe variables are described by `vars`.
pub fn declaration<'build, F: term::BuilderTrait<'build>>(term: F, vars: &[&'build str]) -> impl BuilderTrait<'build> {
    let len = vars.len();
    let lvl_env = vars.iter().enumerate().map(|(n, name)| (*name, n)).collect();
    move |arena| Ok(Declaration::new(term(arena, &term::Environment::new(), &lvl_env, 0.into())?, len))
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Builder<'build> {
    #[display(fmt = "{_0}")]
    Decl(Box<term::Builder<'build>>, Vec<&'build str>),
}

impl<'build> Builder<'build> {
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultDecl<'arena> {
        arena.build_declaration(self.partial_application())
    }

    fn partial_application(&self) -> impl BuilderTrait<'build> + '_ {
        |arena| self.realise_in_context(arena)
    }

    fn realise_in_context<'arena>(&self, arena: &mut Arena<'arena>) -> ResultDecl<'arena> {
        match self {
            Builder::Decl(term, vars) => declaration(term.partial_application(), vars.as_slice())(arena),
        }
    }
}

fn try_build_instance<'arena, 'build>(
    decl: Declaration<'arena>,
    levels: &'build [level::Builder<'build>],
    arena: &mut Arena<'arena>,
    env: &level::Environment<'build>,
) -> ResultInstantiatedDecl<'arena> {
    let levels = levels.iter().map(|level_builder| level_builder.realise_in_context(arena, env)).collect::<Result<Vec<_>>>()?;

    if decl.1 == levels.len() {
        Ok(InstantiatedDeclaration::instantiate(decl, levels.as_slice(), arena))
    } else {
        Err(Error {
            kind: DeclarationError::IncorrectVariableNumber(decl.1, levels.len()).into(),
        })
    }
}

/// Returns a builder creating the declaration built by `decl` instantiated with the universe
/// levels `levels`.
///
/// Please note that this is the only function from the closure API requiring the use of Builders.
/// This is by choice, as opposed to the other possibility, where `levels` would be a slice over
/// `Box<dyn level::BuilderTrait>`, which is not interesting performance-wise.
pub fn instance<'build, F: BuilderTrait<'build>>(
    decl: F,
    levels: &'build [level::Builder<'build>],
) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| try_build_instance(decl(arena)?, levels, arena, env)
}

pub fn var<'build>(name: &'build str, levels: &'build [level::Builder<'build>]) -> impl InstantiatedBuilderTrait<'build> {
    move |arena, env| {
        let decl = arena.get_binding_decl(name).ok_or(Error {
            kind: DeclarationError::UnknownDeclaration(arena.store_name(name)).into(),
        })?;
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InstantiatedBuilder<'build> {
    Instance(Box<Builder<'build>>, Vec<level::Builder<'build>>),

    Var(&'build str, Vec<level::Builder<'build>>),
}

impl<'arena> std::fmt::Display for InstantiatedBuilder<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InstantiatedBuilder::*;
        match self {
            Instance(decl, params) => { write!(f, "{decl}.{{")?;
                params.iter().try_for_each(|level| write!(f, "{level}, "))?;
                write!(f, "}}")
            }
            Var(decl, params) => { write!(f, "{decl}.{{")?;
                params.iter().try_for_each(|level| write!(f, "{level}, "))?;
                write!(f, "}}")
            }
        }
    }
}
impl<'build> InstantiatedBuilder<'build> {
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultInstantiatedDecl<'arena> {
        arena.build_instantiated_declaration(self.partial_application())
    }

    pub(in super::super) fn partial_application(&'build self) -> impl InstantiatedBuilderTrait<'build> {
        |arena, lvl_env| self.realise_in_context(arena, lvl_env)
    }

    fn realise_in_context<'arena>(
        &'build self,
        arena: &mut Arena<'arena>,
        lvl_env: &level::Environment<'build>,
    ) -> ResultInstantiatedDecl<'arena> {
        use InstantiatedBuilder::*;
        match self {
            Instance(decl, levels) => instance(decl.partial_application(), levels)(arena, lvl_env),
            Var(name, levels) => var(name, levels)(arena, lvl_env),
        }
    }
}