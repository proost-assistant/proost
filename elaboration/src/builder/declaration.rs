//! Builder types for [`Declaration`]s. and [`InstantiatedDeclaration`]s.
//!
//! This is a naive description of declarations. It can be transformed into concrete declarations
//! through the [`BuiderTrait`](kernel::memory::declaration::builder::BuilderTrait) declared in
//! the kernel.

use derive_more::Display;
use kernel::error::{ResultDecl, ResultInstantiatedDecl};
use kernel::memory::arena::Arena;
use kernel::memory::declaration::builder::{declaration, instance, var, BuilderTrait, InstantiatedBuilderTrait};
use kernel::memory::declaration::{Declaration, InstantiatedDeclaration};
use kernel::trace::{Trace, Traceable};

use super::Buildable;
use crate::builder::{level, term};
use crate::location::Location;

/// Template of declarations.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum Builder<'build> {
    #[display(fmt = "{_0}")]
    Decl(Box<term::Builder<'build>>, Vec<&'build str>),
}

impl<'build> Traceable<Location> for Builder<'build> {
    #[inline]
    fn apply_trace(&self, trace: &[Trace]) -> Location {
        match *self {
            Builder::Decl(ref term, _) => term.apply_trace(trace),
        }
    }
}

impl<'build> Buildable<'build> for Builder<'build> {
    type Output<'arena> = Declaration<'arena>;

    type Closure = impl BuilderTrait<'build>;

    /// Realise a builder into a [`Declaration`]. This internally uses functions described in
    /// the [builder](`crate::memory::declaration::builder`) module.
    ///
    /// # Errors
    /// If the declaration could not be built, yields an error indicating the reason
    #[inline]
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultDecl<'arena> {
        arena.build_declaration(self.as_closure())
    }

    /// Associates a builder to a builder trait.
    #[inline]
    fn as_closure(&'build self) -> Self::Closure {
        |arena| match *self {
            Builder::Decl(ref term, ref vars) => declaration(term.as_closure(), vars.as_slice())(arena),
        }
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

impl<'build> Buildable<'build> for InstantiatedBuilder<'build> {
    type Output<'arena> = InstantiatedDeclaration<'arena>;

    type Closure = impl InstantiatedBuilderTrait<'build>;

    /// Realise a builder into an [`InstantiatedDeclaration`]. This internally uses functions described in
    /// the [builder](`crate::memory::declaration::builder`) module.
    ///
    /// # Errors
    /// If the instantiated declaration could not be built, yields an error indicating the reason
    #[inline]
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultInstantiatedDecl<'arena> {
        arena.build_instantiated_declaration(self.as_closure())
    }

    /// Associates a builder to a builder trait.
    #[inline]
    fn as_closure(&'build self) -> Self::Closure {
        |arena, lvl_env| match *self {
            InstantiatedBuilder::Instance(ref decl, ref levels) => instance(decl.as_closure(), levels.as_closure())(arena, lvl_env),
            InstantiatedBuilder::Var(name, ref levels) => var(name, levels.as_closure())(arena, lvl_env),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::term::{Builder, Payload};
    use super::Builder::Decl;
    use super::*;

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
