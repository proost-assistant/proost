//! Builder types for [`Levels`]s.
//!
//! This is a naive description of declarations. It can be transformed into concrete declarations
//! through the [`BuiderTrait`](kernel::memory::level::builder::BuilderTrait) declared in
//! the kernel.

use derive_more::Display;
use kernel::error::{Result, ResultLevel};
use kernel::memory::arena::Arena;
use kernel::memory::level::builder::{const_, imax, max, plus, succ, var, zero, BuilderTrait, ErrorKind, VecBuilderTrait};
use kernel::memory::level::Level;

use super::Buildable;

/// Limit of universe level
static UNIVERSE_LIMIT: usize = 15;

/// Template of levels.
///
/// A Builder describes a term in a naive but easy-to-build manner.
///
/// Please refer to the item descriptions in [levels](crate::memory::level::Payload) for a
/// description of the corresponding items. Please understand that there are still differences,
///
/// Because the purpose of a builder is to provide an easy way to build terms, even through the
/// API, it offers different ways to build some terms, for convenience.
///
/// Unlike [`Term` builders](crate::memory::term::builder::Builder), level builders do not back-propagate
/// a trace when an error occurs. This makes their structure simpler, but also limits the accuracy of the
/// error reports associated to them. This is not an issue, as levels typically have a very limited size.
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
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
    fn max_depth(&self) -> usize {
        match self {
            Builder::Zero => 0,
            Builder::Const(c) => *c,
            Builder::Plus(u, n) => u.max_depth() + n,
            Builder::Succ(u) => u.max_depth() + 1,
            Builder::Max(u, v) => u.max_depth().max(v.max_depth()),
            Builder::IMax(u, v) => u.max_depth().max(v.max_depth()),
            Builder::Var(_) => 0,
        }
    }
}

impl<'build> Buildable<'build> for Builder<'build> {
    type Output<'arena> = Level<'arena>;

    type Closure = impl BuilderTrait<'build>;

    /// Realise a builder into a [`Level`]. This internally uses functions described in
    /// the [builder](`crate::memory::level::builder`) module.
    ///
    /// # Errors
    /// If the level could not be built, yields an error indicating the reason.
    #[inline]
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultLevel<'arena> {
        arena.build_level(self.as_closure())
    }

    /// Associates a builder to a builder trait.
    #[inline]
    fn as_closure(&'build self) -> Self::Closure {
        |arena, env| {
            let depth = self.max_depth();

            if depth > UNIVERSE_LIMIT {
                return Err(kernel::error::Error {
                    kind: kernel::error::Kind::Level(ErrorKind::UniverseTooLarge(depth)),
                    trace: vec![],
                });
            }

            match *self {
                Builder::Zero => zero()(arena, env),
                Builder::Const(c) => const_(c)(arena, env),
                Builder::Plus(ref u, n) => plus(u.as_closure(), n)(arena, env),
                Builder::Succ(ref l) => succ(l.as_closure())(arena, env),
                Builder::Max(ref l, ref r) => max(l.as_closure(), r.as_closure())(arena, env),
                Builder::IMax(ref l, ref r) => imax(l.as_closure(), r.as_closure())(arena, env),
                Builder::Var(s) => var(s)(arena, env),
            }
        }
    }
}

impl<'build> Buildable<'build> for Vec<Builder<'build>> {
    type Output<'arena> = Vec<Level<'arena>>;

    type Closure = impl VecBuilderTrait<'build>;

    /// Realise a vector of builders into a `Vec<Level>`. This internally uses functions described
    /// in the [builder](`crate::memory::level::builder`) module.
    ///
    /// # Errors
    /// If the level could not be built, yields an error indicating the reason.
    #[inline]
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> Result<'arena, Vec<Level<'arena>>> {
        arena.build_vec_level(self.as_closure())
    }

    /// Associates a builder to a builder trait.
    #[inline]
    fn as_closure(&'build self) -> Self::Closure {
        |arena, env| {
            self.iter()
                .map(|level_builder| level_builder.as_closure()(arena, env))
                .collect::<Result<Vec<_>>>()
        }
    }
}
