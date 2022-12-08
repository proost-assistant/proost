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

use std::collections::HashSet;

use derive_more::Display;
use im_rc::hashmap::HashMap as ImHashMap;

use super::arena::{Arena, DeBruijnIndex, Namespace, NamespaceSet, Term, UniverseLevel};
use crate::error::{Error, ResultTerm};

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DefinitionError<'arena> {
    #[display(fmt = "unknown identifier {}", _0)]
    ConstNotFound(Namespace<'arena>),
    #[display(fmt = "identifier may ambiguously refer to: {}", _0)]
    AmbiguousIdentifier(NamespaceSet<'arena>),
}

/// Local environment used to store correspondence between locally-bound variables and the pair
/// (depth at which they were bound, their type)
pub type Environment<'build, 'arena> = ImHashMap<&'build str, (DeBruijnIndex, Term<'arena>)>;

/// Struct containing all the information needed to realize a Builder in a given module context
/// This struct should not be used outside out of the kernel. It was made public not to leak private types.
#[derive(Copy, Clone)]
pub struct ModuleContext<'a> {
    /// Stack of currently opened module
    pub module_stack: &'a Vec<String>,
    /// Modules used in the context
    pub used_modules: &'a Vec<HashSet<Vec<String>>>,
    /// Vars used in the context
    pub used_vars: &'a Vec<HashSet<String>>,
}

/// The trait of closures which build terms with an adequate logic.
///
/// A call with a triplet of arguments `(arena, env, index)` of a closure with this trait should
/// build a definite term in the [`Arena`] `arena`, knowing the bindings declared in `environment`,
/// provided that the term is built at a current depth `index`.
///
/// Please note that this is just a trait alias, meaning it enforces very little constraints: while
/// functions in this module returning a closure with this trait are guaranteed to be sound, end
/// users can also create their own closures satisfying `BuilderTrait`; this should be avoided.
pub trait BuilderTrait<'build, 'arena> =
    FnOnce(&mut Arena<'arena>, &Environment<'build, 'arena>, DeBruijnIndex, ModuleContext<'build>) -> ResultTerm<'arena>;

impl<'arena> Arena<'arena> {
    /// Returns the term built from the given closure, provided with an empty context, at depth 0.
    #[inline]
    pub fn build<'build, F: BuilderTrait<'build, 'arena>>(&mut self, f: F, mod_ctx: ModuleContext<'build>) -> ResultTerm<'arena>
    where
        'arena: 'build,
    {
        f(self, &Environment::new(), 0.into(), mod_ctx)
    }
}

fn test_binding_in_module<'arena>(
    arena: &mut Arena<'arena>,
    module: &Vec<String>,
    name: &Vec<&str>,
) -> Option<(Namespace<'arena>, Term<'arena>)> {
    let full_name = module.iter().map(String::as_str).chain(name.into_iter().copied()).collect::<Vec<&str>>();
    arena.get_binding_with_name(&full_name)
}

// for the `super` keyword, you might want to use a function that takes (name, stack) and returns
// (name without super:: prefixes, corresponding stack)

/// Returns a closure building a variable associated to the name `name`
#[inline]
pub const fn var<'build, 'arena>(name: &'build Vec<&'build str>) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth, mod_ctx| {
        if name.len() == 1 && let Some((bind_depth, term)) = env.get(&name[0]) {
                // This is arguably an eager computation, it could be worth making it lazy,
                // or at least memoizing it so as to not compute it again
                let var_type = arena.shift(*term, usize::from(depth - *bind_depth), 0);
                return Ok(arena.var(depth - *bind_depth, var_type))
        }

        let candidates = mod_ctx.used_modules.last().into_iter().flatten().chain(std::iter::once(mod_ctx.module_stack));
        let results = candidates.filter_map(|prefix| test_binding_in_module(arena, prefix, name)).collect::<Vec<_>>();

        if results.len() == 0 {
            Err(Error {
                kind: DefinitionError::ConstNotFound(arena.store_name_1(name)).into(),
            })
        } else if results.len() == 1 {
            Ok(results[0].1)
        } else {
            let results: Vec<Namespace> = results.into_iter().map(|(n, _)| n).collect();
            Err(Error {
                kind: DefinitionError::AmbiguousIdentifier(results.into()).into(),
            })
        }
        // .or_else(|| test_binding_in_module(arena, mod_ctx.module_stack, name).into_iter().collect::<Option<Vec<Term<'arena>>>>())
        // .or_else(|| {
        //     mod_ctx.used_modules.iter().next_back()?.iter().map(|module| test_binding_in_module(arena, module,
        // name)).collect::<Option<Vec<Term<'arena>>>>() })
        // .ok_or(Error {
        //     kind: DefinitionError::ConstNotFound(arena.store_name_1(name)).into(),
        // })
    }
}

/// Returns a closure building the Prop term.
#[inline]
pub const fn prop<'build, 'arena>() -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _, _| Ok(arena.prop())
}

/// Returns a closure building the Type `level` term.
#[inline]
pub const fn type_<'build, 'arena>(level: UniverseLevel) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _, _| Ok(arena.type_(level))
}

/// Returns a closure building the Type `level` term (indirection from `usize`).
#[inline]
pub const fn type_usize<'build, 'arena>(level: usize) -> impl BuilderTrait<'build, 'arena> {
    use num_bigint::BigUint;
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _, _| Ok(arena.type_(BigUint::from(level).into()))
}

/// Returns a closure building the application of two terms built from the given closures `u1` and
/// `u2`.
#[inline]
#[no_coverage]
pub const fn app<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    u1: F1,
    u2: F2,
) -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth, mod_ctx| {
        let u1 = u1(arena, env, depth, mod_ctx)?;
        let u2 = u2(arena, env, depth, mod_ctx)?;
        Ok(arena.app(u1, u2))
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
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth, mod_ctx: ModuleContext| {
        let arg_type = arg_type(arena, env, depth, mod_ctx)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, depth + 1.into(), mod_ctx)?;
        Ok(arena.abs(arg_type, body))
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
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth, mod_ctx: ModuleContext| {
        let arg_type = arg_type(arena, env, depth, mod_ctx)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, depth + 1.into(), mod_ctx)?;
        Ok(arena.prod(arg_type, body))
    }
}

/// Template of terms.
///
/// A Builder describes a term in a naive but easy to build manner. It strongly resembles the
/// [payload](`crate::term::arena::Payload`) type, except that `Var`, `Abs` and `Prod` constructors
/// include a name, as in the classic way of writing lambda-terms (i.e. no de Bruijn indices
/// involved).
#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Builder<'r> {
    #[display(fmt = "{}", "_0.join(\"::\")")]
    Var(Vec<&'r str>),

    #[display(fmt = "Prop")]
    Prop,

    #[display(fmt = "Type {}", _0)]
    Type(usize),

    #[display(fmt = "{} {}", _0, _1)]
    App(Box<Builder<'r>>, Box<Builder<'r>>),

    #[display(fmt = "\u{003BB} {}: {} \u{02192} {}", _0, _1, _2)]
    Abs(&'r str, Box<Builder<'r>>, Box<Builder<'r>>),

    #[display(fmt = "\u{03A0} {}: {} \u{02192} {}", _0, _1, _2)]
    Prod(&'r str, Box<Builder<'r>>, Box<Builder<'r>>),
}

impl<'build> Builder<'build> {
    /// Build a terms from a [`Builder`]. This internally uses functions described in the
    /// [builders](`crate::term::builders`) module.
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>, mod_ctx: ModuleContext) -> ResultTerm<'arena> {
        arena.build(self.partial_application(), mod_ctx)
    }

    fn partial_application<'arena>(&'build self) -> impl BuilderTrait<'build, 'arena> + '_ {
        |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth, mod_ctx: ModuleContext| {
            self.realise_in_context(arena, env, depth, mod_ctx)
        }
    }

    fn realise_in_context<'arena>(
        &'build self,
        arena: &mut Arena<'arena>,
        env: &Environment<'build, 'arena>,
        depth: DeBruijnIndex,
        mod_ctx: ModuleContext<'build>,
    ) -> ResultTerm<'arena> {
        use Builder::*;
        match self {
            Var(vec) => var(vec)(arena, env, depth, mod_ctx),
            Type(level) => type_usize(*level)(arena, env, depth, mod_ctx),
            Prop => prop()(arena, env, depth, mod_ctx),
            App(ref l, ref r) => app(l.partial_application(), r.partial_application())(arena, env, depth, mod_ctx),
            Abs(s, ref arg, ref body) => abs(s, arg.partial_application(), body.partial_application())(arena, env, depth, mod_ctx),
            Prod(s, ref arg, ref body) => {
                prod(s, arg.partial_application(), body.partial_application())(arena, env, depth, mod_ctx)
            },
        }
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub trait BuilderTrait<'arena> = FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_raw<F: BuilderTrait<'arena>>(&mut self, f: F) -> Term<'arena> {
            f(self)
        }
    }

    impl<'arena> Term<'arena> {
        pub(crate) const fn into(self) -> impl BuilderTrait<'arena> {
            move |_: &mut Arena<'arena>| self
        }
    }

    pub const fn var<'arena, F: BuilderTrait<'arena>>(index: DeBruijnIndex, type_: F) -> impl BuilderTrait<'arena> {
        move |env: &mut Arena<'arena>| {
            let ty = type_(env);
            env.var(index, ty)
        }
    }

    pub const fn prop<'arena>() -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| env.prop()
    }

    pub const fn type_<'arena>(level: UniverseLevel) -> impl BuilderTrait<'arena> {
        move |env: &mut Arena<'arena>| env.type_(level)
    }

    pub const fn app<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.app(u1, u2)
        }
    }

    pub const fn abs<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.abs(u1, u2)
        }
    }

    pub const fn prod<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(u1: F1, u2: F2) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.prod(u1, u2)
        }
    }
}
