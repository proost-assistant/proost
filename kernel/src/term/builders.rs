use derive_more::Display;
use im_rc::hashmap::HashMap as ImHashMap;

use super::arena::{Arena, DeBruijnIndex, Term, UniverseLevel};
use crate::error::{Error, ResultTerm};

/// These functions are available publicly, to the attention of the parser. They manipulate
/// objects with a type morally equal to (Env -> Term), where Env is a working environment used
/// in term construction from the parser.
/// This is done as a way to elengantly keep the logic encapsulated in the kernel, but let the
/// parser itself explore the term.

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DefinitionError<'arena> {
    #[display(fmt = "unknown identifiant {}", _0)]
    ConstNotFound(&'arena str),
}

pub type Environment<'build, 'arena> = ImHashMap<&'build str, (DeBruijnIndex, Term<'arena>)>;

pub trait BuilderTrait<'build, 'arena> =
    FnOnce(&mut Arena<'arena>, &Environment<'build, 'arena>, DeBruijnIndex) -> ResultTerm<'arena>;

impl<'arena> Arena<'arena> {
    #[inline]
    pub fn build<'build, F: BuilderTrait<'build, 'arena>>(&mut self, f: F) -> ResultTerm<'arena>
    where
        'arena: 'build,
    {
        f(self, &Environment::new(), 0.into())
    }
}

#[inline]
pub fn var<'build, 'arena>(name: &'build str) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth| {
        env.get(name)
            .map(|(bind_depth, term)| {
                // maybe find a way to make this call efficiently lazy
                let var_type = arena.shift(*term, usize::from(depth - *bind_depth), 0);
                arena.var(depth - *bind_depth, var_type)
            })
            .or_else(|| arena.get_binding(name))
            .ok_or(Error {
                kind: DefinitionError::ConstNotFound(arena.store_name(name)).into(),
            })
    }
}

#[inline]
pub fn prop<'build, 'arena>() -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _| Ok(arena.prop())
}

#[inline]
pub fn type_<'build, 'arena>(level: UniverseLevel) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _| Ok(arena.type_(level))
}

#[inline]
pub fn type_usize<'build, 'arena>(level: usize) -> impl BuilderTrait<'build, 'arena> {
    use num_bigint::BigUint;
    move |arena: &mut Arena<'arena>, _: &Environment<'build, 'arena>, _| {
        Ok(arena.type_(BigUint::from(level).into()))
    }
}

#[inline]
pub fn app<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    u1: F1,
    u2: F2,
) -> impl BuilderTrait<'build, 'arena> {
    |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth| {
        let u1 = u1(arena, env, depth)?;
        let u2 = u2(arena, env, depth)?;
        Ok(arena.app(u1, u2))
    }
}

#[inline]
pub fn abs<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth| {
        let arg_type = arg_type(arena, env, depth)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, depth + 1.into())?;
        Ok(arena.abs(arg_type, body))
    }
}

#[inline]
pub fn prod<'build, 'arena, F1: BuilderTrait<'build, 'arena>, F2: BuilderTrait<'build, 'arena>>(
    name: &'build str,
    arg_type: F1,
    body: F2,
) -> impl BuilderTrait<'build, 'arena> {
    move |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth| {
        let arg_type = arg_type(arena, env, depth)?;
        let env = env.update(name, (depth, arg_type));
        let body = body(arena, &env, depth + 1.into())?;
        Ok(arena.prod(arg_type, body))
    }
}

#[derive(Clone)]
pub enum Builder<'r> {
    Var(&'r str),
    Type(usize),
    Prop,
    App(Box<Builder<'r>>, Box<Builder<'r>>),
    Abs(&'r str, Box<Builder<'r>>, Box<Builder<'r>>),
    Prod(&'r str, Box<Builder<'r>>, Box<Builder<'r>>),
}

impl<'build> Builder<'build> {
    pub fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> ResultTerm<'arena> {
        arena.build(self.partial_application())
    }

    fn partial_application<'arena>(&self) -> impl BuilderTrait<'build, 'arena> + '_ {
        |arena: &mut Arena<'arena>, env: &Environment<'build, 'arena>, depth| {
            self.realise_in_context(arena, env, depth)
        }
    }

    fn realise_in_context<'arena>(
        &self,
        arena: &mut Arena<'arena>,
        env: &Environment<'build, 'arena>,
        depth: DeBruijnIndex,
    ) -> ResultTerm<'arena> {
        use Builder::*;
        match *self {
            Var(s) => var(s)(arena, env, depth),
            Type(level) => type_usize(level)(arena, env, depth),
            Prop => prop()(arena, env, depth),
            App(ref l, ref r) => {
                app(l.partial_application(), r.partial_application())(arena, env, depth)
            }
            Abs(s, ref arg, ref body) => {
                abs(s, arg.partial_application(), body.partial_application())(arena, env, depth)
            }
            Prod(s, ref arg, ref body) => {
                prod(s, arg.partial_application(), body.partial_application())(arena, env, depth)
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod raw {
    use super::*;

    pub(crate) trait BuilderTrait<'arena> = FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build_raw<F: BuilderTrait<'arena>>(&mut self, f: F) -> Term<'arena> {
            f(self)
        }
    }

    impl<'arena> Term<'arena> {
        pub(crate) fn into(self) -> impl BuilderTrait<'arena> {
            move |_: &mut Arena<'arena>| self
        }
    }

    #[inline]
    pub(crate) fn var<'arena, F: BuilderTrait<'arena>>(
        index: DeBruijnIndex,
        type_: F,
    ) -> impl BuilderTrait<'arena> {
        move |env: &mut Arena<'arena>| {
            let ty = type_(env);
            env.var(index, ty)
        }
    }

    #[inline]
    pub(crate) fn prop<'arena>() -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| env.prop()
    }

    #[inline]
    pub(crate) fn type_<'arena>(level: UniverseLevel) -> impl BuilderTrait<'arena> {
        move |env: &mut Arena<'arena>| env.type_(level)
    }

    #[inline]
    pub(crate) fn app<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.app(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn abs<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.abs(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn prod<'arena, F1: BuilderTrait<'arena>, F2: BuilderTrait<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl BuilderTrait<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.prod(u1, u2)
        }
    }
}
