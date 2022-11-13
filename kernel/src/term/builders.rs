use super::arena::{Arena, DeBruijnIndex, Term, UniverseLevel};

#[cfg(test)]
pub(crate) mod intern {
    use super::*;

    impl<'arena> Arena<'arena> {
        pub(crate) fn build<F: Builder<'arena>>(&mut self, f: F) -> Term<'arena> {
            f(self)
        }
    }

    impl<'arena> Term<'arena> {
        pub(crate) fn into(self) -> impl Builder<'arena> {
            move |_: &mut Arena<'arena>| self
        }
    }

    pub(crate) trait Builder<'arena> = FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    #[inline]
    pub(crate) fn prop<'arena>() -> impl Builder<'arena> {
        |env: &mut Arena<'arena>| env.prop()
    }

    #[inline]
    pub(crate) fn type_<'arena>(level: UniverseLevel) -> impl Builder<'arena> {
        move |env: &mut Arena<'arena>| env.type_(level)
    }

    #[inline]
    pub(crate) fn var<'arena, F: Builder<'arena>>(
        index: DeBruijnIndex,
        type_: F,
    ) -> impl Builder<'arena> {
        move |env: &mut Arena<'arena>| {
            let ty = type_(env);
            env.var(index, ty)
        }
    }

    #[inline]
    pub(crate) fn app<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Builder<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.app(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn abs<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Builder<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.abs(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn prod<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Builder<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.prod(u1, u2)
        }
    }
}

/// These functions are available publicly, to the attention of the parser. They manipulate
/// objects with a type morally equal to (Env -> Term), where Env is a working environment used
/// in term construction from the parser.
/// This is done as a way to elengantly keep the logic encapsulated in the kernel, but let the
/// parser itself explore the term.
pub mod extern_ {
    use derive_more::Display;
    use im_rc::hashmap::HashMap as ImHashMap;

    use super::*;
    use crate::error::{Error, ResultTerm};

    #[non_exhaustive]
    #[derive(Clone, Debug, Display, Eq, PartialEq)]
    pub enum DefinitionError<'arena> {
        #[display(fmt = "unknown identifiant {}", _0)]
        ConstNotFound(&'arena str),
    }

    pub type Environment<'arena> = ImHashMap<&'arena str, (DeBruijnIndex, Term<'arena>)>;
    pub trait Builder<'arena> =
        FnOnce(&mut Arena<'arena>, &Environment<'arena>, DeBruijnIndex) -> ResultTerm<'arena>;

    #[inline]
    pub fn var<'arena>(name: &'arena str) -> impl Builder<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            env.get(name)
                .map(|(bind_depth, term)| {
                    // maybe find a way to make this call efficiently lazy
                    let var_type = arena.shift(*term, usize::from(depth - *bind_depth), 0);
                    arena.var(depth - *bind_depth, var_type)
                })
                .or_else(|| arena.get_binding(name))
                .ok_or(Error {
                    kind: DefinitionError::ConstNotFound(name).into(),
                })
        }
    }

    #[inline]
    pub fn prop<'arena>() -> impl Builder<'arena> {
        |arena: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(arena.prop())
    }

    #[inline]
    pub fn type_<'arena>(level: UniverseLevel) -> impl Builder<'arena> {
        move |arena: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(arena.type_(level))
    }

    #[inline]
    pub fn app<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Builder<'arena> {
        |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let u1 = u1(arena, env, depth)?;
            let u2 = u2(arena, env, depth)?;
            Ok(arena.app(u1, u2))
        }
    }

    #[inline]
    pub fn abs<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Builder<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(arena, env, depth)?;
            let env = env.update(name, (depth, arg_type));
            let body = body(arena, &env, depth + 1.into())?;
            Ok(arena.abs(arg_type, body))
        }
    }

    #[inline]
    pub fn prod<'arena, F1: Builder<'arena>, F2: Builder<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Builder<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(arena, env, depth)?;
            let env = env.update(name, (depth, arg_type));
            let body = body(arena, &env, depth + 1.into())?;
            Ok(arena.prod(arg_type, body))
        }
    }
}
