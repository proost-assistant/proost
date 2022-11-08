use derive_more::{Add, Display, From, Into, Sub};

use num_bigint::BigUint;

use crate::error::{Error, Result, ResultTerm};
use bumpalo::Bump;
use im_rc::hashmap::HashMap as ImHashMap;
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash,
)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Default, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord, Hash)]
pub struct UniverseLevel(BigUint);

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DefinitionError<'arena> {
    #[display(fmt = "unknown constant {}", _0)]
    ConstNotFound(&'arena str),
}

pub struct Arena<'arena> {
    alloc: &'arena Bump,
    _phantom: PhantomData<*mut &'arena ()>,

    hashcons: HashSet<&'arena Node<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,

    mem_subst: HashMap<(Term<'arena>, Term<'arena>, DeBruijnIndex), Term<'arena>>,
}

#[derive(Clone, Copy, Display, Eq, Debug)]
#[display(fmt = "{}", "_0.payload")]
// PhantomData is a marker to ensure invariance over the 'arena lifetime.
pub struct Term<'arena>(&'arena Node<'arena>, PhantomData<*mut &'arena ()>);

// no name storage here: meaning consts are known and can be found, but no pretty printing is
// possible so far.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Node<'arena> {
    payload: Payload<'arena>,
    // free_vars: an efficient type to measure whether a term is closed
    // this can be added in future iterations
    head_normal_form: OnceCell<Term<'arena>>,
    type_: OnceCell<Term<'arena>>,
}

type BinTermHashMap<'arena> = HashMap<(Term<'arena>, Term<'arena>), Term<'arena>>;
// binary maps are not used so far, because, upon building terms, no particular optimisation is
// done. On the other hand, if it is decided that only WHNF terms may exist in the arena, there can
// be an AppMap (only App is relevant, as Abs and Prod preserve WHNF).
//
// We also need a substitution hasmap, (body, level_of_substitution, Term_to_incorporate)

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    #[display(fmt = "{}", _0)]
    Var(DeBruijnIndex, Term<'arena>),

    #[display(fmt = "Prop")]
    Prop,

    #[display(fmt = "Type {}", _0)]
    Type(UniverseLevel),

    #[display(fmt = "{} {}", _0, _1)]
    App(Term<'arena>, Term<'arena>),

    #[display(fmt = "\u{003BB} {} \u{02192} {}", _0, _1)]
    Abs(Term<'arena>, Term<'arena>),

    #[display(fmt = "\u{03A0} {} \u{02192} {}", _0, _1)]
    Prod(Term<'arena>, Term<'arena>),
}

use Payload::*;

/// (TODO PRECISE DOCUMENTATION) make use of unicity invariant to speed up equality test
impl<'arena> PartialEq<Term<'arena>> for Term<'arena> {
    fn eq(&self, x: &Term<'arena>) -> bool {
        std::ptr::eq(self.0, x.0)
    }
}

/// (TODO PRECISE DOCUMENTATION) make use of unicity invariant to speed up equality test
impl<'arena> Hash for Term<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

/// (TODO PRECISE DOCUMENTATION) Only the payload matters and caracterises the value. Changing
/// OnceCells is *guaranteed* to have no impact on that.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}

pub fn use_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(Arena<'arena>) -> T,
{
    let alloc = Bump::new();
    let arena = Arena::new(&alloc);
    f(arena)
}

use intern_build::Generator;
use extern_build::Generator as ExternGenerator;

impl<'arena> Arena<'arena> {
    /// (TODO DOC.) Allocate a new memory arena.
    fn new(alloc: &'arena Bump) -> Self {
        Arena {
            alloc,
            _phantom: PhantomData,

            hashcons: HashSet::new(),
            named_terms: HashMap::new(),

            mem_subst: HashMap::new(),

        }
    }

    /// (TODO DOC.) there are concurrent designs here: either hashcons take a Node and functions
    /// which use it immediatly intend to compute the type and/or WHNF of the term, or this is
    /// postponed and computed lazily. This iteration chooses the second approach.
    fn hashcons(&mut self, n: Payload<'arena>) -> Term<'arena> {
        let nn = Node {
            payload: n,
            head_normal_form: OnceCell::new(),
            type_: OnceCell::new(),
        };
        match self.hashcons.get(&nn) {
            Some(addr) => Term(addr, PhantomData),
            None => {
                let addr = self.alloc.alloc(nn);
                self.hashcons.insert(addr);
                Term(addr, PhantomData)
            }
        }
    }

    pub(crate) fn var(&mut self, index: DeBruijnIndex, type_: Term<'arena>) -> Term<'arena> {
        self.hashcons(Var(index, type_))
    }

    pub(crate) fn prop(&mut self) -> Term<'arena> {
        self.hashcons(Prop)
    }

    pub(crate) fn type_(&mut self, level: UniverseLevel) -> Term<'arena> {
        self.hashcons(Type(level))
    }

    pub(crate) fn app(&mut self, u1: Term<'arena>, u2: Term<'arena>) -> Term<'arena> {
        self.hashcons(App(u1, u2))
    }

    pub(crate) fn abs(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Abs(arg_type, u))
    }

    pub(crate) fn prod(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Prod(arg_type, u))
    }

    pub(crate) fn build<F: Generator<'arena>>(&mut self, f: F) -> Term<'arena> {
        f(self)
    }

    pub fn build_from_extern<F: ExternGenerator<'arena>>(
        &mut self,
        f: F,
    ) -> Result<Term<'arena>> {
        f(self, &ImHashMap::new(), DeBruijnIndex(0))
    }

    /// Apply one step of β-reduction, using the leftmost-outermost evaluation strategy.
    pub fn beta_reduction(&mut self, t: Term<'arena>) -> Term<'arena> {
        match *t {
            App(t1, t2) => match *t1 {
                Abs(_, t1) => self.substitute(t1, t2, 1),
                _ => {
                    let t1 = self.beta_reduction(t1);
                    self.app(t1, t2)
                }
            },
            Abs(arg_type, body) => {
                let body = self.beta_reduction(body);
                self.abs(arg_type, body)
            }
            _ => t,
        }
    }

    pub(crate) fn shift(&mut self, t: Term<'arena>, offset: usize, depth: usize) -> Term<'arena> {
        match *t {
            Var(i, type_) if i > depth.into() => self.var(i + offset.into(), type_),
            App(t1, t2) => {
                let t1 = self.shift(t1, offset, depth);
                let t2 = self.shift(t2, offset, depth);
                self.app(t1, t2)
            },
            Abs(arg_type, body) => {
                let arg_type = self.shift(arg_type, offset, depth);
                let body = self.shift(body, offset, depth + 1);
                self.abs(arg_type, body)
            }
            Prod(arg_type, body) => {
                let arg_type = self.shift(arg_type, offset, depth);
                let body = self.shift(body, offset, depth + 1);
                self.prod(arg_type, body)
            }
            _ => t,
        }
    }

    pub(crate) fn substitute(
        &mut self,
        lhs: Term<'arena>,
        rhs: Term<'arena>,
        depth: usize,
    ) -> Term<'arena> {
        match *lhs {
            Var(i, _) if i == depth.into() => self.shift(rhs, depth - 1, 0),
            Var(i, type_) if i > depth.into() => self.var(i - 1.into(), type_),
            App(l, r) => {
                let l = self.substitute(l, rhs, depth);
                let r = self.substitute(r, rhs, depth);
                self.app(l, r)
            },
            Abs(arg_type, body) => {
                let arg_type = self.substitute(arg_type, rhs, depth);
                let body = self.substitute(body, rhs, depth + 1);
                self.abs(arg_type, body)
            },
            Prod(arg_type, body) => {
                let arg_type = self.substitute(arg_type, rhs, depth);
                let body = self.substitute(body, rhs, depth + 1);
                self.prod(arg_type, body)
            },
            _ => lhs,
        }
    }

    /// Returns the normal form of a term in a given environment.
    ///
    /// This function is comput aE0581.tionally expensive and should only be used for Reduce/Eval commands, not when type-checking.
    pub fn normal_form(&mut self, t: Term<'arena>) -> Term<'arena> {
        let mut temp = t;
        let mut res = self.beta_reduction(t);

        while res != temp {
            temp = res;
            res = self.beta_reduction(res);
        }
        res
    }

    /// Returns the weak-head normal form of a term in a given environment.
    pub fn whnf(&mut self, t: Term<'arena>) -> Term<'arena> {
        match *t {
            App(t1, t2) => {
                let t1 = self.whnf(t1);
                match *t1 {
                    Abs(_, _) => {
                        let t = self.app(t1, t2);
                        let t = self.beta_reduction(t);
                        self.whnf(t)
                    },
                    _ => t,
                }
            }
            _ => t,
        }
    }
}

impl<'arena> Term<'arena> {
    pub(crate) fn into(self) -> impl Generator<'arena>
        { move |_: &mut Arena<'arena>| self }

    pub fn is_redex(self) -> bool {
        match *self {
            App(t, _) => match *t {
                Abs(_, _) => true,
                _ => false
            }
            _ => false
        }
    }

    pub fn get_type_or_try_init<F>(self, f: F) -> ResultTerm<'arena>
        where F: FnOnce() -> ResultTerm<'arena> {
        self.0.type_.get_or_try_init(f).copied()
    }
}

impl<'arena> Deref for Term<'arena> {
    type Target = Payload<'arena>;

    fn deref(&self) -> &Self::Target {
        &self.0.payload
    }
}

pub(crate) mod intern_build {
    use super::*;

    pub(crate) trait Generator<'arena> = FnOnce(&mut Arena<'arena>) -> Term<'arena>;

    pub fn prop<'arena>() -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| env.prop()
    }

    pub fn type_<'arena>(level: UniverseLevel) -> impl Generator<'arena> {
        move |env: &mut Arena<'arena>| env.type_(level)
    }

    pub fn app<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.app(u1, u2)
        }
    }

    pub fn abs<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.abs(u1, u2)
        }
    }

    pub fn prod<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
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
pub mod extern_build {
    use super::*;

    pub type Environment<'arena> = ImHashMap<&'arena str, (DeBruijnIndex, Term<'arena>)>;
pub trait Generator<'arena> = FnOnce(
    &mut Arena<'arena>,
    &Environment<'arena>,
    DeBruijnIndex,
) -> ResultTerm<'arena>;

    #[inline]
    pub fn var<'arena>(name: &'arena str) -> impl Generator<'arena> {
        move |context: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            env.get(name)
                .map(|(bind_depth, term)| context.var(depth - *bind_depth, *term))
                .or_else(|| context.named_terms.get(name).copied())
                .ok_or(Error { kind: DefinitionError::ConstNotFound(name).into() })
        }
    }

    #[inline]
    pub fn prop<'arena>() -> impl Generator<'arena> {
        |context: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(context.prop())
    }

    #[inline]
    pub fn type_<'arena>(level: UniverseLevel) -> impl Generator<'arena> {
        move |context: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(context.type_(level))
    }

    #[inline]
    pub fn app<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |context: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let u1 = u1(context, env, depth)?;
            let u2 = u2(context, env, depth)?;
            Ok(context.app(u1, u2))
        }
    }

    #[inline]
    pub fn abs<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Generator<'arena> {
        move |context: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(context, env, depth)?;
            let env = env.update(name, (depth + DeBruijnIndex(1), arg_type));
            let body = body(context, &env, depth)?;
            Ok(context.abs(arg_type, body))
        }
    }

    #[inline]
    pub fn prod<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Generator<'arena> {
        move |context: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(context, env, depth)?;
            let env = env.update(name, (depth + DeBruijnIndex(1), arg_type));
            let body = body(context, &env, depth)?;
            Ok(context.prod(arg_type, body))
        }
    }

    pub fn extern_gen<F: ExternTermGenerator<'arena>>(f: F) -> Result<Term<'arena>, KernelError<'arena>> {
        f(DeBruijnIndex(0), &ImHashMap::new())
    }


    /// Apply one step of β-reduction, using leftmost outermost evaluation strategy.
    pub fn beta_reduction(&'arena mut self, t: Term<'arena>) -> Term<'arena> {
        match t.into() {
            App(t1, t2) => match t1.into() {
                Abs(_, t1) => self.substitute(t1, t2, 1),
                _ => {
                    let t1_new = self.beta_reduction(t1);
                    if t1_new == t1 {
                        let t2_new = self.beta_reduction(t2);
                        self.app(t1, t2_new)
                    } else {
                        self.app(t1_new, t2)
                    }
                }
            }
            Abs(arg_type, body) => self.abs(arg_type, self.beta_reduction(body)),
            Prod(arg_type, body) => self.prod(arg_type, self.beta_reduction(body)),
            _ => t,
        }
    }

    pub(crate) fn shift(&'arena mut self, t: Term<'arena>, offset: usize, depth: usize) -> Term<'arena> {
        match t.into() {
            Var(i, type_) if i > depth.into() => self.var(i + offset.into(), type_),
            App(t1, t2) => self.app(self.shift(t1, offset, depth), self.shift(t2, offset, depth)),
            Abs(arg_type, body) => self.abs(arg_type, self.shift(body, offset, depth + 1)),
            Prod(arg_type, body) => self.prod(arg_type, self.shift(body, offset, depth + 1)),
            _ => t,
        }
    }

    pub(crate) fn substitute(&'arena mut self, lhs: Term<'arena>, rhs: Term<'arena>, depth: usize) -> Term<'arena> {
        match lhs.into() {
            Var(i, type_) if i == depth.into() => self.shift(rhs, depth - 1, 0),
            Var(i, type_) if i > depth.into() => self.var(i - 1.into(), type_),
            App(l, r) => self.app(self.substitute(l, rhs, depth), self.substitute(r, rhs, depth)),
            Abs(arg_type, body) => self.abs(arg_type /* really? TEST */, self.substitute(body, rhs, depth + 1)),
            Prod(arg_type, body) => self.prod(arg_type /* really? TEST */, self.substitute(body, rhs, depth + 1)),
            _ => lhs,
        }
    }

    /// Returns the normal form of a term in a given environment.
    ///
    /// This function is computationally expensive and should only be used for Reduce/Eval commands, not when type-checking.
    pub fn normal_form(&'arena mut self, t: Term<'arena>) -> Term<'arena> {
        let mut temp = t;
        let mut res = self.beta_reduction(t);

        while res != temp {
            temp = res;
            res = self.beta_reduction(res);
        }
        res
    }

    /// Returns the weak-head normal form of a term in a given environment.
    pub fn whnf(&'arena mut self, t: Term<'arena>) -> Term<'arena> {
        match t.into() {
            App(t1, t2) => {
                let t1 = self.whnf(t1);
                match t1.into() {
                    Abs(_, _) => self.whnf(self.beta_reduction(self.app(t1, t2))),
                    _ => t
                }
            }
            _ => t,
        }
    }
}
/*
#[cfg(test)]
mod tests {
    // /!\ most of these tests are on ill-typed terms and should not be used for further testings
    use super::*;

    #[test]
    fn simple_subst() {
        // λx.(λy.x y) x
        let term = Abs(
            box Prop,
            box App(
                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                box Var(1.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(1.into()), box Var(1.into())));

        assert_eq!(term.beta_reduction(&Environment::new()), reduced);
    }

    #[test]
    fn complex_subst() {
        // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
        let term = App(
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(
                        box Prop,
                        box App(
                            box App(
                                box App(
                                    box Var(3.into()),
                                    box Abs(
                                        box Prop,
                                        box Abs(
                                            box Prop,
                                            box App(
                                                box Var(1.into()),
                                                box App(box Var(2.into()), box Var(4.into())),
                                            ),
                                        ),
                                    ),
                                ),
                                box Abs(box Prop, box Var(2.into())),
                            ),
                            box Abs(box Prop, box Var(1.into())),
                        ),
                    ),
                ),
            ),
            box Abs(
                box Prop,
                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
            ),
        );

        let term_step_1 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box App(
                            box Abs(
                                box Prop,
                                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                            ),
                            box Abs(
                                box Prop,
                                box Abs(
                                    box Prop,
                                    box App(
                                        box Var(1.into()),
                                        box App(box Var(2.into()), box Var(4.into())),
                                    ),
                                ),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_2 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box Abs(
                            box Prop,
                            box App(
                                box Abs(
                                    box Prop,
                                    box Abs(
                                        box Prop,
                                        box App(
                                            box Var(1.into()),
                                            box App(box Var(2.into()), box Var(5.into())),
                                        ),
                                    ),
                                ),
                                box Var(1.into()),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_3 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box Abs(
                            box Prop,
                            box Abs(
                                box Prop,
                                box App(
                                    box Var(1.into()),
                                    box App(box Var(2.into()), box Var(4.into())),
                                ),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_4 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(
                        box Prop,
                        box App(
                            box Var(1.into()),
                            box App(box Abs(box Prop, box Var(3.into())), box Var(3.into())),
                        ),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_5 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(box Prop, box Var(1.into())),
                    box App(box Abs(box Prop, box Var(2.into())), box Var(2.into())),
                ),
            ),
        );

        let term_step_6 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(box Abs(box Prop, box Var(2.into())), box Var(2.into())),
            ),
        );

        // λa.λb.b
        let term_step_7 = Abs(box Prop, box Abs(box Prop, box Var(1.into())));

        let env = Environment::new();

        assert_eq!(term.beta_reduction(&env), term_step_1);
        assert_eq!(term_step_1.beta_reduction(&env), term_step_2);
        assert_eq!(term_step_2.beta_reduction(&env), term_step_3);
        assert_eq!(term_step_3.beta_reduction(&env), term_step_4);
        assert_eq!(term_step_4.beta_reduction(&env), term_step_5);
        assert_eq!(term_step_5.beta_reduction(&env), term_step_6);
        assert_eq!(term_step_6.beta_reduction(&env), term_step_7);
        assert_eq!(term_step_7.beta_reduction(&env), term_step_7);
    }

    #[test]
    fn shift_prod() {
        let reduced = Prod(box Prop, box Var(1.into()));
        let term = App(box Abs(box Prop, box reduced.clone()), box Prop);

        assert_eq!(term.beta_reduction(&Environment::new()), reduced)
    }

    #[test]
    fn const_subst() {
        let id_prop = Prod(box Prop, box Prod(box Var(1.into()), box Var(1.into())));

        let mut env = Environment::new();
        env.insert("foo".into(), id_prop.clone(), Prop).unwrap();

        assert_eq!(Const("foo".into()).beta_reduction(&env), id_prop);

    }

    #[test]
    fn prod_beta_red() {
        let term = Prod(
            box Prop,
            box App(box Abs(box Prop, box Var(1.into())), box Var(1.into())),
        );
        let reduced = Prod(box Prop, box Var(1.into()));

        assert_eq!(term.beta_reduction(&Environment::new()), reduced);
    }

    #[test]
    fn app_red_rhs() {
        let term = Abs(
            box Prop,
            box App(
                box Var(1.into()),
                box App(box Abs(box Prop, box Var(1.into())), box Var(1.into())),
            ),
        );
        let reduced = Abs(box Prop, box App(box Var(1.into()), box Var(1.into())));

        assert_eq!(term.beta_reduction(&Environment::new()), reduced);
    }
}
    }*/
