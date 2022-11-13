use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use bumpalo::Bump;
use derive_more::{Add, Display, From, Into, Sub};
use im_rc::hashmap::HashMap as ImHashMap;
use num_bigint::BigUint;

use crate::error::{Error, ResultTerm};

#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash,
)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Default, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord, Hash)]
pub struct UniverseLevel(BigUint);

#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DefinitionError<'arena> {
    #[display(fmt = "unknown identifiant {}", _0)]
    ConstNotFound(&'arena str),
}

pub struct Arena<'arena> {
    alloc: &'arena Bump,
    _phantom: PhantomData<*mut &'arena ()>,

    hashcons: HashSet<&'arena Node<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,

    mem_subst: HashMap<(Term<'arena>, Term<'arena>, DeBruijnIndex), Term<'arena>>,
    // a shift hashmap may also be added when the is_certainly_closed also is
}

#[derive(Clone, Copy, Display, Eq)]
#[display(fmt = "{}", "_0.payload")]
// PhantomData is a marker to ensure invariance over the 'arena lifetime.
pub struct Term<'arena>(&'arena Node<'arena>, PhantomData<*mut &'arena ()>);

// the rest of the struct is very verbose and useless for debugging
impl<'arena> Debug for Term<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.payload.fmt(f)
    }
}

// no name storage here: meaning consts are known and can be found, but no pretty printing is
// possible so far.
#[derive(Debug)]
struct Node<'arena> {
    payload: Payload<'arena>,

    head_normal_form: OnceCell<Term<'arena>>,
    type_: OnceCell<Term<'arena>>,
    //
    // is_certainly_closed: boolean underapproximation of whether a term is closed, which can
    // greatly improve performance in shifting
}

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

impl<'arena> PartialEq<Node<'arena>> for Node<'arena> {
    fn eq(&self, x: &Node<'arena>) -> bool {
        self.payload == x.payload
    }
}

impl<'arena> Eq for Node<'arena> {}

/// (TODO PRECISE DOCUMENTATION) Only the payload matters and caracterises the value. Changing
/// OnceCells is *guaranteed* to have no impact on that.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}

pub fn use_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    let alloc = Bump::new();
    let mut arena = Arena::new(&alloc);
    f(&mut arena)
}

use extern_build::Generator as ExternGenerator;
use intern_build::Generator;

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

    pub(crate) fn type_usize(&mut self, level: usize) -> Term<'arena> {
        self.hashcons(Type(BigUint::from(level).into()))
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

    #[inline]
    pub fn build_from_extern<F: ExternGenerator<'arena>>(&mut self, f: F) -> ResultTerm<'arena> {
        f(self, &ImHashMap::new(), 0.into())
    }

    pub fn bind(&mut self, name: &'arena str, t: Term<'arena>) {
        self.named_terms.insert(name, t);
    }

    /// Apply one step of β-reduction, using the leftmost-outermost evaluation strategy.
    pub fn beta_reduction(&mut self, t: Term<'arena>) -> Term<'arena> {
        match *t {
            App(t1, t2) => match *t1 {
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
            },
            Abs(arg_type, body) => {
                let body = self.beta_reduction(body);
                self.abs(arg_type, body)
            }
            Prod(arg_type, body) => {
                let body = self.beta_reduction(body);
                self.prod(arg_type, body)
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
            }
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
        match self.mem_subst.get(&(lhs, rhs, depth.into())) {
            Some(res) => *res,
            None => {
                let res = match *lhs {
                    Var(i, _) if i == depth.into() => self.shift(rhs, depth - 1, 0),
                    Var(i, type_) if i > depth.into() => self.var(i - 1.into(), type_),
                    App(l, r) => {
                        let l = self.substitute(l, rhs, depth);
                        let r = self.substitute(r, rhs, depth);
                        self.app(l, r)
                    }
                    Abs(arg_type, body) => {
                        let arg_type = self.substitute(arg_type, rhs, depth);
                        let body = self.substitute(body, rhs, depth + 1);
                        self.abs(arg_type, body)
                    }
                    Prod(arg_type, body) => {
                        let arg_type = self.substitute(arg_type, rhs, depth);
                        let body = self.substitute(body, rhs, depth + 1);
                        self.prod(arg_type, body)
                    }
                    _ => lhs,
                };
                self.mem_subst.insert((lhs, rhs, depth.into()), res);
                res
            }
        }
    }

    /// Returns the normal form of a term in a given environment.
    ///
    /// This function is computationally expensive and should only be used for Reduce/Eval commands, not when type-checking.
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
        t.get_whnf_or_init(|| match *t {
            App(t1, t2) => {
                let t1 = self.whnf(t1);
                match *t1 {
                    Abs(_, _) => {
                        let t = self.app(t1, t2);
                        let t = self.beta_reduction(t);
                        self.whnf(t)
                    }
                    _ => t,
                }
            }
            _ => t,
        })
    }
}

impl<'arena> Term<'arena> {
    pub(crate) fn into(self) -> impl Generator<'arena> {
        move |_: &mut Arena<'arena>| self
    }

    pub(crate) fn get_whnf_or_init<F>(self, f: F) -> Term<'arena>
    where
        F: FnOnce() -> Term<'arena>,
    {
        *self.0.head_normal_form.get_or_init(f)
    }

    pub(crate) fn get_type_or_try_init<F>(self, f: F) -> ResultTerm<'arena>
    where
        F: FnOnce() -> ResultTerm<'arena>,
    {
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

    #[inline]
    pub(crate) fn prop<'arena>() -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| env.prop()
    }

    #[inline]
    pub(crate) fn type_<'arena>(level: UniverseLevel) -> impl Generator<'arena> {
        move |env: &mut Arena<'arena>| env.type_(level)
    }

    pub(crate) fn var<'arena, F: Generator<'arena>>(
        index: DeBruijnIndex,
        type_: F,
    ) -> impl Generator<'arena> {
        move |env: &mut Arena<'arena>| {
            let ty = type_(env);
            env.var(index, ty)
        }
    }

    #[inline]
    pub(crate) fn app<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.app(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn abs<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |env: &mut Arena<'arena>| {
            let u1 = u1(env);
            let u2 = u2(env);
            env.abs(u1, u2)
        }
    }

    #[inline]
    pub(crate) fn prod<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
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
    pub trait Generator<'arena> =
        FnOnce(&mut Arena<'arena>, &Environment<'arena>, DeBruijnIndex) -> ResultTerm<'arena>;

    #[inline]
    pub fn var<'arena>(name: &'arena str) -> impl Generator<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            env.get(name)
                .map(|(bind_depth, term)| {
                    // maybe find a way to make this call efficiently lazy
                    let var_type = arena.shift(*term, usize::from(depth - *bind_depth), 0);
                    arena.var(depth - *bind_depth, var_type)
                })
                .or_else(|| arena.named_terms.get(name).copied())
                .ok_or(Error {
                    kind: DefinitionError::ConstNotFound(name).into(),
                })
        }
    }

    #[inline]
    pub fn prop<'arena>() -> impl Generator<'arena> {
        |arena: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(arena.prop())
    }

    #[inline]
    pub fn type_<'arena>(level: UniverseLevel) -> impl Generator<'arena> {
        move |arena: &mut Arena<'arena>, _: &Environment<'arena>, _| Ok(arena.type_(level))
    }

    #[inline]
    pub fn app<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        u1: F1,
        u2: F2,
    ) -> impl Generator<'arena> {
        |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let u1 = u1(arena, env, depth)?;
            let u2 = u2(arena, env, depth)?;
            Ok(arena.app(u1, u2))
        }
    }

    #[inline]
    pub fn abs<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Generator<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(arena, env, depth)?;
            let env = env.update(name, (depth, arg_type));
            let body = body(arena, &env, depth + 1.into())?;
            Ok(arena.abs(arg_type, body))
        }
    }

    #[inline]
    pub fn prod<'arena, F1: Generator<'arena>, F2: Generator<'arena>>(
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl Generator<'arena> {
        move |arena: &mut Arena<'arena>, env: &Environment<'arena>, depth| {
            let arg_type = arg_type(arena, env, depth)?;
            let env = env.update(name, (depth, arg_type));
            let body = body(arena, &env, depth + 1.into())?;
            Ok(arena.prod(arg_type, body))
        }
    }
}

#[cfg(test)]
mod tests {
    // /!\ most of these tests are on ill-typed terms and should not be used for further testings
    use super::intern_build::*;
    use super::*;

    #[test]
    fn simple_subst() {
        use_arena(|arena| {
            // λx.(λy.x y) x
            let term = arena.build(abs(
                prop(),
                app(
                    abs(prop(), app(var(2.into(), prop()), var(1.into(), prop()))),
                    var(1.into(), prop()),
                ),
            ));
            // λx.x x
            let reduced = arena.build(abs(
                prop(),
                app(var(1.into(), prop()), var(1.into(), prop())),
            ));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
    }

    #[test]
    fn complex_subst() {
        use_arena(|arena| {
            // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
            let term = arena.build(app(
                abs(
                    prop(),
                    abs(
                        prop(),
                        abs(
                            prop(),
                            app(
                                app(
                                    app(
                                        var(3.into(), prop()),
                                        abs(
                                            prop(),
                                            abs(
                                                prop(),
                                                app(
                                                    var(1.into(), prop()),
                                                    app(
                                                        var(2.into(), prop()),
                                                        var(4.into(), prop()),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                    abs(prop(), var(2.into(), prop())),
                                ),
                                abs(prop(), var(1.into(), prop())),
                            ),
                        ),
                    ),
                ),
                abs(
                    prop(),
                    abs(prop(), app(var(2.into(), prop()), var(1.into(), prop()))),
                ),
            ));

            let term_step_1 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            app(
                                abs(
                                    prop(),
                                    abs(prop(), app(var(2.into(), prop()), var(1.into(), prop()))),
                                ),
                                abs(
                                    prop(),
                                    abs(
                                        prop(),
                                        app(
                                            var(1.into(), prop()),
                                            app(var(2.into(), prop()), var(4.into(), prop())),
                                        ),
                                    ),
                                ),
                            ),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_2 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            abs(
                                prop(),
                                app(
                                    abs(
                                        prop(),
                                        abs(
                                            prop(),
                                            app(
                                                var(1.into(), prop()),
                                                app(var(2.into(), prop()), var(5.into(), prop())),
                                            ),
                                        ),
                                    ),
                                    var(1.into(), prop()),
                                ),
                            ),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_3 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            abs(
                                prop(),
                                abs(
                                    prop(),
                                    app(
                                        var(1.into(), prop()),
                                        app(var(2.into(), prop()), var(4.into(), prop())),
                                    ),
                                ),
                            ),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_4 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        abs(
                            prop(),
                            app(
                                var(1.into(), prop()),
                                app(abs(prop(), var(3.into(), prop())), var(3.into(), prop())),
                            ),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_5 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        abs(prop(), var(1.into(), prop())),
                        app(abs(prop(), var(2.into(), prop())), var(2.into(), prop())),
                    ),
                ),
            ));

            let term_step_6 = arena.build(abs(
                prop(),
                abs(
                    prop(),
                    app(abs(prop(), var(2.into(), prop())), var(2.into(), prop())),
                ),
            ));

            // λa.λb.b
            let term_step_7 = arena.build(abs(prop(), abs(prop(), var(1.into(), prop()))));

            assert_eq!(arena.beta_reduction(term), term_step_1);
            assert_eq!(arena.beta_reduction(term_step_1), term_step_2);
            assert_eq!(arena.beta_reduction(term_step_2), term_step_3);
            assert_eq!(arena.beta_reduction(term_step_3), term_step_4);
            assert_eq!(arena.beta_reduction(term_step_4), term_step_5);
            assert_eq!(arena.beta_reduction(term_step_5), term_step_6);
            assert_eq!(arena.beta_reduction(term_step_6), term_step_7);
            assert_eq!(arena.beta_reduction(term_step_7), term_step_7);
        })
    }

    #[test]
    fn shift_prod() {
        use_arena(|arena| {
            let reduced = arena.build(prod(prop(), var(1.into(), prop())));
            let term = arena.build(app(abs(prop(), reduced.into()), prop()));

            assert_eq!(arena.beta_reduction(term), reduced)
        })
    }

    #[test]
    fn prod_beta_red() {
        use_arena(|arena| {
            let term = arena.build(prod(
                prop(),
                app(abs(prop(), var(1.into(), prop())), var(1.into(), prop())),
            ));
            let reduced = arena.build(prod(prop(), var(1.into(), prop())));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
    }

    #[test]
    fn app_red_rhs() {
        use_arena(|arena| {
            let term = arena.build(abs(
                prop(),
                app(
                    var(1.into(), prop()),
                    app(abs(prop(), var(1.into(), prop())), var(1.into(), prop())),
                ),
            ));
            let reduced = arena.build(abs(
                prop(),
                app(var(1.into(), prop()), var(1.into(), prop())),
            ));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
    }
}
