use derive_more::{Add, Display, From, Into, Sub};

use num_bigint::BigUint;

use bumpalo::Bump;
use im_rc::hashmap::HashMap as ImHashMap;
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, PartialEq, From, Into, Sub, PartialOrd, Ord, Hash,
)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Default, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord, Hash)]
pub struct UniverseLevel(BigUint);

pub struct Arena<'arena> {
    alloc: Bump,

    hashcons: HashSet<&'arena Node<'arena>>,
    named_terms: HashMap<&'arena str, Term<'arena>>,
}

#[derive(Clone, Copy, Eq, Debug)]
pub struct Term<'arena>(&'arena Node<'arena>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct Node<'arena> {
    payload: Payload<'arena>,
    // free_vars: an efficient type to measure whether a term is closed
    // this can be added in future iterations
    head_normal_form: OnceCell<Term<'arena>>,
    type_: OnceCell<Term<'arena>>,
}

// type BinTermHashMap<'arena> = HashMap<(Term<'arena>, Term<'arena>), Term<'arena>>;
// binary maps are not used so far, because, upon building terms, no particular optimisation is
// done. On the other hand, if it is decided that only WHNF terms may exist in the arena, there can
// be an AppMap (only App is relevant, as Abs and Prod preserve WHNF).
//
// We also need a substitution hasmap, (body, level_of_substitution, Term_to_incorporate)

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Payload<'arena> {
    Var(DeBruijnIndex, Term<'arena>),
    Prop,
    Type(UniverseLevel),
    App(Term<'arena>, Term<'arena>),
    Abs(Term<'arena>, Term<'arena>),
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

type Environment<'arena> = ImHashMap<&'arena str, (DeBruijnIndex, Term<'arena>)>;
trait ExternTermGenerator<'arena> = FnOnce(DeBruijnIndex, &Environment<'arena>) -> Term<'arena>;

impl<'arena> Arena<'arena> {
    /// (TODO DOC.) Allocate a new memory arena.
    fn new() -> Self {
        Arena {
            alloc: Bump::new(),

            hashcons: HashSet::new(),
            named_terms: HashMap::new(),
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
            Some(addr) => Term(addr),
            None => {
                let addr = &*Bump::alloc(&self.alloc, nn);
                self.hashcons.insert(addr);
                Term(addr)
            }
        }
    }

    pub fn var(&mut self, index: DeBruijnIndex, type_: Term<'arena>) -> Term<'arena> {
        self.hashcons(Var(index, type_))
    }

    pub fn prop(&mut self) -> Term<'arena> {
        self.hashcons(Prop)
    }

    pub fn type_(&mut self, level: UniverseLevel) -> Term<'arena> {
        self.hashcons(Type(level))
    }

    pub fn app(&mut self, u1: Term<'arena>, u2: Term<'arena>) -> Term<'arena> {
        self.hashcons(App(u1, u2))
    }

    pub fn abs(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Abs(arg_type, u))
    }

    pub fn prod(&mut self, arg_type: Term<'arena>, u: Term<'arena>) -> Term<'arena> {
        self.hashcons(Prod(arg_type, u))
    }

    /// These functions are available publicly, to the attention of the parser. They manipulate
    /// objects with a type morally equal to (Env -> Term), where Env is a working environment used
    /// in term construction from the parser.
    /// This is done as a way to elengantly keep the logic encapsulated in the kernel, but let the
    /// parser itself explore the term.

    pub fn extern_var<'a>(&'arena mut self, name: &'arena str) -> impl ExternTermGenerator<'arena> {
        move |depth, env: &Environment<'arena>| match env.get(&name) {
            Some((bind_depth, term)) => self.var(depth - *bind_depth, *term),
            None => panic!("change this for a proper error"),
        }
        env.get(&name).map(|(bind_depth, term)| Ok(self.var(depth - *bind_depth, *term))).
    }

    pub fn extern_prop(&'arena mut self) -> impl ExternTermGenerator<'arena> {
        |depth, env: &Environment<'arena>| self.prop()
    }

    pub fn extern_type(&'arena mut self, level: UniverseLevel) -> impl ExternTermGenerator<'arena> {
        |depth, env: &Environment<'arena>| self.type_(level)
    }

    pub fn extern_app<F1: ExternTermGenerator<'arena>, F2: ExternTermGenerator<'arena>>(
        &'arena mut self,
        u1: F1,
        u2: F2,
    ) -> impl ExternTermGenerator<'arena> {
        |depth, env: &Environment<'arena>| self.app(u1(depth, env), u2(depth, env))
    }

    pub fn extern_abs<F1: ExternTermGenerator<'arena>, F2: ExternTermGenerator<'arena>>(
        &'arena mut self,
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl ExternTermGenerator<'arena> {
        move |depth, env: &Environment<'arena>| {
            let arg_type = arg_type(depth, env);
            let env = env.update(name, (depth + DeBruijnIndex(1), arg_type));
            self.abs(arg_type, body(depth, &env))
        }
    }

    pub fn extern_prod<F1: ExternTermGenerator<'arena>, F2: ExternTermGenerator<'arena>>(
        &'arena mut self,
        name: &'arena str,
        arg_type: F1,
        body: F2,
    ) -> impl ExternTermGenerator<'arena> {
        move |depth, env: &Environment<'arena>| {
            let arg_type = arg_type(depth, env);
            let env = env.update(name, (depth + DeBruijnIndex(1), arg_type));
            self.prod(arg_type, body(depth, &env))
        }
    }

    pub fn extern_gen<F: ExternTermGenerator<'arena>>(f: F) -> Term<'arena> {
        f(DeBruijnIndex(0), &ImHashMap::new())
    }
}

/*
}

impl Term {
    /// Apply one step of β-reduction, using leftmost outermost evaluation strategy.
    pub fn beta_reduction(&self, env: &Environment) -> Term {
        match self {
            App(box Abs(_, box t1), box t2) => t1.substitute(t2, 1),
            App(box t1, box t2) => {
                let t1_new = t1.beta_reduction(env);
                if t1.clone() == t1_new {
                    App(box t1_new, box t2.beta_reduction(env))
                } else {
                    App(box t1_new, box t2.clone())
                }
            }
            Abs(x, box t) => Abs(x.clone(), box t.beta_reduction(env)),
            Prod(x, box t) => Prod(x.clone(), box t.beta_reduction(env)),
            Const(s) => env.get_term(s).unwrap(),
            _ => self.clone(),
        }
    }

    pub(crate) fn shift(&self, offset: usize, depth: usize) -> Term {
        match self {
            Var(i) if *i > depth.into() => Var(*i + offset.into()),
            App(box t1, box t2) => App(box t1.shift(offset, depth), box t2.shift(offset, depth)),
            Abs(t1, box t2) => Abs(box t1.shift(offset, depth), box t2.shift(offset, depth + 1)),
            Prod(t1, box t2) => Prod(box t1.shift(offset, depth), box t2.shift(offset, depth + 1)),
            _ => self.clone(),
        }
    }

    pub(crate) fn substitute(&self, rhs: &Term, depth: usize) -> Term {
        match self {
            Var(i) if *i == depth.into() => rhs.shift(depth - 1, 0),
            Var(i) if *i > depth.into() => Var(*i - 1.into()),

            App(l, r) => App(box l.substitute(rhs, depth), box r.substitute(rhs, depth)),
            Abs(t, term) => Abs(
                box t.substitute(rhs, depth),
                box term.substitute(rhs, depth + 1),
            ),
            Prod(t, term) => Prod(
                box t.substitute(rhs, depth),
                box term.substitute(rhs, depth + 1),
            ),
            _ => self.clone(),
        }
    }

    /// Returns the normal form of a term in a given environment.
    ///
    /// This function is computationally expensive and should only be used for Reduce/Eval commands, not when type-checking.
    pub fn normal_form(self, env: &Environment) -> Term {
        let mut res = self.beta_reduction(env);
        let mut temp = self;

        while res != temp {
            temp = res.clone();
            res = res.beta_reduction(env)
        }
        res
    }

    /// Returns the weak-head normal form of a term in a given environment.
    // TODO make whnf more lax, it shouldn't reduce applications in which the head is a neutral term. (#34)
    pub fn whnf(&self, env: &Environment) -> Term {
        match self {
            App(box t, t2) => match t.whnf(env) {
                whnf @ Abs(_, _) => App(box whnf, t2.clone()).beta_reduction(env).whnf(env),
                _ => self.clone(),
            },
            Const(s) => env.get_term(s).unwrap(),
            _ => self.clone(),
        }
    }
}

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
*/
