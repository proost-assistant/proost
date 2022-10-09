use crate::term::*;
use derive_more::{Add, Display, From, Into, Sub};
use std::ops::Index;
use Term::*;

type Env = Vec<Val>;

#[derive(Add, Copy, Clone, Debug, Display, Eq, Into, From, Sub, PartialEq, PartialOrd, Ord)]
pub struct DeBruijnLevel(usize);

impl Index<DeBruijnIndex> for Vec<Val> {
    type Output = Val;

    fn index(&self, i: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(i)]
    }
}

#[derive(Clone, Debug)]
pub enum Val {
    VVar(DeBruijnLevel),

    VProp,

    VType(UniverseLevel),

    VApp(Box<Val>, Box<Val>),

    VAbs(Box<Val>, Closure),

    VProd(Box<Val>, Closure),
}

#[derive(Clone, Debug)]
pub struct Closure {
    env: Env,
    term: Term,
}

impl Closure {
    pub fn shift(self, v: Val) -> Val {
        let e = &mut self.env.clone();
        e.push(v);
        eval(e, self.term)
    }
}

use Val::*;

fn eval(e: &Env, t: Term) -> Val {
    //println!("Evaluating {} in env {:?}",t.clone(),e.clone());
    let res = match t {
        Prop => VProp,
        Type(i) => VType(i),
        Var(i) => e[i].clone(),
        App(box t1, box t2) => match eval(&e, t1) {
            VAbs(_, t) => t.shift(eval(&e, t2)),
            t => VApp(box t, box eval(&e, t2)),
        },
        Abs(box a, box b) => VAbs(
            box eval(&e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
        Prod(box a, box b) => VProd(
            box eval(e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
    };
    //println!("resulting Val when evaluating {} in env {:?}: {}",t,e.clone(),res.clone());
    res
}

<<<<<<< HEAD
fn level_to_index(l1: Level, l2: Level) -> Index {
    l1 - l2 - 1
}

fn quote(l: Level, v: Val) -> Term {
    //println!("Quoting {} at Level {:?}",v.clone(),l.clone());
    let res =
    match v {
=======
fn quote(l: DeBruijnLevel, v: Val) -> Term {
    //println!("Quoting {} at Level {:?}",v.clone(),l.clone());
    let res = match v {
>>>>>>> 5a9bb8e (chore : adapt existing code to rebase)
        VProp => Prop,
        VType(i) => Type(i),
        VVar(i) => Var(usize::from(i).into()),
        VApp(box t, box u) => App(box quote(l, t), box quote(l, u)),
        VAbs(box t, u) => Abs(box quote(l, t), box quote(l + 1.into(), u.shift(VVar(l)))),
        VProd(box t, u) => Prod(box quote(l, t), box quote(l + 1.into(), u.shift(VVar(l)))),
    };
    //println!("resulting Term when evaluating {} at level {}: {}",v,l.clone(),res.clone());
    res
}

// returns normal form of term t in env e, should only be used for Reduce/Eval command, not when type-checking
pub fn nf(e: Env, t: Term) -> Term {
    quote(e.len().into(), eval(&e, t))
}

// /!\ IMPORTANT /!\
// Conversion function, checks whether two values are equal.
// The conversion is untyped, meaning that it should **Only**
// be called during type-checking when the two vals are already
// known to be of the same type.
pub fn conv(l: DeBruijnLevel, v1: Val, v2: Val) -> bool {
    println!(
        "checking conversion between {:?} and {:?} at level {}",
        v1.clone(),
        v2.clone(),
        l.clone()
    );
    let res = match (v1, v2) {
        (VType(i), VType(j)) => i == j,

        (VProp, VProp) => true,

        (VVar(i), VVar(j)) => i == j,

        (VProd(box a1, b1), VProd(box a2, b2)) => {
            conv(l, a1, a2) && conv(l + 1.into(), b1.shift(VVar(l)), b2.shift(VVar(l)))
        }

        //Since we assume that both vals already have the same type,
        //checking conversion over the argument type is useless.
        //However, this doesn't mean we can simply remove the arg type
        //from the type constructor in the enum, it is needed to quote back to terms.
        (VAbs(_, t), VAbs(_, u)) => conv(l + 1.into(), t.shift(VVar(l)), u.shift(VVar(l))),

        (VAbs(_, t), u) | (u, VAbs(_, t)) => {
            conv(l + 1.into(), t.shift(VVar(l)), VApp(box u, box VVar(l)))
        }

        (VApp(box t1, box u1), VApp(box t2, box u2)) => conv(l, t1, t2) && conv(l, u1, u2),

        _ => false,
    };
    if res {
        println!("SUCCESS")
    } else {
        println!("FAIL")
    };
    res
}

pub fn assert_def_eq(t1: Term, t2: Term) {
    println!("t1 : {}", t1.clone());
    println!("t1 nf : {}", nf(Vec::new(), t1.clone()));
    println!("t2 : {}", t2.clone());
    println!("t2 nf : {}", nf(Vec::new(), t2.clone()));
    assert!(conv(0.into(), eval(&Vec::new(), t1), eval(&Vec::new(), t2)))
}

//type of lists of tuples representing the respective types of terms
/*type Types = Vec<(Term, Term)>;

struct Ctx {
    env: Env,
    types: Types,
    lvl: DeBruijnLevel,
}

impl Ctx {
    pub fn empty() -> Ctx {
        Ctx {
            env: Vec::new(),
            types: Vec::new(),
            lvl: 0.into(),
        }
    }

    /*fn bind(s : String,t : Term, Ctx{env : env, types : types, lvl : lvl} : Ctx) {
        Ctx {
            env : e.push(eval(&e,t)),
            types : ty.push((s,t)),
            lvl : lvl + 1,
        }
    }*/
}*/

#[cfg(test)]
mod tests {
    use std::env;
    // TODO: Correctly types lambda terms.
    use crate::type_checker::*;

    fn assert_def_eq(t1: Term, t2: Term) {
        assert_eq!(conv(0, eval(&Vec::new(), t1), eval(&Vec::new(), t2)), true)
    }

    #[test]
    fn simple() {
        let t1 = App(box Abs(box Type(0.into()), box Var(0.into())), box Prop);
        let t2 = Prop;
        assert_eq!(
            conv(0.into(), eval(&Vec::new(), t1), eval(&Vec::new(), t2)),
            true
        )
    }

    #[test]
    fn simple_subst() {
        env::set_var("RUST_BACKTRACE", "1");
        // λx.(λy.x y) x
        let term = Abs(
            box Prod(box Prop, box Prop),
            box App(
                box Abs(box Prop, box App(box Var(1.into()), box Var(0.into()))),
                box Var(0.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(0.into()), box Var(0.into())));

        assert_def_eq(term, reduced);
    }

    #[test]
    fn complex_subst() {
        // λa.λb.(((λc.λd.c d) (λc.λd.d (c a))) (λ_.b)) (λc.c)
        let term = Abs(
                    box Prop, //a
                    box Abs(
                        box Prop, //b
                        box App(
                            box App(
                                box App(
                                    box Abs(
                                        box Prod(box Prop,box Prop),
                                        box Abs(box Prop, box App(box Var(1), box Var(0)))),
                                    box Abs(
                                        box Prod(box Prop,box Prop),
                                        box Abs(
                                            box Prod(box Prod(box Prop,box Prop),box Prop),
                                            box App(box Var(0), box App(box Var(1), box Var(3))),
                                        ),
                                    ),
                                ),
                                box Abs(box Prop, box Var(1)),
                            ),
                            box Abs(box Prop, box Var(2)),
                        ),
                    ),
                );

        // λa.λb.b
        let term_step_7 = Abs(box Prop, box Abs(box Prop, box Var(0)));

        assert_def_eq(term, term_step_7);
    }


    fn id(l: usize) -> Box<Term> {
        box Abs(box Prop, box Var(l.into()))
    }

    #[test]
    fn complex_conv() {
        //(λa.λb.λc.a ((λd.λe.e b d)(λx.x))) ((λa.λb.a b) ((λx.x) (λx.x)))
        let term = App(
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(
                        box Prop,
                        box App(
                            box Var(0.into()),
                            box App(
                                box Abs(
                                    box Prop,
                                    box Abs(
                                        box Prop,
                                        box App(
                                            box App(box Var(4.into()), box Var(1.into())),
                                            box Var(3.into()),
                                        ),
                                    ),
                                ),
                                id(3),
                            ),
                        ),
                    ),
                ),
            ),
            box App(
                box Abs(
                    box Prop,
                    box Abs(box Prop, box App(box Var(0.into()), box Var(1.into()))),
                ),
                box App(id(0), id(0)),
            ),
        );
        //(λb.(λc.(λe.((e b) (λx.x)))))
        let reduced = Abs(
            box Prop,
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box App(box App(box Var(2.into()), box Var(0.into())), id(3)),
                ),
            ),
        );
        assert_def_eq(term, reduced)
    }

    //(λ ℙ → λ ℙ → λ ℙ → (0 (λ ℙ → λ ℙ → ((4 1) 3) λ ℙ → 3)) (λ ℙ → λ ℙ → (0 1) (λ ℙ → 0 λ ℙ → 0)))
    #[test]
    fn nf_test() {
        //λa.a (λx.x) (λx.x)
        let reduced = Abs(box Prop, box App(box App(box Var(0.into()), id(1)), id(1)));
        let nff = nf(Vec::new(), reduced.clone());
        println!("r : {}", reduced.clone());
        println!("r nf : {}", nff.clone());
        assert_eq!(reduced.clone(), nff.clone());
        assert_def_eq(reduced, nff);
    }
}
