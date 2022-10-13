use crate::term::*;
use core::panic;
use std::cmp::max;
use std::ops::Index;
use Term::*;

type Env = Vec<Val>;

impl Index<DeBruijnIndex> for Vec<Val> {
    type Output = Val;

    fn index(&self, i: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(i)]
    }
}

//  transform Vtruc to truc
//  describe the type
// maintains the invariant that a val is in normal form, which is unnecessary for type checking/conversion, we should only need
// weak-head normal forms
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    VVar(DeBruijnIndex),

    VProp,

    VType(UniverseLevel),

    VApp(Box<Val>, Box<Val>),

    VAbs(String, Box<Val>, Closure),

    VProd(String, Box<Val>, Closure),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    env: Env,
    term: Term,
}

impl Closure {
    pub fn subst(self, v: Val) -> Val {
        let e = &mut self.env.clone();
        e.push(v);
        eval(e, self.term)
    }
}

use Val::*;

// TODO modify eval to get WHNFs instead of NFs
fn eval(e: &Env, t: Term) -> Val {
    match t {
        Prop => VProp,
        Type(i) => VType(i),
        Var(i) => e[i].clone(),
        App(box t1, box t2) => match eval(e, t1) {
            VAbs(_, _, t) => t.subst(eval(e, t2)),
            t => VApp(box t, box eval(e, t2)),
        },
        Abs(s, box a, box b) => VAbs(
            s,
            box eval(e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
        Prod(s, box a, box b) => VProd(
            s,
            box eval(e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
    }
}

//TODO transform into Into
fn quote(v: Val) -> Term {
    match v {
        VProp => Prop,
        VType(i) => Type(i),
        VVar(i) => Var(i),
        VApp(box t, box u) => App(box quote(t), box quote(u)),
        VAbs(s, box t, u) => Abs(s, box quote(t), box u.term),
        VProd(s, box t, u) => Prod(s, box quote(t), box u.term),
    }
}

// returns normal form of term t in env e, should only be used for Reduce/Eval command, not when type-checking
pub fn nf(e: Env, t: Term) -> Term {
    quote(eval(&e, t))
}

// /!\ IMPORTANT /!\
// Conversion function, checks whether two values are equal.
// The conversion is untyped, meaning that it should **Only**
// be called during type-checking when the two vals are already
// known to be of the same type and in the same context
pub fn conv(l: DeBruijnIndex, v1: Val, v2: Val) -> bool {
    println!(
        "checking conversion between {:?} and {:?} at level {}",
        v1, v2, l
    );
    let res = match (v1, v2) {
        (VType(i), VType(j)) => i == j,

        (VProp, VProp) => true,

        (VVar(i), VVar(j)) => i == j,

        (VProd(_, box a1, b1), VProd(_, box a2, b2)) => {
            conv(l, a1, a2) && conv(l + 1.into(), b1.subst(VVar(l)), b2.subst(VVar(l)))
        }

        //Since we assume that both vals already have the same type,
        //checking conversion over the argument type is useless.
        //However, this doesn't mean we can simply remove the arg type
        //from the type constructor in the enum, it is needed to quote back to terms.
        (VAbs(_, _, t), VAbs(_, _, u)) => conv(l + 1.into(), t.subst(VVar(l)), u.subst(VVar(l))),

        (VAbs(_, _, t), u) | (u, VAbs(_, _, t)) => {
            conv(l + 1.into(), t.subst(VVar(l)), VApp(box u, box VVar(l)))
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
    println!("t1 : {}", t1);
    println!("t1 nf : {}", nf(Vec::new(), t1.clone()));
    println!("t2 : {}", t2);
    println!("t2 nf : {}", nf(Vec::new(), t2.clone()));
    assert!(conv(0.into(), eval(&Vec::new(), t1), eval(&Vec::new(), t2)))
}

//The context, which is supposed to contain other definitions in the environment, is not implemented for now, though it wouldn't be too hard to implement

//type of lists of tuples representing the respective types of each variables
type Types = Vec<Val>;
#[derive(Clone, Debug)]
pub struct Ctx {
    env: Env,
    types: Types,
}

impl Ctx {
    pub fn empty() -> Ctx {
        Ctx {
            env: Vec::new(),
            types: Vec::new(),
        }
    }

    // Extend Ctx with a bound variable.
    fn bind(self, vty: Val) -> Ctx {
        let mut new_env = self.env.clone();
        new_env.push(VVar(self.env.len().into()));
        let mut new_types = self.types;
        new_types.push(vty);
        Ctx {
            env: new_env,
            types: new_types,
            //lvl : lvl + 1.into(), //note, currently, lvl = types.len()
        }
    }
    // Extend Ctx with a definition.
    fn define(self, v: Val, vty: Val) -> Ctx {
        let mut new_env = self.env.clone();
        new_env.push(v);
        let mut new_types = self.types;
        new_types.push(vty);
        Ctx {
            env: new_env,
            types: new_types,
            //lvl : lvl + 1.into()
        }
    }
}

fn is_universe(t: Val) -> bool {
    matches!(t, VProp | VType(_))
}

// Computes universe the universe in which (x : A) -> B lives when A : u1 and B : u2
fn imax(u1: Val, u2: Val) -> Val {
    match u2 {
        VProp => VProp, // Because Prod is impredicative, if B : Prod, then (x : A) -> b : Prod
        VType(ref i) => match u1 {
            VProp => VType(i.clone()),
            // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
            VType(j) => VType(max(i.clone(), j)),
            _ => panic!("Expected universe, found {:?}", u2.clone()),
        },
        _ => panic!("Expected universe, found {:?}", u1),
    }
}

pub fn check(ctx: &Ctx, t: Term, vty: Val) {
    match (t.clone(), vty.clone()) {
        (Abs(_, box t1, box t2), VProd(_, box a, b)) => {
            check(&ctx.clone(), t1, a.clone());
            check(
                &ctx.clone().bind(a),
                t2,
                b.subst(VVar(ctx.env.len().into())),
            )
        }
        _ => {
            let tty = infer(ctx, eval(&ctx.env, t));
            if !conv(ctx.env.len().into(), tty.clone(), vty.clone()) {
                panic!(
                    "type mismatch\nexpected type:\n  {:?}\n\ninferred type:\n  {:?}\n",
                    vty, tty
                )
            };
        }
    }
}

pub fn infer(ctx: &Ctx, t: Val) -> Val {
    match t {
        VProp => VType(0.into()),
        VType(i) => VType(i + 1.into()),
        VVar(i) => ctx.types[i].clone(),
        VProd(_, box a, c) => {
            let ua = infer(ctx, a.clone());
            assert!(is_universe(ua.clone()));
            let ctx2 = ctx.clone().define(a, ua.clone());
            let ub = infer(&ctx2, eval(&ctx2.env, c.term));
            assert!(is_universe(ub.clone()));
            imax(ua, ub)
        }
        VAbs(_s, box t1, c) => {
            let ctx2 = ctx.clone().bind(t1);
            println!("{:?}", ctx2);
            infer(&ctx2, eval(&ctx2.env, c.term))
        }
        VApp(box a, box b) => {
            if let VProd(_, box t1, cls) = infer(ctx, a.clone()) {
                let t1_ = infer(ctx, b.clone());
                if !conv(ctx.env.len().into(), t1.clone(), t1_.clone()) {
                    panic!("Wrong argument, function\n  {:?}\n expected argument of type\n  {:?}\n but term\n    {:?}\n is of type\n    {:?}",
                a,t1,b,t1_)
                };
                eval(&cls.env, cls.term)
            } else {
                panic!("\n    {:?}\nIs not a function, hence argument \n    {:?}\ncan't be given to it",a,b)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    // TODO: Correctly types lambda terms.
    use crate::type_checker::*;

    fn assert_def_eq(t1: Term, t2: Term) {
        assert_eq!(
            conv(0.into(), eval(&Vec::new(), t1), eval(&Vec::new(), t2)),
            true
        )
    }

    #[test]
    fn simple() {
        let t1 = App(
            box Abs("".into(), box Type(0.into()), box Var(0.into())),
            box Prop,
        );
        let t2 = Prop;
        let v1 = eval(&Vec::new(), t1.clone());
        assert_eq!(conv(0.into(), v1.clone(), eval(&Vec::new(), t2)), true);
        let ty = infer(&Ctx::empty(), v1);
        println!("{:?}", ty.clone());
        assert_eq!(ty, VType(0.into()));
    }

    #[test]
    #[should_panic(
        expected = "Wrong argument, function\n  VVar(DeBruijnIndex(0))\n expected argument of type\n  VProp\n but term\n    VVar(DeBruijnIndex(0))\n is of type\n    VProd(\"\", VProp, Closure { env: [], term: Prop })"
    )]
    fn simple_subst() {
        env::set_var("RUST_BACKTRACE", "0");
        // λx.(λy.x y) x
        let term = Abs(
            "".into(),
            box Prod("".into(), box Prop, box Prop),
            box App(
                box Abs(
                    "".into(),
                    box Prop,
                    box App(box Var(1.into()), box Var(0.into())),
                ),
                box Var(0.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(
            "".into(),
            box Prop,
            box App(box Var(0.into()), box Var(0.into())),
        );

        assert_def_eq(term.clone(), reduced);
        let v1 = eval(&Vec::new(), term.clone());
        let _ty = infer(&Ctx::empty(), v1);
    }

    fn id(l: usize) -> Box<Term> {
        box Abs("".into(), box Prop, box Var(l.into()))
    }

    #[test]
    fn complex_conv() {
        //(λa.λb.λc.a ((λd.λe.e b d)(λx.x))) ((λa.λb.a b) ((λx.x) (λx.x)))
        let term = App(
            box Abs(
                "".into(),
                box Prop,
                box Abs(
                    "".into(),
                    box Prop,
                    box Abs(
                        "".into(),
                        box Prop,
                        box App(
                            box Var(0.into()),
                            box App(
                                box Abs(
                                    "".into(),
                                    box Prop,
                                    box Abs(
                                        "".into(),
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
                    "".into(),
                    box Prop,
                    box Abs(
                        "".into(),
                        box Prop,
                        box App(box Var(0.into()), box Var(1.into())),
                    ),
                ),
                box App(id(0), id(0)),
            ),
        );
        //(λb.(λc.(λe.((e b) (λx.x)))))
        let reduced = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box Abs(
                    "".into(),
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
        let reduced = Abs(
            "".into(),
            box Prop,
            box App(box App(box Var(0.into()), id(1)), id(1)),
        );
        let nff = nf(Vec::new(), reduced.clone());
        println!("r : {}", reduced.clone());
        println!("r nf : {}", nff.clone());
        assert_eq!(reduced.clone(), nff.clone());
        assert_def_eq(reduced, nff);
    }

    #[test]
    fn polymorphism() {
        env::set_var("RUST_BACKTRACE", "full");
        let id = Abs(
            "A".into(),
            box Type(0.into()),
            box Abs("x".into(), box Var(0.into()), box Var(1.into())),
        );
        let _ = infer(&Ctx::empty(), eval(&Vec::new(), id));
        ()
    }
}
