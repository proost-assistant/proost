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

// terms with closures
// maintains the invariant that a val is in normal form, which is unnecessary for type checking/conversion, we should only need
// weak-head normal forms
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Var(DeBruijnIndex),

    Prop,

    Type(UniverseLevel),

    App(Box<Val>, Box<Val>),

    Abs(String, Box<Val>, Closure),

    Prod(String, Box<Val>, Closure),
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

// TODO modify eval to get WHNFs instead of NFs
fn eval(e: &Env, t: Term) -> Val {
    match t {
        Prop => Val::Prop,
        Type(i) => Val::Type(i),
        Var(i) => e[i].clone(),
        App(box t1, box t2) => match eval(e, t1) {
            Val::Abs(_, _, t) => t.subst(eval(e, t2)),
            t => Val::App(box t, box eval(e, t2)),
        },
        Abs(s, box a, box b) => Val::Abs(
            s,
            box eval(e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
        Prod(s, box a, box b) => Val::Prod(
            s,
            box eval(e, a),
            Closure {
                env: e.clone(),
                term: b,
            },
        ),
    }
}

impl From<Val> for Term {
    fn from(v: Val) -> Term {
        match v {
            Val::Prop => Prop,
            Val::Type(i) => Type(i),
            Val::Var(i) => Var(i),
            Val::App(box t, box u) => App(box t.into(), box u.into()),
            Val::Abs(s, box t, u) => Abs(s, box t.into(), box u.term),
            Val::Prod(s, box t, u) => Prod(s, box t.into(), box u.term),
        }
    }
}

// returns normal form of term t in env e, should only be used for Reduce/Eval command, not when type-checking
pub fn nf(e: Env, t: Term) -> Term {
    eval(&e, t).into()
}

// /!\ IMPORTANT /!\
// Conversion function, checks whether two values are equal.
// The conversion is untyped, meaning that it should **Only**
// be called during type-checking when the two vals are already
// known to be of the same type and in the same context
pub fn conv(l: DeBruijnIndex, v1: Val, v2: Val) -> bool {
    match (v1, v2) {
        (Val::Type(i), Val::Type(j)) => i == j,

        (Val::Prop, Val::Prop) => true,

        (Val::Var(i), Val::Var(j)) => i == j,

        (Val::Prod(_, box a1, b1), Val::Prod(_, box a2, b2)) => {
            conv(l, a1, a2) && conv(l + 1.into(), b1.subst(Val::Var(l)), b2.subst(Val::Var(l)))
        }

        //Since we assume that both vals already have the same type,
        //checking conversion over the argument type is useless.
        //However, this doesn't mean we can simply remove the arg type
        //from the type constructor in the enum, it is needed to quote back to terms.
        (Val::Abs(_, _, t), Val::Abs(_, _, u)) => {
            conv(l + 1.into(), t.subst(Val::Var(l)), u.subst(Val::Var(l)))
        }

        (Val::Abs(_, _, t), u) | (u, Val::Abs(_, _, t)) => conv(
            l + 1.into(),
            t.subst(Val::Var(l)),
            Val::App(box u, box Val::Var(l)),
        ),

        (Val::App(box t1, box u1), Val::App(box t2, box u2)) => conv(l, t1, t2) && conv(l, u1, u2),

        _ => false,
    }
}

pub fn assert_def_eq(t1: Term, t2: Term) {
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
        new_env.push(Val::Var(self.env.len().into()));
        let mut new_types = self.types;
        new_types.push(vty);
        Ctx {
            env: new_env,
            types: new_types,
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
        }
    }
}

fn is_universe(t: Val) -> bool {
    matches!(t, Val::Prop | Val::Type(_))
}

// Computes universe the universe in which (x : A) -> B lives when A : u1 and B : u2
fn imax(u1: Val, u2: Val) -> Val {
    match u2 {
        Val::Prop => Val::Prop, // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
        Val::Type(ref i) => match u1 {
            Val::Prop => Val::Type(i.clone()),
            // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
            Val::Type(j) => Val::Type(max(i.clone(), j)),
            _ => panic!("Expected universe, found {:?}", u2.clone()),
        },
        _ => panic!("Expected universe, found {:?}", u1),
    }
}

pub fn check(ctx: &Ctx, t: Term, vty: Val) {
    match (t.clone(), vty.clone()) {
        (Abs(_, box t1, box t2), Val::Prod(_, box a, b)) => {
            check(&ctx.clone(), t1, a.clone());
            check(
                &ctx.clone().bind(a),
                t2,
                b.subst(Val::Var(ctx.env.len().into())),
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
        Val::Prop => Val::Type(0.into()),
        Val::Type(i) => Val::Type(i + 1.into()),
        Val::Var(i) => ctx.types[i].clone(),
        Val::Prod(_, box a, c) => {
            let ua = infer(ctx, a.clone());
            assert!(is_universe(ua.clone()));
            let ctx2 = ctx.clone().define(a, ua.clone());
            let ub = infer(&ctx2, eval(&ctx2.env, c.term));
            assert!(is_universe(ub.clone()));
            imax(ua, ub)
        }
        Val::Abs(s, box t1, c) => {
            let ctx2 = ctx.clone().bind(t1.clone());
            Val::Prod(
                s,
                box infer(ctx, t1),
                Closure {
                    env: ctx2.env.clone(),
                    term: infer(&ctx2, eval(&ctx2.env, c.term)).into(),
                },
            )
        }
        Val::App(box a, box b) => {
            if let Val::Prod(_, box t1, cls) = infer(ctx, a.clone()) {
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
    use crate::type_checker::*;
    use std::env;

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
        assert_eq!(ty, Val::Type(0.into()));
    }

    #[test]
    #[should_panic(
        expected = "Wrong argument, function\n  Var(DeBruijnIndex(0))\n expected argument of type\n  Prop\n but term\n    Var(DeBruijnIndex(0))\n is of type\n    Prod(\"\", Prop, Closure { env: [], term: Prop })"
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
