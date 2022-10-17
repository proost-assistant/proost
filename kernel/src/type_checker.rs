use crate::term::*;
use num_bigint::BigUint;
use std::cmp::max;
use std::ops::Index;

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Val {
    Var(DeBruijnIndex),

    Prop,

    Type(UniverseLevel),

    App(Box<Val>, Box<Val>),

    Abs(String, Box<Val>, Closure),

    Prod(String, Box<Val>, Closure),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    env: Env,
    term: Term,
}
use Val::*;

impl Closure {
    pub fn subst(self, v: Val) -> Val {
        let e = &mut self.env.clone();
        e.push(v);
        Term::eval(e, self.term)
    }
}

impl From<Val> for Term {
    fn from(v: Val) -> Term {
        match v {
            Prop => Term::Prop,
            Type(i) => Term::Type(i),
            Var(i) => Term::Var(i),
            App(box t, box u) => Term::App(box t.into(), box u.into()),
            Abs(s, box t, u) => Term::Abs(s, box t.into(), box u.term),
            Prod(s, box t, u) => Term::Prod(s, box t.into(), box u.term),
        }
    }
}

impl Term {
    // TODO modify eval to get WHNFs instead of NFs
    fn eval(e: &Env, t: Term) -> Val {
        match t {
            Term::Prop => Prop,
            Term::Type(i) => Type(i),
            Term::Var(i) => e[i].clone(),
            Term::App(box t1, box t2) => match Term::eval(e, t1) {
                Abs(_, _, t) => t.subst(Term::eval(e, t2)),
                t => App(box t, box Term::eval(e, t2)),
            },
            Term::Abs(s, box a, box b) => Abs(
                s,
                box Term::eval(e, a),
                Closure {
                    env: e.clone(),
                    term: b,
                },
            ),
            Term::Prod(s, box a, box b) => Prod(
                s,
                box Term::eval(e, a),
                Closure {
                    env: e.clone(),
                    term: b,
                },
            ),
        }
    }

    // returns normal form of term t in env e, should only be used for Reduce/Eval command, not when type-checking
    pub fn normal_form(e: Env, t: Term) -> Term {
        Self::eval(&e, t).into()
    }

    // /!\ IMPORTANT /!\
    // Conversion function, checks whether two values are equal.
    // The conversion is untyped, meaning that it should **Only**
    // be called during type-checking when the two vals are already
    // known to be of the same type and in the same context
    pub fn conv(l: DeBruijnIndex, v1: Val, v2: Val) -> bool {
        match (v1, v2) {
            (Type(i), Type(j)) => i == j,

            (Prop, Prop) => true,

            (Var(i), Var(j)) => i == j,

            (Prod(_, box a1, b1), Prod(_, box a2, b2)) => {
                Term::conv(l, a1, a2)
                    && Term::conv(l + 1.into(), b1.subst(Var(l)), b2.subst(Var(l)))
            }

            //Since we assume that both vals already have the same type,
            //checking conversion over the argument type is useless.
            //However, this doesn't mean we can simply remove the arg type
            //from the type constructor in the enum, it is needed to quote back to terms.
            (Abs(_, _, t), Abs(_, _, u)) => {
                Term::conv(l + 1.into(), t.subst(Var(l)), u.subst(Var(l)))
            }

            (Abs(_, _, t), u) | (u, Abs(_, _, t)) => {
                Term::conv(l + 1.into(), t.subst(Var(l)), App(box u, box Var(l)))
            }

            (App(box t1, box u1), App(box t2, box u2)) => {
                Term::conv(l, t1, t2) && Term::conv(l, u1, u2)
            }

            _ => false,
        }
    }

    pub fn assert_def_eq(t1: Term, t2: Term) {
        assert!(Term::conv(
            0.into(),
            Term::eval(&Vec::new(), t1),
            Term::eval(&Vec::new(), t2)
        ))
    }
}
//The context, which is supposed to contain other definitions in the environment, is not implemented for now
//TODO use context for type-checking(#17)

//type of lists of tuples representing the respective types of each variables
type Types = Vec<Val>;
#[derive(Clone, Debug)]
pub struct Ctx {
    env: Env,
    types: Types,
}

impl Ctx {
    pub fn new() -> Ctx {
        Ctx {
            env: Vec::new(),
            types: Vec::new(),
        }
    }

    // Extend Ctx with a bound variable.
    fn bind(self, vty: Val) -> Ctx {
        let mut new_env = self.env.clone();
        new_env.push(Var(self.env.len().into()));
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

impl Default for Ctx {
    fn default() -> Self {
        Ctx::new()
    }
}

fn is_universe(t: Val) -> bool {
    matches!(t, Prop | Type(_))
}

// Computes universe the universe in which (x : A) -> B lives when A : u1 and B : u2
fn imax(u1: Val, u2: Val) -> Result<Val, String> {
    match u2 {
        Prop => Ok(Prop), // Because Term::Prop is impredicative, if B : Term::Prop, then (x : A) -> b : Term::Prop
        Type(ref i) => match u1 {
            Prop => Ok(Type(i.clone())),
            // else if u1 = Term::Type(i) and u2 = Term::Type(j), then (x : A) -> B : Term::Type(max(i,j))
            Type(j) => Ok(Type(max(i.clone(), j))),
            _ => Err(format!("Expected universe, found {:?}", u2.clone())),
        },
        _ => Err(format!("Expected universe, found {:?}", u1)),
    }
}

pub fn check(ctx: &Ctx, t: Term, vty: Val) -> Result<(), String> {
    match (t.clone(), vty.clone()) {
        (Term::Abs(_, box t1, box t2), Prod(_, box a, b)) => {
            check(&ctx.clone(), t1, a.clone())?;
            check(&ctx.clone().bind(a), t2, b.subst(Var(ctx.env.len().into())))
        }
        _ => {
            let tty = infer(ctx, Term::eval(&ctx.env, t))?;
            if !Term::conv(ctx.env.len().into(), tty.clone(), vty.clone()) {
                return Err(format!(
                    "type mismatch\nexpected type:\n  {:?}\n\ninferred type:\n  {:?}\n",
                    vty, tty
                ));
            };
            Ok(())
        }
    }
}

pub fn infer(ctx: &Ctx, t: Val) -> Result<Val, String> {
    match t {
        Prop => Ok(Type(BigUint::from(0_u64).into())),
        Type(i) => Ok(Type(i + BigUint::from(1_u64).into())),
        Var(i) => Ok(ctx.types[i].clone()),
        Prod(_, box a, c) => {
            let ua = infer(ctx, a.clone())?;
            if !is_universe(ua.clone()) {
                Err(format!("   {:?}\n Is not a type.", ua))
            } else {
                let ctx2 = ctx.clone().define(a, ua.clone());
                let ub = infer(&ctx2, Term::eval(&ctx2.env, c.term))?;
                if !is_universe(ub.clone()) {
                    Err(format!("   {:?}\n Is not a type.", ub))
                } else {
                    imax(ua, ub)
                }
            }
        }
        Abs(s, box t1, c) => {
            let ctx2 = ctx.clone().bind(t1.clone());
            Ok(Prod(
                s,
                box infer(ctx, t1)?,
                Closure {
                    env: ctx2.env.clone(),
                    term: infer(&ctx2, Term::eval(&ctx2.env, c.term))?.into(),
                },
            ))
        }
        App(box a, box b) => {
            if let Prod(_, box t1, cls) = infer(ctx, a.clone())? {
                let t1_ = infer(ctx, b.clone());
                if !Term::conv(ctx.env.len().into(), t1.clone(), t1_.clone()?) {
                    return Err(format!("Wrong argument, function\n  {:?}\n expected argument of type\n  {:?}\n but term\n    {:?}\n is of type\n    {:?}",a,t1,b,t1_));
                };
                Ok(Term::eval(&cls.env, cls.term))
            } else {
                Err(format!("\n    {:?}\nIs not a function, hence argument \n    {:?}\ncan't be given to it",a,b))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::type_checker::*;
    use std::env;

    #[test]
    fn simple() {
        let t1 = Term::App(
            box Term::Abs(
                "".into(),
                box Term::Type(BigUint::from(0_u64).into()),
                box Term::Var(0.into()),
            ),
            box Term::Prop,
        );
        let t2 = Term::Prop;
        let v1 = Term::eval(&Vec::new(), t1.clone());
        assert_eq!(
            Term::conv(0.into(), v1.clone(), Term::eval(&Vec::new(), t2)),
            true
        );
        let ty = infer(&Ctx::new(), v1);
        assert_eq!(ty, Ok(Type(BigUint::from(0_u64).into())));
    }

    #[test]
    fn simple_subst() {
        env::set_var("RUST_BACKTRACE", "0");
        // λx.(λy.x y) x
        let term = Term::Abs(
            "".into(),
            box Term::Prod("".into(), box Term::Prop, box Term::Prop),
            box Term::App(
                box Term::Abs(
                    "".into(),
                    box Term::Prop,
                    box Term::App(box Term::Var(1.into()), box Term::Var(0.into())),
                ),
                box Term::Var(0.into()),
            ),
        );

        // λx.x x
        let reduced = Term::Abs(
            "".into(),
            box Term::Prop,
            box Term::App(box Term::Var(0.into()), box Term::Var(0.into())),
        );

        Term::assert_def_eq(term.clone(), reduced);
        let v1 = Term::eval(&Vec::new(), term.clone());
        let _ty = infer(&Ctx::new(), v1);
        assert_eq!(_ty, Err("Wrong argument, function\n  Var(DeBruijnIndex(0))\n expected argument of type\n  Prop\n but term\n    Var(DeBruijnIndex(0))\n is of type\n    Ok(Prod(\"\", Prop, Closure { env: [], term: Prop }))".into()))
    }

    fn id(l: usize) -> Box<Term> {
        box Term::Abs("".into(), box Term::Prop, box Term::Var(l.into()))
    }

    #[test]
    fn complex_conv() {
        //(λa.λb.λc.a ((λd.λe.e b d)(λx.x))) ((λa.λb.a b) ((λx.x) (λx.x)))
        let term = Term::App(
            box Term::Abs(
                "".into(),
                box Term::Prop,
                box Term::Abs(
                    "".into(),
                    box Term::Prop,
                    box Term::Abs(
                        "".into(),
                        box Term::Prop,
                        box Term::App(
                            box Term::Var(0.into()),
                            box Term::App(
                                box Term::Abs(
                                    "".into(),
                                    box Term::Prop,
                                    box Term::Abs(
                                        "".into(),
                                        box Term::Prop,
                                        box Term::App(
                                            box Term::App(
                                                box Term::Var(4.into()),
                                                box Term::Var(1.into()),
                                            ),
                                            box Term::Var(3.into()),
                                        ),
                                    ),
                                ),
                                id(3),
                            ),
                        ),
                    ),
                ),
            ),
            box Term::App(
                box Term::Abs(
                    "".into(),
                    box Term::Prop,
                    box Term::Abs(
                        "".into(),
                        box Term::Prop,
                        box Term::App(box Term::Var(0.into()), box Term::Var(1.into())),
                    ),
                ),
                box Term::App(id(0), id(0)),
            ),
        );
        //(λb.(λc.(λe.((e b) (λx.x)))))
        let reduced = Term::Abs(
            "".into(),
            box Term::Prop,
            box Term::Abs(
                "".into(),
                box Term::Prop,
                box Term::Abs(
                    "".into(),
                    box Term::Prop,
                    box Term::App(
                        box Term::App(box Term::Var(2.into()), box Term::Var(0.into())),
                        id(3),
                    ),
                ),
            ),
        );
        Term::assert_def_eq(term, reduced)
    }

    //(λ ℙ → λ ℙ → λ ℙ → (0 (λ ℙ → λ ℙ → ((4 1) 3) λ ℙ → 3)) (λ ℙ → λ ℙ → (0 1) (λ ℙ → 0 λ ℙ → 0)))
    #[test]
    fn nf_test() {
        //λa.a (λx.x) (λx.x)
        let reduced = Term::Abs(
            "".into(),
            box Term::Prop,
            box Term::App(box Term::App(box Term::Var(0.into()), id(1)), id(1)),
        );
        let nff = Term::normal_form(Vec::new(), reduced.clone());
        println!("r : {}", reduced.clone());
        println!("r nf : {}", nff.clone());
        assert_eq!(reduced.clone(), nff.clone());
        Term::assert_def_eq(reduced, nff);
    }

    #[test]
    fn polymorphism() {
        env::set_var("RUST_BACKTRACE", "full");
        let id = Term::Abs(
            "A".into(),
            box Term::Type(BigUint::from(0_u64).into()),
            box Term::Abs("x".into(), box Term::Var(0.into()), box Term::Var(1.into())),
        );
        let _ = infer(&Ctx::new(), Term::eval(&Vec::new(), id));
        ()
    }
}
