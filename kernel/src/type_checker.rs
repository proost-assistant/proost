use crate::term::*;
use num_bigint::BigUint;
use std::cmp::max;
use std::ops::Index;

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
use Val::*;

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

type Env = Vec<Val>;

impl Index<DeBruijnIndex> for Vec<Val> {
    type Output = Val;

    fn index(&self, i: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(i)]
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    env: Env,
    term: Term,
}

impl Closure {
    pub fn subst(self, v: Val) -> Val {
        let e = &mut self.env.clone();
        e.push(v);
        self.term.eval(e)
    }
}

impl Val {
    // /!\ IMPORTANT /!\
    // Conversion function, checks whether two values are equal.
    // The conversion is untyped, meaning that it should **Only**
    // be called during type-checking when the two vals are already
    // known to be of the same type and in the same context
    pub fn conversion(self, rhs: Val, l: DeBruijnIndex) -> bool {
        match (self, rhs) {
            (Type(i), Type(j)) => i == j,

            (Prop, Prop) => true,

            (Var(i), Var(j)) => i == j,

            (Prod(_, box a1, b1), Prod(_, box a2, b2)) => {
                a1.conversion(a2, l) && b1.subst(Var(l)).conversion(b2.subst(Var(l)), l + 1.into())
            }

            //Since we assume that both vals already have the same type,
            //checking conversion over the argument type is useless.
            //However, this doesn't mean we can simply remove the arg type
            //from the type constructor in the enum, it is needed to quote back to terms.
            (Abs(_, _, t), Abs(_, _, u)) => {
                t.subst(Var(l)).conversion(u.subst(Var(l)), l + 1.into())
            }

            (Abs(_, _, t), u) | (u, Abs(_, _, t)) => t
                .subst(Var(l))
                .conversion(App(box u, box Var(l)), l + 1.into()),

            (App(box t1, box u1), App(box t2, box u2)) => {
                t1.conversion(t2, l) && u1.conversion(u2, l)
            }

            _ => false,
        }
    }

    fn is_universe(&self) -> bool {
        matches!(*self, Prop | Type(_))
    }
}

impl Term {
    // TODO modify eval to get WHNFs instead of NFs
    fn eval(self, e: &Env) -> Val {
        match self {
            Term::Prop => Prop,
            Term::Type(i) => Type(i),
            Term::Var(i) => e[i].clone(),
            Term::App(box t1, box t2) => match t1.eval(e) {
                Abs(_, _, t) => t.subst(t2.eval(e)),
                t => App(box t, box t2.eval(e)),
            },
            Term::Abs(s, box a, box b) => Abs(
                s,
                box a.eval(e),
                Closure {
                    env: e.clone(),
                    term: b,
                },
            ),
            Term::Prod(s, box a, box b) => Prod(
                s,
                box a.eval(e),
                Closure {
                    env: e.clone(),
                    term: b,
                },
            ),
        }
    }

    // returns normal form of term t in env e, should only be used for Reduce/Eval command, not when type-checking
    pub fn normal_form(self, e: Env) -> Term {
        self.eval(&e).into()
    }

    pub fn is_def_eq(self, rhs: Term) -> Result<(), String> {
        if !self
            .clone()
            .eval(&Vec::new())
            .conversion(rhs.clone().eval(&Vec::new()), 0.into())
        {
            //TODO #19
            Err(format!(
                "Error, term\n  {:?}\n is not definitionally equal to \n    {:?}\n",
                self, rhs
            ))
        } else {
            Ok(())
        }
    }

    pub fn check(self, ctx: &Ctx, vty: Val) -> Result<(), String> {
        match (self.clone(), vty.clone()) {
            (Term::Abs(_, box t1, box t2), Prod(_, box a, b)) => {
                t1.check(&ctx.clone(), a.clone())?;
                t2.check(&ctx.clone().bind(a), b.subst(Var(ctx.env.len().into())))
            }
            _ => {
                let tty = self.eval(&ctx.env).infer(ctx)?;
                if !tty.clone().conversion(vty.clone(), ctx.env.len().into()) {
                    //TODO #19
                    return Err(format!(
                        "type mismatch\nexpected type:\n  {:?}\n\ninferred type:\n  {:?}\n",
                        vty, tty
                    ));
                };
                Ok(())
            }
        }
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

impl Val {
    // Computes universe the universe in which (x : A) -> B lives when A : u1 and B : u2
    fn imax(self, u2: Val) -> Result<Val, String> {
        match u2 {
            Prop => Ok(Prop), // Because Term::Prop is impredicative, if B : Term::Prop, then (x : A) -> b : Term::Prop
            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),
                // else if u1 = Term::Type(i) and u2 = Term::Type(j), then (x : A) -> B : Term::Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j))),
                //TODO #19
                _ => Err(format!("Expected universe, found {:?}", u2.clone())),
            },
            //TODO #19
            _ => Err(format!("Expected universe, found {:?}", self)),
        }
    }

    pub fn infer(self, ctx: &Ctx) -> Result<Val, String> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[i].clone()),
            Prod(_, box a, c) => {
                let ua = a.clone().infer(ctx)?;
                if !ua.is_universe() {
                    //TODO #19
                    Err(format!("   {:?}\n Is not a type.", ua))
                } else {
                    let ctx2 = ctx.clone().define(a, ua.clone());
                    let ub = c.term.eval(&ctx2.env).infer(&ctx2)?;
                    if !ub.is_universe() {
                        //TODO #19
                        Err(format!("   {:?}\n Is not a type.", ub))
                    } else {
                        ua.imax(ub)
                    }
                }
            }
            Abs(s, box t1, c) => {
                let ctx2 = ctx.clone().bind(t1.clone());
                Ok(Prod(
                    s,
                    box t1.infer(ctx)?,
                    Closure {
                        env: ctx2.env.clone(),
                        term: c.term.eval(&ctx2.env).infer(&ctx2)?.into(),
                    },
                ))
            }
            App(box a, box b) => {
                if let Prod(_, box t1, cls) = a.clone().infer(ctx)? {
                    let t1_ = b.clone().infer(ctx);
                    if !t1.clone().conversion(t1_.clone()?, ctx.env.len().into()) {
                        //TODO #19
                        return Err(format!("Wrong argument, function\n  {:?}\n expected argument of type\n  {:?}\n but term\n    {:?}\n is of type\n    {:?}",a,t1,b,t1_));
                    };
                    Ok(cls.term.eval(&cls.env))
                } else {
                    //TODO #19
                    Err(format!("\n    {:?}\nIs not a function, hence argument \n    {:?}\ncan't be given to it",a,b))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::type_checker::*;

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
        let v1 = t1.clone().eval(&Vec::new());
        assert_eq!(v1.clone().conversion(t2.eval(&Vec::new()), 0.into()), true);
        let ty = v1.infer(&Ctx::new());
        assert_eq!(ty, Ok(Type(BigUint::from(0_u64).into())));
    }

    #[test]
    fn simple_subst() {
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

        assert_eq!(Term::is_def_eq(term.clone(), reduced), Ok(()));
        let v1 = term.clone().eval(&Vec::new());
        let _ty = v1.infer(&Ctx::new());
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
        assert_eq!(Term::is_def_eq(term, reduced), Ok(()))
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
        let nff = reduced.clone().normal_form(Vec::new());
        assert_eq!(reduced.clone(), nff.clone());
        assert_eq!(Term::is_def_eq(reduced, nff), Ok(()));
    }

    #[test]
    fn polymorphism() {
        let id = Term::Abs(
            "A".into(),
            box Term::Type(BigUint::from(0_u64).into()),
            box Term::Abs("x".into(), box Term::Var(0.into()), box Term::Var(1.into())),
        );
        assert_eq!(
            matches!(id.eval(&Vec::new()).infer(&Ctx::new()), Ok(_)),
            true
        )
    }
}
