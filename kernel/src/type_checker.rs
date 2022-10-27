use crate::environment::Environment;
use crate::error::KernelError;
use crate::term::{DeBruijnIndex, Term};
use num_bigint::BigUint;
use std::cmp::max;
use std::ops::Index;
use Term::*;

impl Index<DeBruijnIndex> for Vec<Term> {
    type Output = Term;

    fn index(&self, idx: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(idx)]
    }
}

/// Type of lists of tuples representing the respective types of each variables
type Types = Vec<Term>;

/// Structure containing a context used for typechecking.
///
/// It serves to store the types of variables in the following way:
/// In a given context {types, lvl}, the type of `Var(i)` is in `types[lvl - i]`.
#[derive(Clone, Debug, Default)]
struct Context {
    types: Types,
    lvl: DeBruijnIndex,
}

impl Context {
    /// Creates a new empty `Context`.
    fn new() -> Self {
        Self::default()
    }

    /// Extends the actual context with a bound variable of type `ty`.
    fn bind(&mut self, ty: &Term) -> &mut Self {
        self.types.push(ty.clone());
        self.lvl = self.lvl + 1.into();
        self
    }
}

impl Term {
    /// Conversion function, checks whether two terms are definitionally equal.
    ///
    /// The conversion is untyped, meaning that it should **only** be called during type-checking when the two `Term`s are already known to be of the same type and in the same context.
    fn conversion(&self, rhs: &Term, env: &Environment, lvl: DeBruijnIndex) -> bool {
        match (self.whnf(env), rhs.whnf(env)) {
            (Type(i), Type(j)) => i == j,

            (Prop, Prop) => true,

            (Var(i), Var(j)) => i == j,

            (Prod(t1, u1), Prod(box t2, u2)) => {
                let u1 = u1.substitute(&Var(lvl), lvl.into());
                let u2 = u2.substitute(&Var(lvl), lvl.into());

                t1.conversion(&t2, env, lvl) && u1.conversion(&u2, env, lvl + 1.into())
            }

            // Since we assume that both vals already have the same type,
            // checking conversion over the argument type is useless.
            // However, this doesn't mean we can simply remove the arg type
            // from the type constructor in the enum, it is needed to quote back to terms.
            (Abs(_, t), Abs(_, u)) => {
                let t = t.substitute(&Var(lvl), lvl.into());
                let u = u.substitute(&Var(lvl), lvl.into());

                t.conversion(&u, env, lvl + 1.into())
            }

            (App(box t1, box u1), App(box t2, box u2)) => {
                t1.conversion(&t2, env, lvl) && u1.conversion(&u2, env, lvl)
            }

            _ => false,
        }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(&self, rhs: &Term, env: &Environment) -> Result<(), KernelError> {
        self.conversion(rhs, env, 1.into())
            .then_some(())
            .ok_or_else(|| KernelError::NotDefEq(self.clone(), rhs.clone()))
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(&self, rhs: &Term) -> Result<Term, KernelError> {
        println!("{:?}, {:?}", self, rhs);

        match rhs {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
            Prop => Ok(Prop),

            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j.clone()))),

                _ => Err(KernelError::NotUniverse(self.clone())),
            },

            _ => Err(KernelError::NotUniverse(rhs.clone())),
        }
    }

    fn _infer(&self, env: &Environment, ctx: &mut Context) -> Result<Term, KernelError> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i.clone() + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[ctx.lvl - *i].clone()),

            Const(s) => match env.get_type(s) {
                Some(ty) => Ok(ty),
                None => Err(KernelError::ConstNotFound(s.clone())),
            },

            Prod(box t, u) => {
                let univ_t = t._infer(env, ctx)?;
                let univ_u = u._infer(env, ctx.clone().bind(t))?;

                univ_t.imax(&univ_u)
            }

            Abs(box t, u) => {
                let u = u._infer(env, ctx.clone().bind(t))?;

                Ok(Prod(box t.clone(), box u))
            }

            App(box t, box u) => match t._infer(env, ctx)? {
                Prod(box typ_lhs, cls) => {
                    let typ_rhs = u._infer(env, ctx)?;

                    typ_lhs
                        .conversion(&typ_rhs, env, ctx.types.len().into())
                        .then_some(*cls)
                        .ok_or_else(|| {
                            KernelError::WrongArgumentType(t.clone(), typ_lhs, u.clone(), typ_rhs)
                        })
                }

                x => Err(KernelError::NotAFunction(t.clone(), x, u.clone())),
            },
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(&self, env: &Environment) -> Result<Term, KernelError> {
        self._infer(env, &mut Context::new())
    }

    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(&self, ty: &Term, env: &Environment) -> Result<(), KernelError> {
        let ctx = &mut Context::new();
        let tty = self._infer(env, ctx)?;

        tty.conversion(ty, env, ctx.types.len().into())
            .then_some(())
            .ok_or_else(|| KernelError::TypeMismatch(tty, ty.clone()))
    }
}

#[cfg(test)]
mod tests {
    use crate::type_checker::*;

    fn id(l: usize) -> Box<Term> {
        box Abs(box Prop, box Var(l.into()))
    }

    #[test]
    fn var_subst_1() {
        // (λ P. λP. 1) (λP.1)
        let t = App(
            box Abs(box Prop, box Abs(box Prop, box Var(1.into()))),
            id(1),
        );

        let nf = Abs(box Prop, box Var(1.into()));
        assert_eq!(t.normal_form(&Environment::new()), nf)
    }

    #[test]
    fn var_subst_2() {
        let t = App(
            box Abs(box Prop, box Abs(box Prop, box Var(2.into()))),
            id(1),
        );

        let nf = Abs(box Prop, id(1));
        assert_eq!(t.normal_form(&Environment::new()), nf)
    }

    #[test]
    fn simple() {
        let t1 = App(
            box Abs(box Type(BigUint::from(0_u64).into()), box Var(1.into())),
            box Prop,
        );

        let t2 = Prop;
        assert!(t1.conversion(&t2, &Environment::new(), 0.into()));

        let ty = t1.infer(&Environment::new());
        assert_eq!(ty, Ok(Type(BigUint::from(0_u64).into())));
    }

    #[test]
    fn simple_substitute() {
        // λ (x : P -> P).(λ (y :P).x y) x
        //λ P -> P.(λ P.2 1) 1
        let term = Abs(
            box Prod(box Prop, box Prop),
            box App(
                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                box Var(1.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(1.into()), box Var(1.into())));
        assert!(term.is_def_eq(&reduced, &Environment::new()).is_ok());
        assert!(term.infer(&Environment::new()).is_err());
    }

    #[test]
    fn complex_conv() {
        // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
        let term = App(
            box Abs(
                // a : ((P → P) → (P → P) → P) → ((P → P) → ((P → P) → P))
                box Prod(
                    // (P → P) → ((P → P) → P)
                    box Prod(
                        // P -> P
                        box Prod(box Prop, box Prop),
                        // (P -> P) -> P
                        box Prod(box Prod(box Prop, box Prop), box Prop),
                    ),
                    // (P → P) → ((P → P) → P)
                    box Prod(
                        // P -> P
                        box Prod(box Prop, box Prop),
                        // (P -> P) -> P
                        box Prod(box Prod(box Prop, box Prop), box Prop),
                    ),
                ),
                box Abs(
                    // b : P
                    box Prop,
                    box Abs(
                        // c : P
                        box Prop,
                        box App(
                            box App(
                                box App(
                                    box Var(3.into()),
                                    box Abs(
                                        // d : P -> P
                                        box Prod(box Prop, box Prop),
                                        box Abs(
                                            // e : P -> P
                                            box Prod(box Prop, box Prop),
                                            box App(
                                                box Var(1.into()),
                                                box App(box Var(2.into()), box Var(4.into())),
                                            ),
                                        ),
                                    ),
                                ),
                                // _ : P
                                box Abs(box Prop, box Var(2.into())),
                            ),
                            //d : P
                            box Abs(box Prop, box Var(1.into())),
                        ),
                    ),
                ),
            ),
            box Abs(
                //a : (P -> P) -> (P -> P) -> P
                box Prod(
                    box Prod(box Prop, box Prop),
                    box Prod(box Prod(box Prop, box Prop), box Prop),
                ),
                box Abs(
                    //b : P -> P
                    box Prod(box Prop, box Prop),
                    box App(box Var(2.into()), box Var(1.into())),
                ),
            ),
        );

        // λa : P.λb : P .b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1.into())));
        assert!(term.is_def_eq(&reduced, &Environment::new()).is_ok());
        assert!(term.infer(&Environment::new()).is_ok())
    }

    //(λ ℙ → λ ℙ → λ ℙ → (0 (λ ℙ → λ ℙ → ((4 1) 3) λ ℙ → 3)) (λ ℙ → λ ℙ → (0 1) (λ ℙ → 0 λ ℙ → 0)))
    #[test]
    fn nf_test() {
        //λa.a (λx.x) (λx.x)
        let reduced = Abs(box Prop, box App(box App(box Var(2.into()), id(1)), id(1)));

        let nff = reduced.clone().normal_form(&Environment::new());
        assert_eq!(reduced, nff);
        assert!(reduced.is_def_eq(&nff, &Environment::new()).is_ok());
    }

    #[test]
    fn polymorphism() {
        let id = Abs(
            box Type(BigUint::from(0_u64).into()),
            box Abs(box Var(1.into()), box Var(1.into())),
        );

        assert!(id.infer(&Environment::new()).is_ok());
    }

    #[test]
    fn type_type() {
        assert!(Type(BigUint::from(0_u64).into())
            .check(&Type(BigUint::from(1_u64).into()), &Environment::new())
            .is_ok());
    }

    #[test]
    fn not_function() {
        let t = App(box Prop, box Prop);
        assert!(t.infer(&Environment::new()).is_err())
    }

    #[test]
    fn not_type_prod() {
        let t = Prod(box Abs(box Prop, box Var(1.into())), box Prop);
        assert!(t.infer(&Environment::new()).is_err());

        let t = Prod(box Prop, box Abs(box Prop, box Prop));
        assert!(t.infer(&Environment::new()).is_err());

        let wf_prod = Prod(box Prop, box Prop);
        assert!(wf_prod
            .check(&Type(BigUint::from(0_u64).into()), &Environment::new())
            .is_ok());

        let wf_prod = Prod(box Prop, box Var(1.into()));
        assert!(wf_prod.check(&Prop, &Environment::new()).is_ok());

        // Type0 -> (A : Prop) ->
        let wf_prod = Prod(box Prop, box Prop);
        assert!(wf_prod
            .check(&Type(BigUint::from(0_u64).into()), &Environment::new())
            .is_ok());
    }

    #[test]
    fn poly_test2() {
        let t = Abs(
            box Prop,
            box Abs(
                box Prop,
                box Abs(
                    box Prod(box Prop, box Prod(box Prop, box Prop)),
                    box App(
                        box App(box Var(1.into()), box Var(3.into())),
                        box Var(2.into()),
                    ),
                ),
            ),
        );

        assert!(t.infer(&Environment::new()).is_ok());
    }

    #[test]
    fn env_test() {
        let id_prop = Prod(box Prop, box Prod(box Var(1.into()), box Var(1.into())));
        let t = Abs(box Prop, box Abs(box Var(1.into()), box Var(1.into())));
        let mut env = Environment::new();
        env.insert("foo".into(), id_prop.clone(), Prop).unwrap();

        assert!(t.check(&Const("foo".into()), &env).is_ok());
        assert!(id_prop.is_def_eq(&Const("foo".into()), &env).is_ok());
    }

    #[test]
    fn check_bad() {
        assert!(Prop.check(&Prop, &Environment::new()).is_err());
    }

    #[test]
    fn not_def_eq() {
        assert!(Prop
            .is_def_eq(&Type(BigUint::from(0_u64).into()), &Environment::new())
            .is_err());
    }

    #[test]
    fn infer_const() {
        let id_prop = Prod(box Prop, box Prod(box Var(1.into()), box Var(1.into())));
        let mut env = Environment::new();
        env.insert("foo".into(), id_prop, Prop).unwrap();

        assert!(Const("foo".into()).infer(&env).is_ok());
        assert!(Const("foo".into()).infer(&Environment::new()).is_err());
    }
}
