use crate::environment::Environment;
use crate::term::{DeBruijnIndex, Term};
use num_bigint::BigUint;
use std::cmp::max;
use std::ops::Index;
use Term::*;

impl Index<DeBruijnIndex> for Vec<Term> {
    type Output = Term;

    fn index(&self, i: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(i)]
    }
}

// TODO #19
/// Type representing kernel errors, is used by the toplevel to pretty-print errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeCheckingError {
    // Constant s has not been found in the current context
    ConstNotFound(String),

    /// t is not a universe
    NotUniverse(Term),

    /// t is not a type
    NotType(Term),

    /// t1 and t2 are not definitionally equal
    NotDefEq(Term, Term),

    /// f of type t1 can't take argument x of type t2
    WrongArgumentType(Term, Term, Term, Term),

    /// t1 is of type ty is not a function, and thus cannot be applied to t2
    NotAFunction(Term, Term, Term),

    /// Expected ty1, found ty2
    TypeMismatch(Term, Term),
}

use TypeCheckingError::*;

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
    fn conversion(&self, rhs: &Term, lvl: DeBruijnIndex, env: &Environment) -> bool {
        match (self.whnf(env), rhs.whnf(env)) {
            (Type(i), Type(j)) => i == j,

            (Prop, Prop) => true,

            (Var(i), Var(j)) => i == j,

            (Prod(a1, b1), Prod(box a2, b2)) => {
                let b1 = b1.substitute(&Var(lvl), lvl.into());
                let b2 = b2.substitute(&Var(lvl), lvl.into());

                a1.conversion(&a2, lvl, env) && b1.conversion(&b2, lvl + 1.into(), env)
            }

            // Since we assume that both vals already have the same type,
            // checking conversion over the argument type is useless.
            // However, this doesn't mean we can simply remove the arg type
            // from the type constructor in the enum, it is needed to quote back to terms.
            (Abs(_, t), Abs(_, u)) => {
                let t = t.substitute(&Var(lvl), lvl.into());
                let u = u.substitute(&Var(lvl), lvl.into());

                t.conversion(&u, lvl + 1.into(), env)
            }

            (App(box t1, box u1), App(box t2, box u2)) => {
                t1.conversion(&t2, lvl, env) && u1.conversion(&u2, lvl, env)
            }

            _ => false,
        }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(&self, rhs: &Term, env: &Environment) -> Result<(), TypeCheckingError> {
        if !self.conversion(rhs, 1.into(), env) {
            Err(NotDefEq(self.clone(), rhs.clone()))
        } else {
            Ok(())
        }
    }

    fn is_universe(&self) -> bool {
        matches!(*self, Prop | Type(_))
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(&self, u2: &Term) -> Result<Term, TypeCheckingError> {
        match u2 {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
            Prop => Ok(Prop),

            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j.clone()))),

                _ => Err(NotUniverse(u2.clone())),
            },

            _ => Err(NotUniverse(self.clone())),
        }
    }

    fn _infer(&self, env: &Environment, ctx: &mut Context) -> Result<Term, TypeCheckingError> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i.clone() + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[ctx.lvl - *i].clone()),

            Const(s) => match env.get_type(s) {
                Some(ty) => Ok(ty),
                None => Err(ConstNotFound(s.clone())),
            },

            Prod(box a, c) => {
                let ua = a._infer(env, ctx)?;

                if !ua.is_universe() {
                    Err(NotType(ua))
                } else {
                    let ub = c._infer(env, ctx.clone().bind(a))?;

                    if !ub.is_universe() {
                        Err(NotType(ub))
                    } else {
                        ua.imax(&ub)
                    }
                }
            }

            Abs(box t1, c) => Ok(Prod(
                box t1.clone(),
                box c._infer(env, ctx.clone().bind(t1))?,
            )),

            App(box a, box b) => {
                let type_a = a._infer(env, ctx)?;

                match type_a {
                    Prod(box t1, cls) => {
                        let t1_ = b._infer(env, ctx)?;

                        if !t1.conversion(&t1_, ctx.types.len().into(), env) {
                            return Err(WrongArgumentType(a.clone(), t1, b.clone(), t1_));
                        };

                        Ok(*cls)
                    }

                    _ => Err(NotAFunction(a.clone(), type_a, b.clone())),
                }
            }
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(&self, env: &Environment) -> Result<Term, TypeCheckingError> {
        self._infer(env, &mut Context::new())
    }

    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(&self, ty: &Term, env: &Environment) -> Result<(), TypeCheckingError> {
        let ctx = &mut Context::new();
        let tty = self._infer(env, ctx)?;

        if !tty.conversion(ty, ctx.types.len().into(), env) {
            return Err(TypeMismatch(ty.clone(), tty));
        };
        Ok(())
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
        assert!(t1.conversion(&t2, 0.into(), &Environment::new()));

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
        assert_eq!(term.is_def_eq(&reduced, &Environment::new()), Ok(()));

        let ty = term.infer(&Environment::new());
        assert!(matches!(ty, Err(WrongArgumentType(_, _, _, _))));
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
        let env = Environment::new()
            .insert("foo".into(), id_prop.clone(), Prop)
            .unwrap();

        assert!(t.check(&Const("foo".into()), &env).is_ok());
        assert!(id_prop.conversion(&Const("foo".into()), 1.into(), &env));
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
        let env = Environment::new()
            .insert("foo".into(), id_prop, Prop)
            .unwrap();

        assert!(Const("foo".into()).infer(&env).is_ok());
        assert!(Const("foo".into()).infer(&Environment::new()).is_err());
    }
}
