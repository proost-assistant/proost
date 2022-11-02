use crate::environment::{Environment, EnvironmentError};
use crate::error::{Error, Result};
use crate::term::{DeBruijnIndex, Term};
use derive_more::Display;
use num_bigint::BigUint;
use std::cmp::max;
use std::ops::Index;
use Term::*;

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{} : {}", _0, _1)]
pub struct TypedTerm(Term, Term);

/// Errors that can occur, at runtime, during type checking.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum TypeCheckerError {
    /// t is not a universe
    #[display(fmt = "{} is not a universe", _0)]
    NotUniverse(Term),

    /// t1 and t2 are not definitionally equal
    #[display(fmt = "{} and {} are not definitionaly equal", _0, _1)]
    NotDefEq(Term, Term),

    /// f of type t1 cannot be applied to x of type t2
    #[display(fmt = "{} cannot be applied to {}", _0, _1)]
    WrongArgumentType(TypedTerm, TypedTerm),

    /// t1 of type ty is not a function so cannot be applied to t2
    #[display(fmt = "{} is not a function so cannot be applied to {}", _0, _1)]
    NotAFunction(TypedTerm, Term),

    /// Expected ty1, found ty2
    #[display(fmt = "expected {}, got {}", _0, _1)]
    TypeMismatch(Term, Term),
}

/// Type of lists of tuples representing the respective types of each variables
type Types = Vec<Term>;

impl Index<DeBruijnIndex> for Types {
    type Output = Term;

    fn index(&self, idx: DeBruijnIndex) -> &Self::Output {
        &self[usize::from(idx)]
    }
}

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
            (Prop, Prop) => true,

            (Type(i), Type(j)) => i == j,

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
            // TODO: Unused code (#34)
            // (app @ App(box Abs(_, _), box _), u) | (u, app @ App(box Abs(_, _), box _)) => {
            //     app.beta_reduction(env).conversion(&u, env, lvl)
            // }
            _ => false,
        }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(&self, rhs: &Term, env: &Environment) -> Result<()> {
        self.conversion(rhs, env, 1.into())
            .then_some(())
            .ok_or(Error {
                kind: TypeCheckerError::NotDefEq(self.clone(), rhs.clone()).into(),
            })
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(&self, rhs: &Term) -> Result<Term> {
        match rhs {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
            Prop => Ok(Prop),

            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j.clone()))),

                _ => Err(Error {
                    kind: TypeCheckerError::NotUniverse(self.clone()).into(),
                }),
            },

            _ => Err(Error {
                kind: TypeCheckerError::NotUniverse(rhs.clone()).into(),
            }),
        }
    }

    fn _infer(&self, env: &Environment, ctx: &mut Context) -> Result<Term> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i.clone() + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[ctx.lvl - *i].clone()),

            Const(s) => env.get_type(s).ok_or(Error {
                kind: EnvironmentError::VariableNotFound(s.clone()).into(),
            }),

            Prod(box t, u) => {
                let univ_t = t._infer(env, ctx)?;
                let univ_u = u._infer(env, ctx.clone().bind(t))?;
                // TODO: lax normalization to whnf once it itself is laxed (#34)
                univ_t.normal_form(env).imax(&univ_u.normal_form(env))
            }

            Abs(box t, u) => {
                let univ_binder = t._infer(env, ctx)?.normal_form(env);
                if !matches!(univ_binder, Type(_) | Prop) {
                    return Err(KernelError::NotUniverse(univ_binder));
                }
                let u = u._infer(env, ctx.clone().bind(&t.shift(1, 0)))?;

                Ok(Prod(box t.clone(), box u))
            }

            App(box t, box u) => match t._infer(env, ctx)? {
                Prod(box typ_lhs, cls) => {
                    let typ_rhs = u._infer(env, ctx)?;

                    typ_lhs
                        .conversion(&typ_rhs, env, ctx.types.len().into())
                        .then_some(*cls)
                        .ok_or(Error {
                            kind: TypeCheckerError::WrongArgumentType(
                                TypedTerm(t.clone(), typ_lhs),
                                TypedTerm(u.clone(), typ_rhs),
                            )
                            .into(),
                        })
                }

                x => Err(Error {
                    kind: TypeCheckerError::NotAFunction(TypedTerm(t.clone(), x), u.clone()).into(),
                }),
            },
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(&self, env: &Environment) -> Result<Term> {
        self._infer(env, &mut Context::new())
    }

    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(&self, ty: &Term, env: &Environment) -> Result<()> {
        let ctx = &mut Context::new();
        let tty = self._infer(env, ctx)?;

        tty.conversion(ty, env, ctx.types.len().into())
            .then_some(())
            .ok_or(Error {
                kind: TypeCheckerError::TypeMismatch(tty, ty.clone()).into(),
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn proj(idx: usize) -> Box<Term> {
        box Abs(box Prop, box Var(idx.into()))
    }

    #[test]
    fn def_eq_1() {
        let term = App(box Abs(box Prop, proj(1)), proj(1));
        let normal_form = Abs(box Prop, box Var(1.into()));

        assert!(term.is_def_eq(&normal_form, &Environment::new()).is_ok());
    }

    #[test]
    fn def_eq_2() {
        let term = App(box Abs(box Prop, proj(2)), proj(1));
        let normal_form = Abs(box Prop, proj(1));

        assert!(term.is_def_eq(&normal_form, &Environment::new()).is_ok());
    }

    #[test]
    fn def_eq_self() {
        // λa.a (λx.x) (λx.x)
        let term = Abs(
            box Prop,
            box App(box App(box Var(2.into()), proj(1)), proj(1)),
        );

        assert!(term.is_def_eq(&term, &Environment::new()).is_ok());
    }

    #[test]
    fn failed_def_equal() {
        let term_lhs = Prop;
        let term_rhs = Type(BigUint::from(0_u64).into());

        assert_eq!(
            term_lhs.is_def_eq(&term_rhs, &Environment::new()),
            Err(Error {
                kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
            })
        );
    }

    #[test]
    fn failed_prod_binder_conversion() {
        let term_lhs = Prod(box Prop, box Prop);
        let term_rhs = Prod(box Type(BigUint::from(0_u64).into()), box Prop);

        assert_eq!(
            term_lhs.is_def_eq(&term_rhs, &Environment::new()),
            Err(Error {
                kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
            })
        );
    }

    #[test]
    fn failed_app_head_conversion() {
        let term_lhs = Abs(
            box Type(BigUint::from(0_u64).into()),
            box Abs(
                box Type(BigUint::from(0_u64).into()),
                box App(box Var(1.into()), box Prop),
            ),
        );

        let term_rhs = Abs(
            box Type(BigUint::from(0_u64).into()),
            box Abs(
                box Type(BigUint::from(0_u64).into()),
                box App(box Var(2.into()), box Prop),
            ),
        );

        assert_eq!(
            term_lhs.is_def_eq(&term_rhs, &Environment::new()),
            Err(Error {
                kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
            })
        );
    }

    #[test]
    fn typed_reduction_app_1() {
        let term = App(
            box Abs(box Type(BigUint::from(0_u64).into()), box Var(1.into())),
            box Prop,
        );

        let reduced = Prop;
        assert!(term.is_def_eq(&reduced, &Environment::new()).is_ok());

        let term_type = term.infer(&Environment::new()).unwrap();
        assert_eq!(term_type, Type(BigUint::from(0_u64).into()));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_reduction_app_2() {
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
                                proj(2),
                            ),
                            // d : P
                            proj(1),
                        ),
                    ),
                ),
            ),
            box Abs(
                // a : (P -> P) -> (P -> P) -> P
                box Prod(
                    box Prod(box Prop, box Prop),
                    box Prod(box Prod(box Prop, box Prop), box Prop),
                ),
                box Abs(
                    // b : P -> P
                    box Prod(box Prop, box Prop),
                    box App(box Var(2.into()), box Var(1.into())),
                ),
            ),
        );

        // λa : P.λb : P .b
        let reduced = Abs(box Prop, proj(1));
        assert!(term.is_def_eq(&reduced, &Environment::new()).is_ok());

        let term_type = term.infer(&Environment::new()).unwrap();
        assert_eq!(term_type, Prod(box Prop, box Prod(box Prop, box Prop)));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_reduction_universe() {
        let term = App(
            box Abs(box Prop, box Type(BigUint::from(0_u64).into())),
            box Prod(box Prop, box Var(1.into())),
        );

        let reduced = Type(BigUint::from(0_u64).into());
        assert!(term.is_def_eq(&reduced, &Environment::new()).is_ok());

        let term_type = term.infer(&Environment::new()).unwrap();
        assert_eq!(term_type, Type(BigUint::from(1_u64).into()));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn escape_from_prop() {
        let term = Abs(
            box Prop,
            box Prod(box Var(1.into()), box Type(BigUint::from(0_u64).into())),
        );

        let term_type = term.infer(&Environment::new()).unwrap();
        assert_eq!(
            term_type,
            Prod(box Prop, box Type(BigUint::from(1_u64).into()))
        );
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn illtyped_reduction() {
        // λ (x : P -> P).(λ (y :P).x y) x
        // λ P -> P.(λ P.2 1) 1
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
    }

    #[test]
    fn typed_prod_1() {
        let term = Prod(box Prop, box Prop);
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(term_type, Type(BigUint::from(0_u64).into()));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_prod_2() {
        let term = Prod(box Prop, box Var(1.into()));
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(term_type, Prop);
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_prod_3() {
        let term = Abs(box Prop, box Abs(box Var(1.into()), box Var(1.into())));
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(
            term_type,
            Prod(box Prop, box Prod(box Var(1.into()), box Var(2.into())))
        );
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_polymorphism() {
        let identity = Abs(
            box Type(BigUint::from(0_u64).into()),
            box Abs(box Var(1.into()), box Var(1.into())),
        );
        let identity_type = identity.infer(&Environment::new()).unwrap();

        assert_eq!(
            identity_type,
            Prod(
                box Type(BigUint::from(0_u64).into()),
                box Prod(box Var(1.into()), box Var(2.into()))
            )
        );
        assert!(identity.check(&identity_type, &Environment::new()).is_ok());
    }

    #[test]
    fn typed_polymorphism_2() {
        let term = Abs(
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
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(
            term_type,
            Prod(
                box Prop,
                box Prod(
                    box Prop,
                    box Prod(box Prod(box Prop, box Prod(box Prop, box Prop)), box Prop)
                )
            )
        );
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn type_hierarchy_prop() {
        let term = Prop;
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(term_type, Type(BigUint::from(0_u64).into()));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn type_hierarchy_type() {
        let term = Type(BigUint::from(0_u64).into());
        let term_type = term.infer(&Environment::new()).unwrap();

        assert_eq!(term_type, Type(BigUint::from(1_u64).into()));
        assert!(term.check(&term_type, &Environment::new()).is_ok());
    }

    #[test]
    fn environment() {
        let term = Abs(box Prop, box Abs(box Var(1.into()), box Var(1.into())));
        let term_type = term.infer(&Environment::new()).unwrap();

        let context = Prod(box Prop, box Prod(box Var(1.into()), box Var(2.into())));
        let mut env = Environment::new();
        env.insert("foo".into(), context, Prop).unwrap();

        assert_eq!(
            term_type,
            Prod(box Prop, box Prod(box Var(1.into()), box Var(2.into())))
        );
        assert!(term.check(&Const("foo".into()), &env).is_ok());
    }

    mod failed_type_inference {
        use super::*;

        #[test]
        fn not_function() {
            let term = App(box Prop, box Prop);

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop
                    )
                    .into()
                })
            );
        }

        #[test]
        fn not_function_prod_1() {
            let term = Prod(box Prop, box App(box Prop, box Prop));

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop
                    )
                    .into()
                })
            );
        }

        #[test]
        fn not_function_prod_2() {
            let term = Prod(box App(box Prop, box Prop), box Prop);

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop
                    )
                    .into()
                })
            );
        }

        #[test]
        fn not_function_app_1() {
            let term = App(box Abs(box Prop, box Prop), box App(box Prop, box Prop));

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop
                    )
                    .into()
                })
            );
        }

        #[test]
        fn not_function_app_2() {
            let term = App(box App(box Prop, box Prop), box Prop);

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop
                    )
                    .into()
                })
            );
        }

        #[test]
        fn wrong_argument_type() {
            // λ (x : P -> P).(λ (y :P).x y) x
            // λ P -> P.(λ P.2 1) 1
            let term = Abs(
                box Prod(box Prop, box Prop),
                box App(
                    box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                    box Var(1.into()),
                ),
            );

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::WrongArgumentType(
                        TypedTerm(
                            Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                            Prop
                        ),
                        TypedTerm(Var(1.into()), Prod(box Prop, box Prop))
                    )
                    .into()
                })
            );
        }

        #[test]
        fn not_universe_1() {
            let term = Prod(proj(1), box Prop);

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotUniverse(Prod(box Prop, box Prop)).into()
                })
            );
        }

        #[test]
        fn not_universe_2() {
            let term = Prod(box Prop, box Abs(box Prop, box Prop));

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotUniverse(Prod(
                        box Prop,
                        box Type(BigUint::from(0_u64).into())
                    ))
                    .into()
                })
            );
        }

        #[test]
        fn const_not_found() {
            let term = Const("foo".to_string());

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: EnvironmentError::VariableNotFound("foo".to_string()).into()
                })
            );
        }

        #[test]
        fn check_fail_1() {
            let term = App(box Prop, box Prop);

            assert_eq!(
                term.check(&Prop, &Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotAFunction(
                        TypedTerm(Prop, Type(BigUint::from(0_u64).into())),
                        Prop,
                    )
                    .into(),
                })
            );
        }

        #[test]
        fn check_fail_2() {
            assert_eq!(
                Prop.check(&Prop, &Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::TypeMismatch(Type(BigUint::from(0_u64).into()), Prop)
                        .into(),
                })
            );
        }
    }

    #[test]
    fn univ_in_binder() {
        let term = Abs(
            box Prop,
            box Abs(
                box Var(1.into()),
                box Abs(box Var(1.into()), box Var(1.into())),
            ),
        );
        assert!(term.infer(&Environment::new()).is_err())
    }
}
