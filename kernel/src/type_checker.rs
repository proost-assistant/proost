use crate::error::{Error, Result};
use crate::term::{Arena, Payload, Term};
use derive_more::Display;
use num_bigint::BigUint;
use std::cmp::max;
use Payload::*;

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}: {}", _0, _1)]
pub struct TypedTerm<'arena>(Term<'arena>, Term<'arena>);

/// Errors that can occur, at runtime, during type checking.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum TypeCheckerError<'arena> {
    /// t is not a universe
    #[display(fmt = "{} is not a universe", _0)]
    NotUniverse(Term<'arena>),

    /// t1 and t2 are not definitionally equal
    #[display(fmt = "{} and {} are not definitionaly equal", _0, _1)]
    NotDefEq(Term<'arena>, Term<'arena>),

    /// f of type t1 cannot be applied to x of type t2
    #[display(fmt = "{} cannot be applied to {}", _0, _1)]
    WrongArgumentType(TypedTerm<'arena>, TypedTerm<'arena>),

    /// t1 of type ty is not a function so cannot be applied to t2
    #[display(fmt = "{} is not a function, it cannot be applied to {}", _0, _1)]
    NotAFunction(TypedTerm<'arena>, Term<'arena>),

    /// Expected ty1, found ty2
    #[display(fmt = "expected {}, got {}", _0, _1)]
    TypeMismatch(Term<'arena>, Term<'arena>),
}

impl<'arena> Arena<'arena> {
    /// Conversion function, checks whether two terms are definitionally equal.
    ///
    /// The conversion is untyped, meaning that it should **only** be called during type-checking when the two `Term`s are already known to be of the same type and in the same context.
    fn conversion(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> bool {
        let lhs = self.whnf(lhs);
        let rhs = self.whnf(rhs);
        lhs == rhs

        // TODO: Unused code (#34)
        // (app @ App(box Abs(_, _), box _), u) | (u, app @ App(box Abs(_, _), box _)) => {
        //     app.beta_reduction(env).conversion(&u, env)
        // }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> Result<()> {
        self.conversion(lhs, rhs)
            .then_some(())
            .ok_or(Error {
                kind: TypeCheckerError::NotDefEq(lhs, rhs).into(),
            })
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> Result<Term<'arena>> {
        match *rhs {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> B : Prop
            Prop => Ok(self.prop()),

            Type(i) => match *lhs {
                Prop => Ok(self.type_(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(self.type_(max(i, j).clone())),

                _ => Err(Error {
                    kind: TypeCheckerError::NotUniverse(lhs).into(),
                }),
            },

            _ => Err(Error {
                kind: TypeCheckerError::NotUniverse(rhs).into(),
            }),
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(&mut self, t: Term<'arena>) -> Result<Term<'arena>> {
        t.get_type_or_try_init(|| {
        match *t {
            Prop => Ok(self.type_(BigUint::from(0_u64).into())),
            Type(i) => Ok(self.type_(i.clone() + BigUint::from(1_u64).into())),
            Var(_, type_) => Ok(type_),

            Prod(t, u) => {
                let univ_t = self.infer(t)?;
                let univ_u = self.infer(u)?;

                // TODO: lax normalization to whnf once it is itself laxed (#34)
                let univ_u = self.normal_form(univ_u);
                let univ_t = self.normal_form(univ_t);
                self.imax(univ_u, univ_t)
            }

            Abs(t, u) => {
                let type_t = self.infer(t)?;
                if !matches!(*type_t, Type(_) | Prop) {
                    return Err(Error {
                        kind: TypeCheckerError::NotUniverse(univ_binder).into(),
                    });
                }
                let type_u = self.infer(u)?;
                Ok(self.prod(t, type_u))
            }

            App(t, u) => {
                let type_t = self.infer(t)?;
                match *type_t {
                Prod(arg_type, cls) => {
                    let type_u = self.infer(u)?;

                    if self.conversion(type_u, arg_type) {
                        Ok(self.substitute(cls, u, 1))
                    } else {
                        Err(Error {
                            kind: TypeCheckerError::WrongArgumentType(
                                TypedTerm(t, type_t),
                                TypedTerm(u, type_u),
                            )
                            .into(),
                        })
                    }
                }

                _ => Err(Error {
                    kind: TypeCheckerError::NotAFunction(TypedTerm(t, type_t), u).into(),
                }),
                }},
        }
        })
        }


    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(&mut self, t: Term<'arena>, ty: Term<'arena>) -> Result<()> {
        let tty = self.infer(t)?;

        self.conversion(tty, ty)
            .then_some(())
            .ok_or(Error {
                kind: TypeCheckerError::TypeMismatch(tty, ty).into(),
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
        assert!(term.check(&term_type, &Environment::new()).is_ok())
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
        fn not_function_abs() {
            let term = Abs(box App(box Prop, box Prop), box Prop);

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
        fn not_function_app_3() {
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
        fn not_universe_abs() {
            let term = Abs(
                box Prop,
                box Abs(
                    box Var(1.into()),
                    box Abs(box Var(1.into()), box Var(1.into())),
                ),
            );

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotUniverse(Var(2.into())).into()
                })
            );
        }

        #[test]
        fn not_universe_prod_1() {
            let term = Prod(proj(1), box Prop);

            assert_eq!(
                term.infer(&Environment::new()),
                Err(Error {
                    kind: TypeCheckerError::NotUniverse(Prod(box Prop, box Prop)).into()
                })
            );
        }

        #[test]
        fn not_universe_prod_2() {
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
}
