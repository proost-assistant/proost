//! Type checking functions
//!
//! The logical core of the kernel.

use std::cmp::max;

use derive_more::Display;
use num_bigint::BigUint;
use Payload::*;

use crate::error::{Error, Result, ResultTerm};
use crate::term::arena::{Arena, Payload, Term};

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}: {}", _0, _1)]
pub struct TypedTerm<'arena>(Term<'arena>, Term<'arena>);

/// Errors that can occur, at runtime, during type checking.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum TypeCheckerError<'arena> {
    /// `t` is not a universe
    #[display(fmt = "{} is not a universe", _0)]
    NotUniverse(Term<'arena>),

    /// `t1` and `t2` are not definitionally equal
    #[display(fmt = "{} and {} are not definitionally equal", _0, _1)]
    NotDefEq(Term<'arena>, Term<'arena>),

    /// function `f` expected a `t`, received `x: t`'
    #[display(fmt = "function {} expects a term of type {}, received {}", _0, _1, _2)]
    WrongArgumentType(Term<'arena>, Term<'arena>, TypedTerm<'arena>),

    /// `t1` of type `ty` is not a function so cannot be applied to `t2`
    #[display(fmt = "{} is not a function, it cannot be applied to {}", _0, _1)]
    NotAFunction(TypedTerm<'arena>, Term<'arena>),

    /// Expected `ty1`, found `ty2`
    #[display(fmt = "expected {}, got {}", _0, _1)]
    TypeMismatch(Term<'arena>, Term<'arena>),
}

impl<'arena> Arena<'arena> {
    /// Conversion function, checks whether two terms are definitionally equal.
    ///
    /// The conversion is untyped, meaning that it should **only** be called during type-checking
    /// when the two `Term`s are already known to be of the same type and in the same context.
    fn conversion(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> bool {
        lhs == rhs
            || match (&*self.whnf(lhs), &*self.whnf(rhs)) {
                (Prop, Prop) => true,

                (Type(i), Type(j)) => *i == *j,

                (Var(i, _), Var(j, _)) => i == j,

                (&Prod(t1, u1), &Prod(t2, u2)) => self.conversion(t1, t2) && self.conversion(u1, u2),

                // Since we assume that both values already have the same type,
                // checking conversion over the argument type is useless.
                // However, this doesn't mean we can simply remove the arg type
                // from the type constructor in the enum, it is needed to quote back to terms.
                (&Abs(_, t), &Abs(_, u)) => self.conversion(t, u),

                (&App(t1, u1), &App(t2, u2)) => self.conversion(t1, t2) && self.conversion(u1, u2),

                _ => false,
            }

        // TODO: Unused code (#34)
        // (app @ App(Abs(_, _), _), u) | (u, app @ App(Abs(_, _), _)) => {
        //     app.beta_reduction(env).conversion(&u, env)
        // }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> Result<'arena, ()> {
        self.conversion(lhs, rhs).then_some(()).ok_or(Error {
            kind: TypeCheckerError::NotDefEq(lhs, rhs).into(),
        })
    }

    /// Computes the universe in which `(x: A) -> B` lives when `A: lhs` and `B: rhs`.
    fn imax(&mut self, lhs: Term<'arena>, rhs: Term<'arena>) -> ResultTerm<'arena> {
        match *rhs {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> B : Prop
            Prop => Ok(self.prop()),

            Type(ref i) => match *lhs {
                Prop => Ok(self.type_(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(ref j) => Ok(self.type_(max(i, j).clone())),

                _ => Err(Error {
                    kind: TypeCheckerError::NotUniverse(lhs).into(),
                }),
            },

            _ => Err(Error {
                kind: TypeCheckerError::NotUniverse(rhs).into(),
            }),
        }
    }

    /// Infers the type of the term `t`, living in arena `self`.
    pub fn infer(&mut self, t: Term<'arena>) -> ResultTerm<'arena> {
        t.get_type_or_try_init(|| {
            match *t {
                Prop => Ok(self.type_usize(0)),
                Type(ref i) => Ok(self.type_(i.clone() + BigUint::from(1_u64).into())),
                Var(_, type_) => Ok(type_),

                Prod(t, u) => {
                    let univ_t = self.infer(t)?;
                    let univ_u = self.infer(u)?;

                    // TODO: relax normalization to whnf once it is itself relaxed (#34)
                    let univ_t = self.normal_form(univ_t);
                    let univ_u = self.normal_form(univ_u);
                    self.imax(univ_t, univ_u)
                },

                Abs(t, u) => {
                    let type_t = self.infer(t)?;
                    match *type_t {
                        Type(_) | Prop => {
                            let type_u = self.infer(u)?;
                            Ok(self.prod(t, type_u))
                        },
                        _ => Err(Error {
                            kind: TypeCheckerError::NotUniverse(type_t).into(),
                        }),
                    }
                },

                App(t, u) => {
                    let type_t = self.infer(t)?;
                    let type_t = self.whnf(type_t);
                    match *type_t {
                        Prod(arg_type, cls) => {
                            let type_u = self.infer(u)?;

                            if self.conversion(type_u, arg_type) {
                                Ok(self.substitute(cls, u, 1))
                            } else {
                                Err(Error {
                                    kind: TypeCheckerError::WrongArgumentType(t, arg_type, TypedTerm(u, type_u)).into(),
                                })
                            }
                        },

                        _ => Err(Error {
                            kind: TypeCheckerError::NotAFunction(TypedTerm(t, type_t), u).into(),
                        }),
                    }
                },
            }
        })
    }

    /// Checks whether the term `t` living in `self` is of type `ty`.
    pub fn check(&mut self, t: Term<'arena>, ty: Term<'arena>) -> Result<'arena, ()> {
        let tty = self.infer(t)?;

        self.conversion(tty, ty).then_some(()).ok_or(Error {
            kind: TypeCheckerError::TypeMismatch(tty, ty).into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::arena::use_arena;
    use crate::term::builders::raw::*;

    fn id<'arena>() -> impl BuilderTrait<'arena> {
        abs(prop(), var(1.into(), prop()))
    }

    #[test]
    fn def_eq_1() {
        use_arena(|arena| {
            let id = arena.build_raw(id());
            let term = arena.build_raw(app(abs(prop(), id.into()), id.into()));
            let normal_form = arena.build_raw(abs(prop(), var(1.into(), prop())));

            assert!(arena.is_def_eq(term, normal_form).is_ok())
        })
    }

    #[test]
    fn def_eq_2() {
        use_arena(|arena| {
            let id = arena.build_raw(id());
            let term = arena.build_raw(app(abs(prop(), abs(prop(), var(2.into(), prop()))), id.into()));
            let normal_form = arena.build_raw(abs(prop(), id.into()));

            assert!(arena.is_def_eq(term, normal_form).is_ok())
        })
    }

    #[test]
    fn def_eq_self() {
        use_arena(|arena| {
            let id = arena.build_raw(id());
            // λa.a (λx.x) (λx.x)
            let term = arena.build_raw(abs(prop(), app(app(var(2.into(), prop()), id.into()), id.into())));

            assert!(arena.is_def_eq(term, term).is_ok());
        })
    }

    #[test]
    fn failed_def_equal() {
        use_arena(|arena| {
            let term_lhs = arena.prop();
            let term_rhs = arena.type_usize(0);

            assert_eq!(
                arena.is_def_eq(term_lhs, term_rhs),
                Err(Error {
                    kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
                })
            );
        })
    }

    #[test]
    fn failed_prod_binder_conversion() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);

            let term_lhs = arena.build_raw(prod(prop(), prop()));
            let term_rhs = arena.build_raw(prod(type_0.into(), prop()));

            assert_eq!(
                arena.is_def_eq(term_lhs, term_rhs),
                Err(Error {
                    kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
                })
            );
        })
    }

    #[test]
    fn failed_app_head_conversion() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);
            let term_lhs = arena.build_raw(abs(type_0.into(), abs(type_0.into(), app(var(1.into(), prop()), prop()))));

            let term_rhs = arena.build_raw(abs(type_0.into(), abs(type_0.into(), app(var(2.into(), prop()), prop()))));

            assert_eq!(
                arena.is_def_eq(term_lhs, term_rhs),
                Err(Error {
                    kind: TypeCheckerError::NotDefEq(term_lhs, term_rhs).into()
                })
            );
        })
    }

    #[test]
    fn typed_reduction_app_1() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);
            let term = arena.build_raw(app(abs(type_0.into(), var(1.into(), type_0.into())), prop()));

            let reduced = arena.build_raw(prop());
            assert!(arena.is_def_eq(term, reduced).is_ok());

            let term_type = arena.infer(term).unwrap();
            assert_eq!(term_type, type_0);
            assert!(arena.check(term, term_type).is_ok())
        })
    }

    #[test]
    // this test uses more intricate terms. In order to preserve some readability,
    // switching to extern_build, which is clearer.
    fn typed_reduction_app_2() {
        use crate::term::builders::*;
        use_arena(|arena| {
            // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λf.λg.f g)
            let term = arena
                .build(app(
                    abs(
                        "a",
                        // a: ((P → P) → (P → P) → P) → ((P → P) → ((P → P) → P))
                        prod(
                            "_",
                            // (P → P) → ((P → P) → P)
                            prod(
                                "_",
                                // P -> P
                                prod("_", prop(), prop()),
                                // (P -> P) -> P
                                prod("_", prod("_", prop(), prop()), prop()),
                            ),
                            // (P → P) → ((P → P) → P)
                            prod(
                                "_",
                                // P -> P
                                prod("_", prop(), prop()),
                                // (P -> P) -> P
                                prod("_", prod("_", prop(), prop()), prop()),
                            ),
                        ),
                        abs(
                            "b",
                            prop(),
                            abs(
                                "c",
                                prop(),
                                app(
                                    app(
                                        app(
                                            var("a"),
                                            abs(
                                                "d",
                                                prod("_", prop(), prop()),
                                                abs("e", prod("_", prop(), prop()), app(var("e"), app(var("d"), var("b")))),
                                            ),
                                        ),
                                        // _ : P
                                        abs("_", prop(), var("c")),
                                    ),
                                    // d : P
                                    abs("d", prop(), var("d")),
                                ),
                            ),
                        ),
                    ),
                    // f: (P -> P) -> (P -> P) -> P
                    abs(
                        "f",
                        prod("_", prod("_", prop(), prop()), prod("_", prod("_", prop(), prop()), prop())),
                        abs(
                            "g",
                            // g: P -> P
                            prod("_", prop(), prop()),
                            app(var("f"), var("g")),
                        ),
                    ),
                ))
                .unwrap();

            // λa: P. λb: P. b
            let reduced = arena.build(abs("_", prop(), abs("x", prop(), var("x")))).unwrap();
            assert!(arena.is_def_eq(term, reduced).is_ok());

            let term_type = arena.infer(term).unwrap();
            let expected_type = arena.build(prod("_", prop(), prod("_", prop(), prop()))).unwrap();
            assert_eq!(term_type, expected_type);
            assert!(arena.check(term, term_type).is_ok())
        })
    }

    #[test]
    fn typed_reduction_universe() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);
            let type_1 = arena.type_usize(1);

            let term = arena.build_raw(app(abs(prop(), type_0.into()), prod(prop(), var(1.into(), prop()))));

            assert!(arena.is_def_eq(term, type_0).is_ok());

            let term_type = arena.infer(term).unwrap();
            assert_eq!(term_type, type_1);
            assert!(arena.check(term, term_type).is_ok())
        })
    }

    #[test]
    fn escape_from_prop() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);
            let type_1 = arena.type_usize(1);
            let term = arena.build_raw(abs(prop(), prod(var(1.into(), prop()), type_0.into())));

            let term_type = arena.infer(term).unwrap();
            let expected_type = arena.build_raw(prod(prop(), type_1.into()));
            assert_eq!(term_type, expected_type);
            assert!(arena.check(term, term_type).is_ok())
        })
    }

    #[test]
    fn illtyped_reduction() {
        use_arena(|arena| {
            // λ(x: P -> P).(λ(y: P).x y) x
            // λP -> P.(λP.2 1) 1
            let term = arena.build_raw(abs(
                prod(prop(), prop()),
                app(
                    abs(prop(), app(var(2.into(), prod(prop(), prop())), var(1.into(), prop()))),
                    var(1.into(), prod(prop(), prop())),
                ),
            ));

            // λx.x x
            let reduced = arena.build_raw(abs(prop(), app(var(1.into(), prop()), var(1.into(), prop()))));

            assert!(arena.is_def_eq(term, reduced).is_ok())
        })
    }

    #[test]
    fn typed_prod_1() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);
            let term = arena.build_raw(prod(prop(), prop()));
            let term_type = arena.infer(term).unwrap();

            assert_eq!(term_type, type_0);
            assert!(arena.check(term, term_type).is_ok())
        })
    }

    #[test]
    fn typed_prod_2() {
        use_arena(|arena| {
            let term = arena.build_raw(prod(prop(), var(1.into(), prop())));
            let term_type = arena.infer(term).unwrap();

            assert_eq!(term_type, arena.prop());
            assert!(arena.check(term, term_type).is_ok());
        })
    }

    #[test]
    fn typed_prod_3() {
        use_arena(|arena| {
            let term = arena.build_raw(abs(prop(), abs(var(1.into(), prop()), var(1.into(), var(2.into(), prop())))));
            let term_type = arena.infer(term).unwrap();
            let expected_type = arena.build_raw(prod(prop(), prod(var(1.into(), prop()), var(2.into(), prop()))));

            assert_eq!(term_type, expected_type);
            assert!(arena.check(term, term_type).is_ok());
        })
    }

    #[test]
    fn typed_polymorphism() {
        use_arena(|arena| {
            let type_0 = arena.type_usize(0);

            let identity =
                arena.build_raw(abs(type_0.into(), abs(var(1.into(), type_0.into()), var(1.into(), var(2.into(), type_0.into())))));

            let identity_type = arena.infer(identity).unwrap();
            let expected_type =
                arena.build_raw(prod(type_0.into(), prod(var(1.into(), type_0.into()), var(2.into(), type_0.into()))));

            assert_eq!(identity_type, expected_type);
            assert!(arena.check(identity, identity_type).is_ok());
        })
    }

    #[test]
    fn typed_polymorphism_2() {
        use_arena(|arena| {
            let term = arena.build_raw(abs(
                prop(),
                abs(
                    prop(),
                    abs(
                        prod(prop(), prod(prop(), prop())),
                        app(app(var(1.into(), prod(prop(), prod(prop(), prop()))), var(3.into(), prop())), var(2.into(), prop())),
                    ),
                ),
            ));
            let term_type = arena.infer(term).unwrap();

            assert_eq!(term_type, arena.build_raw(prod(prop(), prod(prop(), prod(prod(prop(), prod(prop(), prop())), prop())))));
            assert!(arena.check(term, term_type).is_ok());
        })
    }

    #[test]
    fn type_hierarchy_prop() {
        use_arena(|arena| {
            let term = arena.prop();
            let term_type = arena.infer(term).unwrap();

            assert_eq!(term_type, arena.type_usize(0));
            assert!(arena.check(term, term_type).is_ok());
        })
    }

    #[test]
    fn type_hierarchy_type() {
        use_arena(|arena| {
            let term = arena.type_usize(0);
            let term_type = arena.infer(term).unwrap();

            assert_eq!(term_type, arena.type_usize(1));
            assert!(arena.check(term, term_type).is_ok());
        })
    }

    mod failed_type_inference {
        use super::*;

        #[test]
        fn not_function_abs() {
            use_arena(|arena| {
                let term = arena.build_raw(abs(app(prop(), prop()), prop()));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn not_function_prod_1() {
            use_arena(|arena| {
                let term = arena.build_raw(prod(prop(), app(prop(), prop())));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn not_function_prod_2() {
            use_arena(|arena| {
                let term = arena.build_raw(prod(app(prop(), prop()), prop()));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn not_function_app_1() {
            use_arena(|arena| {
                let term = arena.build_raw(app(prop(), prop()));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn not_function_app_2() {
            use_arena(|arena| {
                let term = arena.build_raw(app(app(prop(), prop()), prop()));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn not_function_app_3() {
            use_arena(|arena| {
                let term = arena.build_raw(app(abs(prop(), prop()), app(prop(), prop())));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into()
                    })
                );
            })
        }

        #[test]
        fn wrong_argument_type() {
            use_arena(|arena| {
                // λ(x: P -> P).(λ(y: P).x y) x
                // λP -> P.(λP.2 1) 1
                let term = arena.build_raw(abs(
                    prod(prop(), prop()),
                    app(
                        abs(prop(), app(var(2.into(), prod(prop(), prop())), var(1.into(), prop()))),
                        var(1.into(), prod(prop(), prop())),
                    ),
                ));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::WrongArgumentType(
                            arena.build_raw(abs(prop(), app(var(2.into(), prod(prop(), prop())), var(1.into(), prop())))),
                            arena.prop(),
                            TypedTerm(arena.build_raw(var(1.into(), prod(prop(), prop()))), arena.build_raw(prod(prop(), prop())))
                        )
                        .into()
                    })
                );
            })
        }

        #[test]
        fn not_universe_abs() {
            use_arena(|arena| {
                let term = arena.build_raw(abs(
                    prop(),
                    abs(var(1.into(), prop()), abs(var(1.into(), var(2.into(), prop())), var(1.into(), var(2.into(), prop())))),
                ));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotUniverse(arena.build_raw(var(2.into(), prop()))).into()
                    })
                );
            })
        }

        #[test]
        fn not_universe_prod_1() {
            use_arena(|arena| {
                let term = arena.build_raw(prod(id(), prop()));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotUniverse(arena.build_raw(prod(prop(), prop()))).into()
                    })
                );
            })
        }

        #[test]
        fn not_universe_prod_2() {
            use_arena(|arena| {
                let term = arena.build_raw(prod(prop(), abs(prop(), prop())));

                assert_eq!(
                    arena.infer(term),
                    Err(Error {
                        kind: TypeCheckerError::NotUniverse(arena.build_raw(prod(prop(), type_(BigUint::from(0_u64).into()))))
                            .into()
                    })
                );
            })
        }

        #[test]
        fn check_fail_1() {
            use_arena(|arena| {
                let term = arena.build_raw(app(prop(), prop()));
                let expected_type = arena.prop();

                assert_eq!(
                    arena.check(term, expected_type),
                    Err(Error {
                        kind: TypeCheckerError::NotAFunction(TypedTerm(arena.prop(), arena.type_usize(0)), arena.prop()).into(),
                    })
                );
            })
        }

        #[test]
        fn check_fail_2() {
            use_arena(|arena| {
                let prop = arena.prop();
                assert_eq!(
                    arena.check(prop, prop),
                    Err(Error {
                        kind: TypeCheckerError::TypeMismatch(arena.type_usize(0), prop).into(),
                    })
                );
            })
        }
    }
}
