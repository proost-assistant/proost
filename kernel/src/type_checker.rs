use crate::term::*;
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
/// Type representing kernel errors, is used by the toplevel to pretty-print errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeCheckingError {
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
// The context, which is supposed to contain other definitions in the environment, is not implemented for now.
// TODO use context for type-checking (#17)

// Type of lists of tuples representing the respective types of each variables
type Types = Vec<Term>;

/// Structure containing a context used for typechecking. It serves to store the types of variables in the following way :
/// in a given context {types,lvl}, the type of `Var(i)` is in `types[lvl-i]`.
#[derive(Clone, Debug, Default)]
struct Context {
    types: Types,
    lvl: DeBruijnIndex,
}

impl Context {
    fn new() -> Self {
        Default::default()
    }

    /// Extend Context with a bound variable of type ty.
    fn bind(self, ty: Term) -> Context {
        let mut new_types = self.types;
        new_types.push(ty);
        Context {
            types: new_types,
            lvl: self.lvl + 1.into(),
        }
    }
}

impl Term {
    /// Conversion function, checks whether two values are definitionally equal.
    ///
    /// The conversion is untyped, meaning that it should **only** be called during type-checking when the two `Term`s are already known to be of the same type and in the same context.
    pub fn conversion(self, rhs: Term, l: DeBruijnIndex) -> bool {
        match (self.whnf(), rhs.whnf()) {
            (Type(i), Type(j)) => i == j,

            (Prop, Prop) => true,

            (Var(i), Var(j)) => i == j,

            (Prod(a1, b1), Prod(box a2, b2)) => {
                a1.conversion(a2, l)
                    && b1
                        .substitute(Var(l), l.into())
                        .conversion(b2.substitute(Var(l), l.into()), l + 1.into())
            }

            // Since we assume that both vals already have the same type,
            // checking conversion over the argument type is useless.
            // However, this doesn't mean we can simply remove the arg type
            // from the type constructor in the enum, it is needed to quote back to terms.
            (Abs(_, t), Abs(_, u)) => t
                .substitute(Var(l), l.into())
                .conversion(u.substitute(Var(l), l.into()), l + 1.into()),

            (App(box t1, box u1), App(box t2, box u2)) => {
                t1.conversion(t2, l) && u1.conversion(u2, l)
            }

            _ => false,
        }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(self, rhs: Term) -> Result<(), TypeCheckingError> {
        if !self.clone().conversion(rhs.clone(), 1.into()) {
            Err(NotDefEq(self, rhs))
        } else {
            Ok(())
        }
    }

    fn is_universe(&self) -> bool {
        matches!(*self, Prop | Type(_))
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(self, u2: Term) -> Result<Term, TypeCheckingError> {
        match u2 {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
            Prop => Ok(Prop),

            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j))),

                _ => Err(NotUniverse(u2.clone())),
            },

            _ => Err(NotUniverse(self)),
        }
    }

    fn _infer(self, ctx: &Context) -> Result<Term, TypeCheckingError> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[ctx.lvl - i].clone()),
            Prod(box a, c) => {
                let ua = a.clone()._infer(ctx)?;
                if !ua.is_universe() {
                    Err(NotType(ua))
                } else {
                    let ctx2 = ctx.clone().bind(a);
                    let ub = c._infer(&ctx2)?;
                    if !ub.is_universe() {
                        Err(NotType(ub))
                    } else {
                        ua.imax(ub)
                    }
                }
            }
            Abs(box t1, c) => {
                let ctx2 = ctx.clone().bind(t1.clone());
                Ok(Prod(box t1, box (*c)._infer(&ctx2)?))
            }
            App(box a, box b) => {
                let type_a = a.clone()._infer(ctx)?;
                if let Prod(box t1, cls) = type_a {
                    let t1_ = b.clone()._infer(ctx)?;
                    if !t1.clone().conversion(t1_.clone(), ctx.types.len().into()) {
                        return Err(WrongArgumentType(a, t1, b, t1_));
                    };
                    Ok(*cls)
                } else {
                    Err(NotAFunction(a, type_a, b))
                }
            }
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(self) -> Result<Term, TypeCheckingError> {
        self._infer(&Context::new())
    }
    fn _check(self, ctx: &Context, ty: Term) -> Result<(), TypeCheckingError> {
        let tty = self._infer(ctx)?;
        if !tty.clone().conversion(ty.clone(), ctx.types.len().into()) {
            return Err(TypeMismatch(ty, tty));
        };
        Ok(())
    }
    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(self, ty: Term) -> Result<(), TypeCheckingError> {
        self._check(&mut Context::new(), ty)
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
        assert_eq!(t.normal_form(), nf)
    }

    #[test]
    fn var_subst_2() {
        let t = App(
            box Abs(box Prop, box Abs(box Prop, box Var(2.into()))),
            id(1),
        );
        let nf = Abs(box Prop, id(1));
        assert_eq!(t.normal_form(), nf)
    }
    #[test]
    fn simple() {
        let t1 = App(
            box Abs(box Type(BigUint::from(0_u64).into()), box Var(1.into())),
            box Prop,
        );
        let t2 = Prop;
        assert!(t1.clone().conversion(t2, 0.into()));
        let ty = t1._infer(&Context::new());
        assert_eq!(ty, Ok(Type(BigUint::from(0_u64).into())));
    }

    #[test]
    fn simple_substitute() -> Result<(), TypeCheckingError> {
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
        assert_eq!(term.clone().is_def_eq(reduced), Ok(()));
        let _ty = term.infer();
        assert!(matches!(_ty, Err(WrongArgumentType(_, _, _, _))));
        Ok(())
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
        assert_eq!(term.clone().is_def_eq(reduced), Ok(()));
        assert!(matches!(term.infer(), Ok(_)))
    }

    //(λ ℙ → λ ℙ → λ ℙ → (0 (λ ℙ → λ ℙ → ((4 1) 3) λ ℙ → 3)) (λ ℙ → λ ℙ → (0 1) (λ ℙ → 0 λ ℙ → 0)))
    #[test]
    fn nf_test() {
        //λa.a (λx.x) (λx.x)
        let reduced = Abs(box Prop, box App(box App(box Var(2.into()), id(1)), id(1)));
        let nff = reduced.clone().normal_form();
        assert_eq!(reduced, nff);
        assert_eq!(reduced.is_def_eq(nff), Ok(()));
    }

    #[test]
    fn polymorphism() {
        let id = Abs(
            box Type(BigUint::from(0_u64).into()),
            box Abs(box Var(1.into()), box Var(1.into())),
        );
        assert!(matches!(id.infer(), Ok(_)))
    }
    #[test]
    fn type_type() {
        assert!(matches!(
            Type(BigUint::from(0_u64).into()).check(Type(BigUint::from(1_u64).into())),
            Ok(_)
        ))
    }

    #[test]
    fn not_function() {
        let t = App(box Prop, box Prop);
        assert!(matches!(t.infer(), Err(NotAFunction(..))))
    }

    #[test]
    fn not_type_prod() {
        let t1 = Prod(box Abs(box Prop, box Var(1.into())), box Prop);
        assert!(matches!(t1.infer(), Err(NotType(..))));
        let t2 = Prod(box Prop, box Abs(box Prop, box Prop));
        assert!(matches!(t2.infer(), Err(NotType(..))));
        let wf_prod1 = Prod(box Prop, box Prop);
        assert!(matches!(
            wf_prod1.check(Type(BigUint::from(0_u64).into())),
            Ok(())
        ));
        let wf_prod2 = Prod(box Prop, box Var(1.into()));
        assert!(matches!(wf_prod2.check(Prop), Ok(())));
        // Type0 -> (A : Prop) ->
        let wf_prod3 = Prod(box Prop, box Prop);
        assert!(matches!(
            wf_prod3.check(Type(BigUint::from(0_u64).into())),
            Ok(())
        ));
    }
}
