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
#[derive(Debug, Eq)]
pub enum Type_checking_error {
    not_universe(Term),
    not_type(Term),
    not_def_eq(Term,Term),
    wrong_argument_type(Term,Term,Term,Term),
    not_a_function(Term,Term,Term),
    type_mismatch(Term,Term)
}

use Type_checking_error::*;
// The context, which is supposed to contain other definitions in the environment, is not implemented for now.
// TODO use context for type-checking (#17)

// Type of lists of tuples representing the respective types of each variables
type Types = Vec<Term>;

#[derive(Clone, Debug, Default)]
/// Structure containing a context used for typechecking. It serves to store the types of variables in the following way :
/// in a given context {types,lvl}, the type of `Var(i)` is in `types[lvl-i]`.
pub struct Context {
    types: Types,
    lvl: DeBruijnIndex,
}

impl Context {
    pub fn new() -> Self {
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

            (Abs(_, t), u) | (u, Abs(_, t)) => t
                .substitute(Var(l), l.into())
                .conversion(App(box u, box Var(l)), l + 1.into()),

            (App(box t1, box u1), App(box t2, box u2)) => {
                t1.conversion(t2, l) && u1.conversion(u2, l)
            }

            _ => false,
        }
    }

    /// Checks whether two terms are definitionally equal.
    pub fn is_def_eq(self, rhs: Term) -> Result<(), Type_checking_error> {
        if !self.clone().conversion(rhs.clone(), 1.into()) {
            //TODO #19
            Err(not_def_eq(
                self, rhs
            ))
        } else {
            Ok(())
        }
    }

    fn is_universe(&self) -> bool {
        matches!(*self, Prop | Type(_))
    }

    /// Computes universe the universe in which `(x : A) -> B` lives when `A : u1` and `B : u2`.
    fn imax(self, u2: Term) -> Result<Term, Type_checking_error> {
        match u2 {
            // Because Prop is impredicative, if B : Prop, then (x : A) -> b : Prop
            Prop => Ok(Prop),

            Type(ref i) => match self {
                Prop => Ok(Type(i.clone())),

                // else if u1 = Type(i) and u2 = Type(j), then (x : A) -> B : Type(max(i,j))
                Type(j) => Ok(Type(max(i.clone(), j))),

                //TODO #19
                _ => Err(not_universe( u2.clone())),
            },

            //TODO #19
            _ => Err(not_universe(self)),
        }
    }

    /// Infers the type of a `Term` in a given context.
    pub fn infer(self, ctx: &Context) -> Result<Term, Type_checking_error> {
        match self {
            Prop => Ok(Type(BigUint::from(0_u64).into())),
            Type(i) => Ok(Type(i + BigUint::from(1_u64).into())),
            Var(i) => Ok(ctx.types[ctx.lvl - i].clone()),
            Prod(box a, c) => {
                let ua = a.infer(ctx)?;
                if !ua.is_universe() {
                    //TODO #19
                    Err(not_type(ua))
                } else {
                    let ctx2 = ctx.clone().bind(ua.clone());
                    let ub = c.infer(&ctx2)?;
                    if !ub.is_universe() {
                        //TODO #19
                        Err(not_type(ub))
                    } else {
                        ua.imax(ub)
                    }
                }
            }
            Abs(box t1, c) => {
                let ctx2 = ctx.clone().bind(t1.clone());
                Ok(Prod(box t1, box (*c).infer(&ctx2)?))
            }
            App(box a, box b) => {
                let type_a = a.clone().infer(ctx)?;
                if let Prod(box t1, cls) = type_a {
                    let t1_ = b.clone().infer(ctx)?;
                    if !t1.clone().conversion(t1_.clone(), ctx.types.len().into()) {
                        //TODO #19
                        return Err(wrong_argument_type( a,t1,b,t1_));
                    };
                    Ok(*cls)
                } else {
                    //TODO #19
                    Err(not_a_function(a,type_a,b))
                }
            }
        }
    }

    /// Checks whether a given term is of type `ty` in a given context.
    pub fn check(self, ctx: &mut Context, ty: Term) -> Result<(), Type_checking_error> {
        match (self.clone(), ty.clone()) {
            (Abs(box t1, box t2), Prod(box a, b)) => {
                t1.check(ctx, a.clone())?;
                t2.check(
                    &mut ctx.clone().bind(a),
                    b.substitute(Var(ctx.types.len().into()), 1),
                )
            }
            _ => {
                let tty = self.infer(ctx)?;
                if !tty.clone().conversion(ty.clone(), ctx.types.len().into()) {
                    //TODO #19
                    return Err(type_mismatch( 
                        ty, tty
                    ));
                };
                Ok(())
            }
        }
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
        let ty = t1.infer(&Context::new());
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
        assert_eq!(term.clone().is_def_eq(reduced), Ok(()));
        let _ty = term.infer(&Context::new());
        assert_eq!(_ty, Err("Wrong argument, function\n  Abs(Prop, App(Var(DeBruijnIndex(2)), Var(DeBruijnIndex(1))))\n expected argument of type\n  Prop\n but term\n    Var(DeBruijnIndex(1))\n is of type\n    Ok(Prod(Prop, Prop))".into()))
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
        assert!(matches!(term.infer(&Context::new()), Ok(_)))
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
            box Abs(box Var(0.into()), box Var(1.into())),
        );
        assert!(matches!(id.infer(&Context::new()), Ok(_)))
    }
}
