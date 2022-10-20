use derive_more::{Add, Display, From, Into, Sub};
use num_bigint::BigUint;
use std::collections::HashMap;

#[derive(
    Add, Copy, Clone, Debug, Default, Display, Eq, Into, From, Sub, PartialEq, PartialOrd, Ord,
)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Display, Eq, From, Sub, PartialEq, PartialOrd, Ord)]
pub struct UniverseLevel(BigUint);

pub type GlobalContext = HashMap<String,(Term,Term)>;


#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Term {
    #[display(fmt = "{}", _0)]
    Var(DeBruijnIndex),

    #[display(fmt = "{}", _0)]
    Const(String),

    #[display(fmt = "\u{02119}")]
    Prop,

    #[display(fmt = "Type({})", _0)]
    Type(UniverseLevel),

    #[display(fmt = "({} {})", _0, _1)]
    App(Box<Term>, Box<Term>),

    #[display(fmt = "\u{003BB}{} \u{02192} {}", _0, _1)]
    Abs(Box<Term>, Box<Term>),

    #[display(fmt = "\u{02200}{} \u{02192} {}", _0, _1)]
    Prod(Box<Term>, Box<Term>),
}

use Term::*;

impl Term {
    /// Apply one step of β-reduction, using leftmost outermost evaluation strategy.
    pub fn beta_reduction(self, ctx : &GlobalContext) -> Term {
        match self {
            App(box Abs(_, box t1), box t2) => t1.substitute(t2, 1),
            App(box t1, box t2) => App(box t1.beta_reduction(ctx), box t2),
            Abs(x, box t) => Abs(x, box t.beta_reduction(ctx)),
            Const(s) => match ctx.get(&s) {
                Some((t,_)) => t.clone(),
                None => panic!("unreachable code has been reached")
            }
            _ => self,
        }
    }

    fn shift(self, offset: usize, depth: usize) -> Term {
        match self {
            Var(i) if i > depth.into() => Var(i + offset.into()),
            App(box t1, box t2) => App(box t1.shift(offset, depth), box t2.shift(offset, depth)),
            Abs(t1, box t2) => Abs(t1, box t2.shift(offset, depth + 1)),
            Prod(t1, box t2) => Prod(t1, box t2.shift(offset, depth + 1)),
            _ => self,
        }
    }

    pub(crate) fn substitute(self, rhs: Term, depth: usize) -> Term {
        match self {
            Var(i) if i == depth.into() => rhs.shift(depth - 1, 0),
            Var(i) if i > depth.into() => Var(i - 1.into()),

            App(l, r) => App(
                box l.substitute(rhs.clone(), depth),
                box r.substitute(rhs, depth),
            ),
            Abs(t, term) => Abs(t, box term.substitute(rhs, depth + 1)),
            Prod(t, term) => Prod(t, box term.substitute(rhs, depth + 1)),
            _ => self,
        }
    }

    /// Returns the normal form of a term in a given environment.
    ///
    /// This function is computationally expensive and should only be used for Reduce/Eval commands, not when type-checking.
    pub fn normal_form(self,ctx : &GlobalContext) -> Term {
        let mut res = self.clone().beta_reduction(ctx);
        let mut temp = self;
        while res != temp {
            temp = res.clone();
            res = res.beta_reduction(ctx)
        }
        res
    }
    /// Returns the weak-head normal form of a term in a given environment.
    pub fn whnf(self, ctx : &GlobalContext) -> Term {
        match self.clone() {
            App(box t, t2) => match t.whnf(ctx) {
                whnf @ Abs(_, _) => App(box whnf, t2).beta_reduction(ctx).whnf(ctx),
                _ => self,
            },
            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    // /!\ most of these tests are on ill-typed terms and should not be used for further testings
    use super::Term::*;

    #[test]
    fn simple_subst() {
        // λx.(λy.x y) x
        let term = Abs(
            box Prop,
            box App(
                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                box Var(1.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(1.into()), box Var(1.into())));

        assert_eq!(term.beta_reduction(&Default::default()), reduced);
    }

    #[test]
    fn complex_subst() {
        // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
        let term = App(
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(
                        box Prop,
                        box App(
                            box App(
                                box App(
                                    box Var(3.into()),
                                    box Abs(
                                        box Prop,
                                        box Abs(
                                            box Prop,
                                            box App(
                                                box Var(1.into()),
                                                box App(box Var(2.into()), box Var(4.into())),
                                            ),
                                        ),
                                    ),
                                ),
                                box Abs(box Prop, box Var(2.into())),
                            ),
                            box Abs(box Prop, box Var(1.into())),
                        ),
                    ),
                ),
            ),
            box Abs(
                box Prop,
                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
            ),
        );

        let term_step_1 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box App(
                            box Abs(
                                box Prop,
                                box Abs(box Prop, box App(box Var(2.into()), box Var(1.into()))),
                            ),
                            box Abs(
                                box Prop,
                                box Abs(
                                    box Prop,
                                    box App(
                                        box Var(1.into()),
                                        box App(box Var(2.into()), box Var(4.into())),
                                    ),
                                ),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_2 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box Abs(
                            box Prop,
                            box App(
                                box Abs(
                                    box Prop,
                                    box Abs(
                                        box Prop,
                                        box App(
                                            box Var(1.into()),
                                            box App(box Var(2.into()), box Var(5.into())),
                                        ),
                                    ),
                                ),
                                box Var(1.into()),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_3 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box Abs(
                            box Prop,
                            box Abs(
                                box Prop,
                                box App(
                                    box Var(1.into()),
                                    box App(box Var(2.into()), box Var(4.into())),
                                ),
                            ),
                        ),
                        box Abs(box Prop, box Var(2.into())),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_4 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(
                        box Prop,
                        box App(
                            box Var(1.into()),
                            box App(box Abs(box Prop, box Var(3.into())), box Var(3.into())),
                        ),
                    ),
                    box Abs(box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_5 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(box Prop, box Var(1.into())),
                    box App(box Abs(box Prop, box Var(2.into())), box Var(2.into())),
                ),
            ),
        );

        let term_step_6 = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(box Abs(box Prop, box Var(2.into())), box Var(2.into())),
            ),
        );

        // λa.λb.b
        let term_step_7 = Abs(box Prop, box Abs(box Prop, box Var(1.into())));

        assert_eq!(term.beta_reduction(&Default::default()), term_step_1);
        assert_eq!(term_step_1.beta_reduction( &Default::default()), term_step_2);
        assert_eq!(term_step_2.beta_reduction( &Default::default()), term_step_3);
        assert_eq!(term_step_3.beta_reduction( &Default::default()), term_step_4);
        assert_eq!(term_step_4.beta_reduction( &Default::default()), term_step_5);
        assert_eq!(term_step_5.beta_reduction( &Default::default()), term_step_6);
        assert_eq!(term_step_6.beta_reduction( &Default::default()), term_step_7);
        assert_eq!(term_step_7.clone().beta_reduction(&Default::default()), term_step_7);
    }
}
