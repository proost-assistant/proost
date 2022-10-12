use derive_more::{Add, Display, From, Into, Sub};
use num_bigint::BigUint;

#[derive(Add, Copy, Clone, Debug, Display, Eq, Into, From, Sub, PartialEq, PartialOrd, Ord)]
pub struct DeBruijnIndex(usize);

#[derive(Add, Clone, Debug, Display, Eq, Into, From, Sub, PartialEq, PartialOrd, Ord)]
pub struct UniverseLevel(BigUint);

impl From<usize> for UniverseLevel {
    fn from(i: usize) -> Self {
        BigUint::from(i).into()
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Term {
    #[display(fmt = "{}", _0)]
    Var(DeBruijnIndex),

    #[display(fmt = "\u{02119}")]
    Prop,

    #[display(fmt = "Type({})", _0)]
    Type(UniverseLevel),

    #[display(fmt = "({} {})", _0, _1)]
    App(Box<Term>, Box<Term>),

    #[display(fmt = "\u{003BB}{} : {} \u{02192} {}", _0, _1, _2)]
    Abs(String, Box<Term>, Box<Term>),

    #[display(fmt = "\u{02200}{} : {} \u{02192} {}", _0, _1, _2)]
    Prod(String, Box<Term>, Box<Term>),
}

use Term::*;

impl Term {
    /// Returns the normal form of the term
    pub fn beta_reduction(self) -> Term {
        match self {
            App(box Abs(_, _, box t1), box t2) => t1.substitute(t2, 1),
            App(box t1, box t2) => App(box t1.beta_reduction(), box t2),
            Abs(s, x, box t) => Abs(s, x, box t.beta_reduction()),
            _ => self,
        }
    }

    fn shift(self, offset: usize, depth: usize) -> Term {
        match self {
            Var(i) if i > depth.into() => Var(i + offset.into()),
            App(box t1, box t2) => App(box t1.shift(offset, depth), box t2.shift(offset, depth)),
            Abs(s, t1, box t2) => Abs(s, t1, box t2.shift(offset, depth + 1)),
            Prod(s, t1, box t2) => Prod(s, t1, box t2.shift(offset, depth + 1)),
            _ => self,
        }
    }

    fn substitute(self, rhs: Term, depth: usize) -> Term {
        match self {
            Var(i) if i == depth.into() => rhs.shift(depth - 1, 0),
            Var(i) if i > depth.into() => Var(i - 1.into()),

            App(l, r) => App(
                box l.substitute(rhs.clone(), depth),
                box r.substitute(rhs, depth),
            ),
            Abs(s, t, term) => Abs(s, t, box term.substitute(rhs, depth + 1)),
            Prod(s, t, term) => Prod(s, t, box term.substitute(rhs, depth + 1)),
            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    // TODO: Correctly types lambda terms.

    use super::Term::*;

    #[test]
    fn simple_subst() {
        // λx.(λy.x y) x
        let term = Abs(
            "".into(),
            box Prop,
            box App(
                box Abs(
                    "".into(),
                    box Prop,
                    box App(box Var(2.into()), box Var(1.into())),
                ),
                box Var(1.into()),
            ),
        );

        // λx.x x
        let reduced = Abs(
            "".into(),
            box Prop,
            box App(box Var(1.into()), box Var(1.into())),
        );

        assert_eq!(term.beta_reduction(), reduced);
    }

    #[test]
    fn complex_subst() {
        // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
        let term = App(
            box Abs(
                "".into(),
                box Prop,
                box Abs(
                    "".into(),
                    box Prop,
                    box Abs(
                        "".into(),
                        box Prop,
                        box App(
                            box App(
                                box App(
                                    box Var(3.into()),
                                    box Abs(
                                        "".into(),
                                        box Prop,
                                        box Abs(
                                            "".into(),
                                            box Prop,
                                            box App(
                                                box Var(1.into()),
                                                box App(box Var(2.into()), box Var(4.into())),
                                            ),
                                        ),
                                    ),
                                ),
                                box Abs("".into(), box Prop, box Var(2.into())),
                            ),
                            box Abs("".into(), box Prop, box Var(1.into())),
                        ),
                    ),
                ),
            ),
            box Abs(
                "".into(),
                box Prop,
                box Abs(
                    "".into(),
                    box Prop,
                    box App(box Var(2.into()), box Var(1.into())),
                ),
            ),
        );

        let term_step_1 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box App(
                        box App(
                            box Abs(
                                "".into(),
                                box Prop,
                                box Abs(
                                    "".into(),
                                    box Prop,
                                    box App(box Var(2.into()), box Var(1.into())),
                                ),
                            ),
                            box Abs(
                                "".into(),
                                box Prop,
                                box Abs(
                                    "".into(),
                                    box Prop,
                                    box App(
                                        box Var(1.into()),
                                        box App(box Var(2.into()), box Var(4.into())),
                                    ),
                                ),
                            ),
                        ),
                        box Abs("".into(), box Prop, box Var(2.into())),
                    ),
                    box Abs("".into(), box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_2 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box App(
                        box Abs(
                            "".into(),
                            box Prop,
                            box App(
                                box Abs(
                                    "".into(),
                                    box Prop,
                                    box Abs(
                                        "".into(),
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
                        box Abs("".into(), box Prop, box Var(2.into())),
                    ),
                    box Abs("".into(), box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_3 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box App(
                        box Abs(
                            "".into(),
                            box Prop,
                            box Abs(
                                "".into(),
                                box Prop,
                                box App(
                                    box Var(1.into()),
                                    box App(box Var(2.into()), box Var(4.into())),
                                ),
                            ),
                        ),
                        box Abs("".into(), box Prop, box Var(2.into())),
                    ),
                    box Abs("".into(), box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_4 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box Abs(
                        "".into(),
                        box Prop,
                        box App(
                            box Var(1.into()),
                            box App(
                                box Abs("".into(), box Prop, box Var(3.into())),
                                box Var(3.into()),
                            ),
                        ),
                    ),
                    box Abs("".into(), box Prop, box Var(1.into())),
                ),
            ),
        );

        let term_step_5 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box Abs("".into(), box Prop, box Var(1.into())),
                    box App(
                        box Abs("".into(), box Prop, box Var(2.into())),
                        box Var(2.into()),
                    ),
                ),
            ),
        );

        let term_step_6 = Abs(
            "".into(),
            box Prop,
            box Abs(
                "".into(),
                box Prop,
                box App(
                    box Abs("".into(), box Prop, box Var(2.into())),
                    box Var(2.into()),
                ),
            ),
        );

        // λa.λb.b
        let term_step_7 = Abs(
            "".into(),
            box Prop,
            box Abs("".into(), box Prop, box Var(1.into())),
        );

        assert_eq!(term.beta_reduction(), term_step_1);
        assert_eq!(term_step_1.beta_reduction(), term_step_2);
        assert_eq!(term_step_2.beta_reduction(), term_step_3);
        assert_eq!(term_step_3.beta_reduction(), term_step_4);
        assert_eq!(term_step_4.beta_reduction(), term_step_5);
        assert_eq!(term_step_5.beta_reduction(), term_step_6);
        assert_eq!(term_step_6.beta_reduction(), term_step_7);
        assert_eq!(term_step_7.clone().beta_reduction(), term_step_7);
    }
}
