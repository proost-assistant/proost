use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Prop,
    Var(usize),
    Type(usize),
    App(Box<Term>, Box<Term>),
    Abs(Box<Term>, Box<Term>),
    Prod(Box<Term>, Box<Term>),
}

use Term::*;

impl Term {
    /// Returns the normal form of the term
    ///
    /// # Examples
    ///
    /// ```
    /// // TODO: Use parser
    /// ```
    pub fn beta_reduction(self) -> Term {
        match self {
            App(box Abs(_, box t1), box t2) => t1.substitute(t2, 1),
            App(box t1, box t2) => App(box t1.beta_reduction(), box t2.beta_reduction()),
            Abs(x, box t) => Abs(x, box t.beta_reduction()),
            _ => self,
        }
    }

    fn shift(self, offset: usize) -> Term {
        match self {
            Var(x) => Var(x + offset),
            App(box t1, box t2) => App(box t1.shift(offset), box t2.shift(offset)),
            Abs(x, box t) => Abs(x.clone(), box t.shift(offset)),
            Prod(x, box t) => Prod(x.clone(), box t.shift(offset)),
            _ => self.clone(),
        }
    }

    fn substitute(self, rhs: Term, depth: usize) -> Term {
        match self {
            Var(i) if i == depth => rhs.shift(depth - 1),
            Var(i) if i > depth => Var(i - 1),

            App(l, r) => App(
                box l.substitute(rhs.clone(), depth),
                box r.substitute(rhs, depth),
            ),
            Abs(t, term) => Abs(t, box term.substitute(rhs, depth + 1)),
            Prod(t, term) => Prod(t, box term.substitute(rhs, depth + 1)),
            _ => self,
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Prop => write!(f, "\u{02119}"),
            Var(i) => write!(f, "{}", i),
            Type(i) => write!(f, "\u{1D54B}({})", i),
            App(t1, t2) => write!(f, "({} {})", t1, t2),
            Abs(t1, t2) => write!(f, "\u{003BB}{} \u{02192} {}", t1, t2),
            Prod(t1, t2) => write!(f, "\u{02200}{} \u{02192} {}", t1, t2),
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
            box Prop,
            box App(
                box Abs(box Prop, box App(box Var(2), box Var(1))),
                box Var(1),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(1), box Var(1)));

        assert_eq!(term.beta_reduction(), reduced);
    }

    #[test]
    fn complex_subst() {
        // [λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)] (λa.λb.a b)
        let term = App(
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(
                        box Prop,
                        box App(
                            box Var(3),
                            box App(
                                box App(
                                    box Abs(
                                        box Prop,
                                        box Abs(
                                            box Prop,
                                            box App(box Var(1), box App(box Var(2), box Var(4))),
                                        ),
                                    ),
                                    box Abs(box Prop, box Var(2)),
                                ),
                                box Abs(box Prop, box Var(1)),
                            ),
                        ),
                    ),
                ),
            ),
            box Abs(box Prop, box Abs(box Prop, box App(box Var(2), box Var(1)))),
        );

        // λa.λb.((λc.(λd.λe.e (d a)) c) (λ_.b)) (λc.c)
        // λa.λb.((λc.λd.(d (c a))) (λ_.b)) (λc.c)
        // λa.λb.(λc.c ((λ_.b) a)) (λc.c)
        // λa.λb.(λc.c) ((λ_.b) a)
        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(term.beta_reduction(), reduced);
    }

    #[test]
    fn complex_subst_step_5() {
        // λa.λb.((λc.(λd.λe.e (d a)) c) (λ_.b)) (λc.c)
        let term = Abs(
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
                                        box App(box Var(1), box App(box Var(2), box Var(5))),
                                    ),
                                ),
                                box Var(1),
                            ),
                        ),
                        box Abs(box Prop, box Var(2)),
                    ),
                    box Abs(box Prop, box Var(1)),
                ),
            ),
        );

        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(
            term.beta_reduction()
                .beta_reduction()
                .beta_reduction()
                .beta_reduction()
                .beta_reduction(),
            reduced
        );
    }

    #[test]
    fn complex_subst_step_4() {
        // λa.λb.((λc.λd.(d (c a))) (λ_.b)) (λc.c)
        let term = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box App(
                        box Abs(
                            box Prop,
                            box Abs(
                                box Prop,
                                box App(box Var(1), box App(box Var(2), box Var(4))),
                            ),
                        ),
                        box Abs(box Prop, box Var(2)),
                    ),
                    box Abs(box Prop, box Var(1)),
                ),
            ),
        );

        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(
            term.beta_reduction()
                .beta_reduction()
                .beta_reduction()
                .beta_reduction(),
            reduced
        );
    }

    #[test]
    fn complex_subst_step_3() {
        // λa.λb.(λc.c ((λ_.b) a)) (λc.c)
        let term = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(
                        box Prop,
                        box App(
                            box Var(1),
                            box App(box Abs(box Prop, box Var(3)), box Var(3)),
                        ),
                    ),
                    box Abs(box Prop, box Var(1)),
                ),
            ),
        );

        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(
            term.beta_reduction().beta_reduction().beta_reduction(),
            reduced
        );
    }

    #[test]
    fn complex_subst_step_2() {
        // λa.λb.(λc.c) ((λ_.b) a)
        let term = Abs(
            box Prop,
            box Abs(
                box Prop,
                box App(
                    box Abs(box Prop, box Var(1)),
                    box App(box Abs(box Prop, box Var(2)), box Var(2)),
                ),
            ),
        );

        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(term.beta_reduction().beta_reduction(), reduced);
    }

    #[test]
    fn complex_subst_step_1() {
        // λa.λb.(λc.b) a
        let term = Abs(
            box Prop,
            box Abs(box Prop, box App(box Abs(box Prop, box Var(2)), box Var(2))),
        );

        // λa.λb.b
        let reduced = Abs(box Prop, box Abs(box Prop, box Var(1)));

        assert_eq!(term.beta_reduction(), reduced);
    }
}
