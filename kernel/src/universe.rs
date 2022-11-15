use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Add;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum UniverseLevel {
    #[default]
    Zero,

    Succ(Box<UniverseLevel>),

    Max(Box<UniverseLevel>, Box<UniverseLevel>),

    IMax(Box<UniverseLevel>, Box<UniverseLevel>),

    Var(usize),
}

impl Add<usize> for UniverseLevel {
    type Output = Self;

    fn add(self, n: usize) -> Self {
        if n == 0 {
            self
        } else {
            Succ(box self.add(n - 1))
        }
    }
}

impl From<usize> for UniverseLevel {
    fn from(n: usize) -> Self {
        if n == 0 {
            Zero
        } else {
            Succ(box (n - 1).into())
        }
    }
}

impl Display for UniverseLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

use UniverseLevel::*;

impl UniverseLevel {
    /// Helper function for pretty printing, if universe doesn't contain any variable then it gets printed as a decimal number.
    fn to_numeral(&self) -> Option<usize> {
        match self {
            Zero => Some(0),
            Succ(box u) => u.to_numeral().map(|n| n + 1),
            Max(box n, box m) => n
                .to_numeral()
                .and_then(|n| m.to_numeral().map(|m| n.max(m))),
            IMax(box n, box m) => m.to_numeral().and_then(|m| {
                if m == 0 {
                    0.into()
                } else {
                    n.to_numeral().map(|n| m.max(n))
                }
            }),
            _ => None,
        }
    }

    /// Helper function for pretty printing, if universe is of the form Succ(Succ(...(Succ(u))...)) then it gets printed as u+n.
    fn plus(&self) -> (UniverseLevel, usize) {
        match self {
            Succ(box u) => {
                let (u, n) = u.plus();
                (u, n + 1)
            }
            _ => (self.clone(), 0),
        }
    }
    /// Pretty prints universe levels, used to impl Display for UniverseLevel.
    fn pretty_print(&self) -> String {
        match self.to_numeral() {
            Some(n) => n.to_string(),
            None => match self {
                Zero => unreachable!("Zero is a numeral"),
                Succ(_) => {
                    let (u, n) = self.plus();
                    format!("{} + {}", u.pretty_print(), n)
                }
                Max(box n, box m) => format!("max ({}) ({})", n.pretty_print(), m.pretty_print()),
                IMax(box _, box Zero) => "Zero".into(),
                IMax(box n, box m @ Succ(_)) => {
                    format!("max ({}) ({})", n.pretty_print(), m.pretty_print())
                }
                IMax(box n, box m) => format!("imax ({}) ({})", n.pretty_print(), m.pretty_print()),
                Var(n) => format!("u{}", n),
            },
        }
    }

    /// Is used to find the number of universe in a declaration.
    pub fn univ_vars(self) -> usize {
        match self {
            Zero => 0,
            Succ(n) => n.univ_vars(),
            Max(n, m) => n.univ_vars().max(m.univ_vars()),
            IMax(n, m) => n.univ_vars().max(m.univ_vars()),
            Var(n) => n + 1,
        }
    }

    /// Helper function for equality checking, used to substitute Var(i) with Z and S(Var(i)).
    fn substitute_single(self, var: usize, u: UniverseLevel) -> UniverseLevel {
        match self {
            Zero => Zero,
            Succ(n) => Succ(box n.substitute_single(var, u)),
            Max(n, m) => Max(
                box n.substitute_single(var, u.clone()),
                box m.substitute_single(var, u),
            ),
            IMax(n, m) => IMax(
                box n.substitute_single(var, u.clone()),
                box m.substitute_single(var, u),
            ),
            Var(n) => {
                if n == var {
                    u
                } else {
                    Var(n)
                }
            }
        }
    }

    /// General universe substitution, given a vector of universe levels, it substitutes each Var(i) with the i-th element of the vector.
    pub fn substitute(self, univs: &[UniverseLevel]) -> Self {
        match self {
            Zero => Zero,
            Succ(v) => Succ(box v.substitute(univs)),
            Max(u1, u2) => Max(box u1.substitute(univs), box u2.substitute(univs)),
            IMax(u1, u2) => IMax(box u1.substitute(univs), box u2.substitute(univs)),
            Var(var) => univs[var].clone(),
        }
    }

    /// Helper function for universe comparison. normalizes imax(es) as follows:
    /// imax(0, u) = u
    /// imax(u, 0) = u
    /// imax(u, S(v)) = max(u, S(v))
    /// imax(u, imax(v, w)) = max(imax(u, w), imax(v, w))
    /// imax(u, max(v, w)) = max(imax(u, v), imax(u, w))
    ///
    /// Here, the imax normalization pushes imaxes to all have a Var(i) as the second argument. To solve this last case, one needs to substitute
    /// Var(i) with 0 and S(Var(i)). This gives us a consistent way to unstuck the geq-checking.
    fn normalize(&self) -> Self {
        match self {
            IMax(box Zero, box u) => u.clone(),
            IMax(_, box Zero) => Zero,
            IMax(box u, box Succ(v)) => Max(box u.normalize(), box Succ(v.clone())).normalize(),
            IMax(box u, box IMax(box v, box w)) => Max(
                box IMax(box u.clone(), box w.clone()).normalize(),
                box IMax(box v.clone(), box w.clone()).normalize(),
            ),
            IMax(box u, box Max(box v, box w)) => Max(
                box IMax(box u.clone(), box v.clone()).normalize(),
                box IMax(box u.clone(), box w.clone()).normalize(),
            ),
            _ => self.clone(),
        }
    }

    // checks whether u1 <= u2 + n
    // returns:
    // -   (true,0) if u1 <= u2 + n,
    // -   (false,0) if !(u1 <= u2 + n),
    // -   (false,i+1) if Var(i) needs to be substituted to unstuck the comparison.
    fn geq_no_subst(&self, u2: &UniverseLevel, n: i64) -> (bool, usize) {
        match (self.normalize(), u2.normalize()) {
            (Zero, _) if n >= 0 => (true, 0),
            (_, _) if *self == *u2 && n >= 0 => (true, 0),
            (Succ(l), _) if l.geq_no_subst(u2, n - 1).0 => (true, 0),
            (_, Succ(box l)) if self.geq_no_subst(&l, n + 1).0 => (true, 0),
            (_, Max(box l1, box l2))
                if self.geq_no_subst(&l1, n).0 || self.geq_no_subst(&l2, n).0 =>
            {
                (true, 0)
            }
            (Max(box l1, box l2), _) if l1.geq_no_subst(u2, n).0 && l2.geq_no_subst(u2, n).0 => {
                (true, 0)
            }
            (_, IMax(_, box Var(i))) | (IMax(_, box Var(i)), _) => (false, i + 1),
            _ => (false, 0),
        }
    }

    /// Checks whether u1 <= u2 + n
    // In a case where comparison is stuck because of a variable Var(i), it checks whether the test is correct when Var(i) is substituted for
    // 0 and S(Var(i)).
    fn geq(&self, u2: &UniverseLevel, n: i64) -> bool {
        match self.geq_no_subst(u2, n) {
            (true, _) => true,
            (false, 0) => false,
            (false, i) => {
                self.clone()
                    .substitute_single(i - 1, Zero)
                    .geq(&u2.clone().substitute_single(i - 1, Zero), n)
                    && self
                        .clone()
                        .substitute_single(i - 1, Succ(box Var(i - 1)))
                        .geq(
                            &u2.clone().substitute_single(i - 1, Succ(box Var(i - 1))),
                            n,
                        )
            }
        }
    }

    pub fn is_eq(&self, u2: &UniverseLevel) -> bool {
        self.geq(u2, 0) && u2.geq(self, 0)
    }
}

#[cfg(test)]
mod tests {
    use crate::universe::UniverseLevel::*;

    mod pretty_printing {
        use crate::universe::UniverseLevel::*;

        #[test]
        fn to_num() {
            assert_eq!(Max(box Succ(box Zero), box Zero).to_numeral(), Some(1));
            assert_eq!(
                Max(box Succ(box Zero), box Succ(box Var(0))).to_numeral(),
                None
            );
            assert_eq!(IMax(box Var(0), box Zero).to_numeral(), Some(0));
            assert_eq!(IMax(box Zero, box Succ(box Zero)).to_numeral(), Some(1))
        }

        #[test]
        fn to_plus() {
            assert_eq!(Succ(box Zero).plus(), (Zero, 1));
        }

        #[test]
        fn to_pretty_print() {
            assert_eq!(
                Max(
                    box Succ(box Zero),
                    box IMax(box Max(box Zero, box Var(0)), box Succ(box Var(0)))
                )
                .pretty_print(),
                "max (1) (imax (max (0) (u0)) (u0 + 1))"
            );
        }
    }

    #[test]
    fn univ_eq() {
        assert!(&Zero.is_eq(&Default::default()));
        assert!(!&Zero.is_eq(&Succ(box Zero)));
        assert!(!&Succ(box Zero).is_eq(&Zero));
        assert!(&Var(0).is_eq(&Max(box Zero, box Var(0))));
        assert!(&Max(box Zero, box Var(0)).is_eq(&Var(0)));
        assert!(&Max(box Var(1), box Var(0)).is_eq(&Max(box Var(0), box Var(1))));
        assert!(!&Max(box Var(1), box Var(1)).is_eq(&Max(box Var(0), box Var(1))));
        assert!(&Succ(box Max(box Var(1), box Var(0)))
            .is_eq(&Max(box Succ(box Var(0)), box Succ(box Var(1)))));
        assert!(&Max(
            box Zero,
            box IMax(box Zero, box Max(box Succ(box Zero), box Zero))
        )
        .is_eq(&IMax(
            box Succ(box Zero),
            box IMax(box Succ(box Zero), box Succ(box Zero))
        )));
        assert!(&Var(0).is_eq(&IMax(box Var(0), box Var(0))));
        assert!(&IMax(box Succ(box Zero), box Max(box Zero, box Zero)).is_eq(&Zero));
        assert!(!&IMax(box Var(0), box Var(1)).is_eq(&IMax(box Var(1), box Var(0))))
    }

    #[test]
    fn univ_vars_count() {
        assert_eq!(
            IMax(
                box Zero,
                box Max(box Succ(box Zero), box Max(box Var(0), box Var(1)))
            )
            .univ_vars(),
            2
        )
    }

    #[test]
    fn subst() {
        let lvl = IMax(
            box Zero,
            box Max(box Succ(box Zero), box Max(box Var(0), box Var(1))),
        );
        let subst = vec![Succ(box Zero), Zero];
        assert_eq!(
            lvl.substitute(&subst),
            IMax(
                box Zero,
                box Max(box Succ(box Zero), box Max(box Succ(box Zero), box Zero))
            )
        )
    }

    #[test]
    fn single_subst() {
        let lvl = IMax(box Max(box Succ(box Zero), box Var(0)), box Var(0));
        assert_eq!(
            lvl.substitute_single(0, Zero),
            IMax(box Max(box Succ(box Zero), box Zero), box Zero)
        )
    }
}
