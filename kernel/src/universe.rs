use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Add;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum UniverseLevel {
    #[default]
    Zero,

    Succ(Box<UniverseLevel>),

    Max(Box<UniverseLevel>, Box<UniverseLevel>),

    Var(usize),
}

//Each declaration in the global context has a number corresponding to its number of universe parameters.
//things to worry about : how to check conversion between universe polymorphic types ? only check universe_eq between them ? might be incomplete.
// TODO extend universe checking.

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
                Succ(_) => match self.plus() {
                    (u, 0) => format!("{}", u.pretty_print()),
                    (u, n) => format!("{} + {}", u.pretty_print(), n),
                },
                Max(box n, box m) => format!("max ({}) ({})", n.pretty_print(), m.pretty_print()),
                Var(n) => format!("u{}", n),
            },
        }
    }

    pub fn univ_vars(self) -> usize {
        match self {
            Zero => 0,
            Succ(n) => n.univ_vars(),
            Max(n, m) => n.univ_vars().max(m.univ_vars()),
            Var(n) => n + 1,
        }
    }

    pub fn substitute(self, univs: &[UniverseLevel]) -> Self {
        match self {
            Zero => Zero,
            Succ(v) => Succ(box v.substitute(univs)),
            Max(u1, u2) => Max(box u1.substitute(univs), box u2.substitute(univs)),
            Var(var) => univs[var].clone(),
        }
    }

    fn geq(&self, u2: &UniverseLevel, n: i64) -> bool {
        match (self, u2) {
            (Zero, _) if n >= 0 => true,
            (_, _) if self == u2 && n >= 0 => true,
            (Succ(l), _) if l.geq(u2, n - 1) => true,
            (_, Succ(box l)) if self.geq(l, n + 1) => true,
            (_, Max(box l1, box l2)) if self.geq(l1, n) || self.geq(l2, n) => true,
            (Max(box l1, box l2), _) if l1.geq(u2, n) && l2.geq(u2, n) => true,
            _ => false,
        }
    }

    pub fn is_eq(&self, u2: &UniverseLevel) -> bool {
        self.geq(u2, 0) && u2.geq(self, 0)
    }
}

#[cfg(test)]
mod tests {
    use crate::universe::UniverseLevel::*;

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
    }

    #[test]
    fn univ_vars_count() {
        assert_eq!(
            Max(box Succ(box Zero), box Max(box Var(0), box Var(1))).univ_vars(),
            2
        )
    }

    #[test]
    fn subst() {
        let lvl = Max(box Succ(box Zero), box Max(box Var(0), box Var(1)));
        let subst = vec![Succ(box Zero), Zero];
        assert_eq!(
            lvl.substitute(&subst),
            Max(box Succ(box Zero), box Max(box Succ(box Zero), box Zero))
        )
    }
}
