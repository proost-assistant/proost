use std::ops::Add;

use derive_more::Display;

#[derive(Clone, Debug, Default, Display, PartialEq, Eq)]
pub enum UniverseLevel {
    #[default]
    Zero,
    Succ(Box<UniverseLevel>),
    #[display(fmt = "max ({}) ({})", _0, _1)]
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

use UniverseLevel::*;

impl UniverseLevel {
    /*fn substitute(self, u: &UniverseLevel, lvl : usize) -> Self {
        match self {
            UnivZero => UnivZero,
            UnivSucc(v) => UnivSucc(box v.substitute(u),lvl),
            UnivMax(u1, u2) => UnivMax(box u1.substitute(u,lvl), box u2.substitute(u,lvl)),
            UnivVar(var) if var == lvl => u.clone(),
            UnivVar(var) if var>lvl => UnivVar(var - 1),
            _ => self
        }
    }*/

    fn geq(&self, u2: &UniverseLevel, n: i64) -> bool {
        match (self, u2) {
            (Zero, _) if n >= 0 => true,
            (_, _) if self == u2 && n >= 0 => true,
            (Succ(l), _) if l.geq(u2, n - 1) => true,
            (_, Succ(box l)) if self.geq(l, n + 1) => true,
            (_, Max(box l1, box l2)) if self.geq(l1, n) || self.geq(l2, n) => true,
            (Max(box l1, box l2), _) if l1.geq(u2, n) && l2.geq(u2, n) => true,
            _ => false
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
        assert!(format!("{}",Max(box Zero, box Zero)) == "max (Zero) (Zero)");
        assert!(&Zero.is_eq(&Default::default()));
        assert!(!&Zero.is_eq(&Succ(box Zero)));
        assert!(!&Succ(box Zero).is_eq(&Zero));
        assert!(&Var(0).is_eq(&Max(box Zero, box Var(0))));
        assert!(&Max(box Zero, box Var(0)).is_eq(&Var(0)));
        assert!(&Max(box Var(1), box Var(0)).is_eq(&Max(box Var(0), box Var(1))));
        assert!(!&Max(box Var(1), box Var(1)).is_eq(&Max(box Var(0), box Var(1))));
        assert!(&Succ(box Max(box Var(1), box Var(0))).is_eq(&Max(box Succ(box Var(0)), box Succ(box Var(1)))));
    }
}
