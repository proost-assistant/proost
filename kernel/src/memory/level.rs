use std::fmt::Display;
use std::fmt::Formatter;

use std::marker::PhantomData;
use std::cell::OnceCell;
use derive_more::{Add, Display, From, Into, Sub};
use std::hash::Hash;
use super::Arena;
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Level<'arena>(&'arena Node<'arena>, PhantomData<*mut &'arena ()>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Node<'arena> {
    payload: Payload<'arena>,

    // put any lazy structure here
    // normalized has been removed, because all levels are guaranteed to be reduced
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Payload<'arena> {
    Zero,

    Succ(Level<'arena>),

    Max(Level<'arena>,Level<'arena>),

    IMax(Level<'arena>,Level<'arena>),

    Var(usize),
}
use Payload::*;

impl<'arena> super::arena::Arena<'arena> {
    /// This function is the base low-level function for creating terms.
    ///
    /// It enforces the uniqueness property of terms in the arena.
    fn hashcons_level(&mut self, payload: Payload<'arena>) -> Level<'arena> {
        let new_node = Node { payload, };

        match self.hashcons_terms.get(&new_node) {
            Some(level) => level,
            None => {
                let reduced = Level(&new_node, PhantomData).reduce(self);
                let reduced = Level(self.alloc.alloc(reduced.0), PhantomData);
                self.hashcons_levels.insert(&new_node, reduced);
                self.hashcons_levels.insert(&reduced, reduced);
                reduced
            }
        }
    }

    /// Returns the term corresponding to a proposition
    pub(crate) fn zero(&mut self) -> Level<'arena> {
        self.hashcons_level(Zero)
    }

    pub(crate) fn succ(&mut self, level: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(Succ(level))
    }

    pub(crate) fn max(&mut self, l: Level<'arena>, r: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(Max(l, r))
    }

    pub(crate) fn imax_level(&mut self, l: Level<'arena>, r: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(IMax(l, r))
    }

    pub(crate) fn var_level(&mut self, id: usize) -> Level<'arena> {
        self.hashcons_level(Var(id))
    }
}

impl<'arena> Hash for Level<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<'arena> Hash for LevelNode<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
    }
}

impl<'arena> Level<'arena> {
    fn add(self, n: usize, arena: &mut Arena<'arena>) -> Self {
        if n == 0 {
            self
        } else {
            let s = self.add(n - 1, arena);
            arena.succ(s)
        }
    }

    fn from(n: usize, arena: &mut Arena<'arena>) -> Self {
        let z = arena.zero();
        z.add(n, arena)
    }
}

impl Display for Level<'_> {
    fn fmt(self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.to_numeral() { Some(n) => write!(f, "{n}"),
            None => match self {
                Zero => unreachable!("Zero is a numeral"),
                Succ(_) => {
                    let (u, n) = self.plus();
                    write!(f, "{u} + {n}")
                }
                Max(n, m) => write!(f, "max ({n}) ({m})"),
                IMax(n, m) => write!(f, "imax ({n}) ({m})"),
                Var(n) => write!(f, "u{n}"),
            },
        }
    }
}


impl<'arena> Level<'arena> {
    /// Helper function for pretty printing, if universe doesn't contain any variable then it gets printed as a decimal number.
    fn to_numeral(self) -> Option<usize> {
        match *self {
            Zero => Some(0),
            Succ(u) => u.to_numeral().map(|n| n + 1),
            Max(n, m) | IMax(n, m) => n .to_numeral() .and_then(|n| m.to_numeral().map(|m| n.max(m))),
            // note: once uniqueness property is ensured with normalization, please remove the IMax case
            _ => None,
        }
    }

    /// Helper function for pretty printing, if universe is of the form Succ(Succ(...(Succ(u))...)) then it gets printed as u+n.
    fn plus(self) -> (UniverseLevel, usize) {
        match *self {
            Succ(u) => {
                let (u, n) = u.plus();
                (u, n + 1)
            }
            _ => (self, 0),
        }
    }

    // Is used to find the number of universe in a declaration.
    // This function is no longer in use
    // pub fn univ_vars(self) -> usize {
    //     match self {
    //         Zero => 0,
    //         Succ(n) => n.univ_vars(),
    //         Max(n, m) => n.univ_vars().max(m.univ_vars()),
    //         IMax(n, m) => n.univ_vars().max(m.univ_vars()),
    //         Var(n) => n + 1,
    //     }
    // }

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

/// A Level is arguably a smart pointer, and as such, can be directly dereferenced to its associated
/// payload.
///
/// Please note that this trait has some limits. For instance, the notations used to match against
/// a *pair* of terms still requires some convolution.
impl<'arena> Deref for Level<'arena> {
    type Target = Payload<'arena>;

    fn deref(&self) -> &Self::Target {
        &self.0.payload
    }
}

/// Debug mode only prints the payload of a term.
///
/// Apart from enhancing the debug readability, this reimplementation is surprisingly necessary:
/// because terms may refer to themselves in the payload, the default debug implementation
/// recursively calls itself and provokes a stack overflow.
impl<'arena> Debug for Term<'arena> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.payload.fmt(f)
    }
}

/// Because terms are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality.
impl<'arena> PartialEq<Level<'arena>> for Level<'arena> {
    fn eq(&self, x: &Term<'arena>) -> bool {
        std::ptr::eq(self.0, x.0)
    }
}

/// Because terms are unique in the arena, it is sufficient to compare their locations in memory to
/// test equality. In particular, hash can also be computed from the location.
impl<'arena> Hash for Level<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<'arena> PartialEq<Node<'arena>> for Node<'arena> {
    fn eq(&self, x: &Node<'arena>) -> bool {
        self.payload == x.payload
    }
}

/// Nodes are not guaranteed to be unique. Nonetheless, only the payload matters and characterises
/// the value. Which means computing the hash for nodes can be restricted to hashing their
/// payloads.
impl<'arena> Hash for Node<'arena> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.payload.hash(state);
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
