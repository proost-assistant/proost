//! Universe levels.

use std::cell::OnceCell;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

use super::arena::Arena;

super::arena::new_dweller!(Level, Header, Payload);

pub mod builder;

struct Header<'arena> {
    /// The plus-form of a level
    plus_form: OnceCell<(Level<'arena>, usize)>,
}

/// A universe level.
///
/// While types in the usual calculus of constructions live in types fully described with integers,
/// more is needed when manipulating universe-polymorphic descriptions: precisely, the right amount
/// of formal computation has to be introduced in order to account for universe-polymorphic
/// variables.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Payload<'arena> {
    /// The zero level (associated to Prop)
    Zero,

    /// The successor of a level
    Succ(Level<'arena>),

    /// The maximum of two levels
    Max(Level<'arena>, Level<'arena>),

    /// The impredicative maximum of two levels
    IMax(Level<'arena>, Level<'arena>),

    /// A universe-polymorphic variable
    Var(usize),
}

impl Display for Level<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.to_numeral() {
            Some(n) => write!(f, "{n}"),
            None => match **self {
                Zero => unreachable!("Zero is a numeral"),
                Succ(_) => {
                    let (u, n) = self.plus();
                    write!(f, "{u} + {n}")
                },
                Max(n, m) => write!(f, "max ({n}) ({m})"),
                IMax(n, m) => write!(f, "imax ({n}) ({m})"),
                Var(n) => write!(f, "u{n}"),
            },
        }
    }
}

use Payload::{IMax, Max, Succ, Var, Zero};

impl<'arena> Level<'arena> {
    /// This function is the base low-level function for creating levels.
    ///
    /// It enforces the uniqueness property of levels in the arena, as well as the reduced-form
    /// invariant.
    fn hashcons(payload: Payload<'arena>, arena: &mut Arena<'arena>) -> Self {
        let new_node = Node {
            payload,
            header: Header {
                plus_form: OnceCell::new(),
            },
        };

        match arena.hashcons_levels.get(&new_node) {
            Some(level) => *level,
            None => {
                // add the unreduced node to the arena
                let node_unreduced = &*arena.alloc.alloc(new_node);
                let level_unreduced = Level::new(node_unreduced);
                arena.hashcons_levels.insert(node_unreduced, level_unreduced);

                // compute its reduced form
                let reduced = level_unreduced.normalize(arena);

                // supersede the previous correspondence
                arena.hashcons_levels.insert(node_unreduced, reduced);
                arena.hashcons_levels.insert(reduced.0, reduced);

                reduced
            },
        }
    }

    /// Returns the 0-level
    pub(crate) fn zero(arena: &mut Arena<'arena>) -> Self {
        Self::hashcons(Zero, arena)
    }

    /// Returns the successor of a level
    pub(crate) fn succ(self, arena: &mut Arena<'arena>) -> Self {
        Self::hashcons(Succ(self), arena)
    }

    /// Returns the level corresponding to the maximum of two levels
    pub(crate) fn max(self, right: Self, arena: &mut Arena<'arena>) -> Self {
        Self::hashcons(Max(self, right), arena)
    }

    /// Returns the level corresponding to the impredicative maximum of two levels
    pub(crate) fn imax(self, right: Self, arena: &mut Arena<'arena>) -> Self {
        Self::hashcons(IMax(self, right), arena)
    }

    /// Returns the level associated to a given variable.
    pub(crate) fn var(id: usize, arena: &mut Arena<'arena>) -> Self {
        Self::hashcons(Var(id), arena)
    }

    /// The addition of a level and an integer
    #[inline]
    pub fn add(self, n: usize, arena: &mut Arena<'arena>) -> Self {
        if n == 0 {
            self
        } else {
            let s = self.add(n - 1, arena);
            s.succ(arena)
        }
    }

    /// Builds a level from an integer
    #[inline]
    pub fn from(n: usize, arena: &mut Arena<'arena>) -> Self {
        Level::zero(arena).add(n, arena)
    }

    /// Converts a level to an integer, if possible
    #[inline]
    pub fn to_numeral(self) -> Option<usize> {
        let (u, n) = self.plus();
        (*u == Zero).then_some(n)
    }

    /// Decomposes a level `l` in the best pair `(u, n)` s.t. `l = u + n`
    #[inline]
    pub fn plus(self) -> (Self, usize) {
        *self.0.header.plus_form.get_or_init(|| match *self {
            Succ(u) => {
                let (u, n) = u.plus();
                (u, n + 1)
            },
            _ => (self, 0),
        })
    }

    /// Helper function for universe comparison. normalizes imax(es) as follows:
    ///  - `imax(0, u) = u`
    ///  - `imax(u, 0) = u`
    ///  - `imax(u, S(v)) = max(u, S(v))`
    ///  - `imax(u, imax(v, w)) = max(imax(u, w), imax(v, w))`
    ///  - `imax(u, max(v, w)) = max(imax(u, v), imax(u, w))`
    ///
    /// The function also reduces max. This is further helpful when trying to print the type.
    ///
    /// Here, the imax normalization pushes imaxes to all have a `Var(i)` as the second argument. To solve this last case, one needs
    /// to substitute `Var(i)` with `0` and `S(Var(i))`. This gives us a consistent way to unstuck the geq-checking.
    fn normalize(self, arena: &mut Arena<'arena>) -> Self {
        match *self {
            IMax(u, v) => {
                if u == v {
                    u
                } else {
                    match &*v {
                        Zero => v,
                        Succ(_) => u.max(v, arena),
                        IMax(_, vw) => Level::max(u.imax(*vw, arena), v, arena),
                        Max(vv, vw) => Level::max(u.imax(*vv, arena), u.imax(*vw, arena), arena),
                        _ => self,
                    }
                }
            },

            Max(u, v) => {
                if u == v {
                    u
                } else {
                    match (&*u, &*v) {
                        (Zero, _) => v,
                        (_, Zero) => u,
                        (Succ(uu), Succ(vv)) => Level::max(*uu, *vv, arena).succ(arena),
                        _ => self,
                    }
                }
            },
            _ => self,
        }
    }
}

#[cfg(test)]
mod pretty_printing {

    use crate::memory::arena::use_arena;
    use crate::memory::level::builder::raw::*;
    use crate::memory::level::Level;

    #[test]
    fn to_num() {
        use_arena(|arena| {
            assert_eq!(arena.build_level_raw(max(succ(zero()), zero())).to_numeral(), Some(1));
            assert_eq!(arena.build_level_raw(max(succ(zero()), succ(var(0)))).to_numeral(), None);
            assert_eq!(arena.build_level_raw(imax(var(0), zero())).to_numeral(), Some(0));
            assert_eq!(arena.build_level_raw(imax(zero(), succ(zero()))).to_numeral(), Some(1));
        });
    }

    #[test]
    fn to_plus() {
        use_arena(|arena| {
            assert_eq!(arena.build_level_raw(succ(zero())).plus(), (Level::zero(arena), 1));
        });
    }

    #[test]
    fn to_pretty_print() {
        use_arena(|arena| {
            assert_eq!(
                format!("{}", arena.build_level_raw(max(succ(zero()), imax(max(zero(), var(0)), succ(var(0)))))),
                "max (1) (max (u0) (u0 + 1))"
            );
        });
    }

    #[test]
    fn normalize() {
        use_arena(|arena| {
            let lvl = arena.build_level_raw(imax(zero(), imax(zero(), imax(succ(zero()), var(0)))));

            assert_eq!(format!("{lvl}"), "max (imax (0) (u0)) (max (imax (0) (u0)) (imax (1) (u0)))");
        });
    }
}
