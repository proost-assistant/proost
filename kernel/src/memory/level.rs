use std::fmt::Display;
use std::fmt::Formatter;

use super::arena::Arena;
use std::cell::OnceCell;
use std::hash::Hash;
use std::marker::PhantomData;

super::arena::new_dweller!(Level, Header, Payload);

struct Header<'arena> {
    // put any lazy structure here
    // normalized has been removed, because all levels are guaranteed to be reduced
    //
    plus_form: OnceCell<(Level<'arena>, usize)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Payload<'arena> {
    Zero,

    Succ(Level<'arena>),

    Max(Level<'arena>, Level<'arena>),

    IMax(Level<'arena>, Level<'arena>),

    Var(usize),
}
use Payload::*;

impl<'arena> super::arena::Arena<'arena> {
    /// This function is the base low-level function for creating levels.
    ///
    /// It enforces the uniqueness property of levels in the arena, as well as the reduced-form
    /// invariant.
    fn hashcons_level(&mut self, payload: Payload<'arena>) -> Level<'arena> {
        let new_node = Node {
            payload,
            header: Header {plus_form: OnceCell::new()}
        };

        match self.hashcons_terms.get(&new_node) {
            Some(level) => level,
            None => {
                // note that this implies that an non-reduced version of the term will still live
                // in the arena, it will only be unreachable.
                let level_unreduced = Level(&*self.alloc.alloc(new_node), PhantomData);
                self.hashcons_levels.insert(&new_node, level_unreduced);

                let reduced = level_unreduced.reduce(self);
                self.hashcons_levels.insert(&new_node, reduced);
                self.hashcons_levels.insert(reduced.0, reduced);
                reduced
            }
        }
    }

    /// Returns the 0-level
    pub(crate) fn zero(&mut self) -> Level<'arena> {
        self.hashcons_level(Zero)
    }

    /// Returns the successor of a level
    pub(crate) fn succ(&mut self, level: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(Succ(level))
    }

    /// Returns the level corresponding to the maximum of two levels
    pub(crate) fn max(&mut self, l: Level<'arena>, r: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(Max(l, r))
    }

    /// Returns the level corresponding to the impredicative maximum of two levels
    ///
    /// The `_level` suffix disambiguates it from the imax function from the [type
    /// checker](crate::type_checker), as both are implementations for an arena.
    pub(crate) fn imax_level(&mut self, l: Level<'arena>, r: Level<'arena>) -> Level<'arena> {
        self.hashcons_level(IMax(l, r))
    }

    /// Returns the level associated to a given variable.
    ///
    /// The `_level` suffix disambiguates it from the var function defined for terms.
    pub(crate) fn var_level(&mut self, id: usize) -> Level<'arena> {
        self.hashcons_level(Var(id))
    }
}

impl<'arena> Level<'arena> {
    pub fn add(self, n: usize, arena: &mut Arena<'arena>) -> Self {
        if n == 0 {
            self
        } else {
            let s = self.add(n - 1, arena);
            arena.succ(s)
        }
    }

    pub fn from(n: usize, arena: &mut Arena<'arena>) -> Self {
        let z = arena.zero();
        z.add(n, arena)
    }

    /// Helper function for pretty printing, if universe doesn't contain any variable then it gets printed as a decimal number.
    fn to_numeral(self) -> Option<usize> {
        // TODO maybe refactor this along with `normalize` so as to make it efficient.
        // With normalization established as an invariant, it may be possible to then assume
        // that to_numeral(self) returns n if self.plus_form = (Zero, n)
        match *self {
            Zero => Some(0),
            Succ(u) => u.to_numeral().map(|n| n + 1),
            Max(n, m) | IMax(n, m) => n
                .to_numeral()
                .and_then(|n| m.to_numeral().map(|m| n.max(m))),
            // note: once uniqueness property is ensured with normalization, please remove the IMax case
            // (and that it has been verified this does not change anything)
            _ => None,
        }
    }

    /// Helper function for pretty printing, if universe is of the form Succ(Succ(...(Succ(u))...)) then it gets printed as u+n.
    fn init_plus_form(self, arena: &mut Arena<'arena>) {
        self.0.plus_form.get_or_init(|| match *self {
            Succ(u) => {
                let (u, n) = u.plus();
                (u, n + 1)
            }
            _ => (self, 0),
        })
    }

    fn plus(self) -> (Self, usize) {
        self.0.plus_form.get().unwrap()
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
    ///
    /// FIXME (to remove once correctly refactored): the initial self argument may not be reduced
    /// (because this function has to define what its reduced form is). Its children, on the other
    /// hand, are.
    fn normalize(self, arena: &mut Arena<'arena>) -> Self {
        self
        // TODO
        //match *self {
        //    IMax( Zero,  u) => u,
        //    IMax(_,  Zero) => Zero,
        //    IMax( u,  Succ(v)) => Max( u.normalize(self),  Succ(v())).normalize(),
        //    IMax( u,  IMax( v,  w)) => Max(
        //         IMax( u(),  w()).normalize(),
        //         IMax( v(),  w()).normalize(),
        //    ),
        //    IMax( u,  Max( v,  w)) => Max(
        //         IMax( u(),  v()).normalize(),
        //         IMax( u(),  w()).normalize(),
        //    ),
        //    _ => self(),
        //}
    }
}

impl Display for Level<'_> {
    fn fmt(self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.to_numeral() {
            Some(n) => write!(f, "{n}"),
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

#[cfg(test)]
mod tests {
    mod pretty_printing {
        //#[test]
        //fn to_num() {
        //    assert_eq!(Max( Succ( Zero),  Zero).to_numeral(), Some(1));
        //    assert_eq!(
        //        Max( Succ( Zero),  Succ( Var(0))).to_numeral(),
        //        None
        //    );
        //    assert_eq!(IMax( Var(0),  Zero).to_numeral(), Some(0));
        //    assert_eq!(IMax( Zero,  Succ( Zero)).to_numeral(), Some(1))
        //}

        //#[test]
        //fn to_plus() {
        //    assert_eq!(Succ( Zero).plus(), (Zero, 1));
        //}

        //#[test]
        //fn to_pretty_print() {
        //    assert_eq!(
        //        Max(
        //             Succ( Zero),
        //             IMax( Max( Zero,  Var(0)),  Succ( Var(0)))
        //        )
        //        .pretty_print(),
        //        "max (1) (imax (max (0) (u0)) (u0 + 1))"
        //    );
        //}
    }
}
