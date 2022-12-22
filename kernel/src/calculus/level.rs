//! A set of useful functions to operate on [`Level`]s.

use Payload::*;

use crate::memory::arena::Arena;
use crate::memory::level::{Level, Payload};

impl<'arena> Level<'arena> {
    /// Helper function for equality checking, used to substitute `Var(i)` with `Z` and `S(Var(i))`.
    fn substitute_single(self, var: usize, u: Self, arena: &mut Arena<'arena>) -> Self {
        match *self {
            Succ(n) => n.substitute_single(var, u, arena).succ(arena),
            Max(n, m) => Level::max(n.substitute_single(var, u, arena), m.substitute_single(var, u, arena), arena),
            IMax(n, m) => Level::imax(n.substitute_single(var, u, arena), m.substitute_single(var, u, arena), arena),
            Var(n) if n == var => u,
            _ => self,
        }
    }

    /// Substitutes all level variables in `self` according to `univs`.
    ///
    /// This function makes no verification that `univs` has an appropriate size, which is way it
    /// cannot be made public.
    pub(crate) fn substitute(self, univs: &[Self], arena: &mut Arena<'arena>) -> Self {
        match *self {
            Zero => self,
            Succ(n) => n.substitute(univs, arena).succ(arena),
            Max(n, m) => Level::max(n.substitute(univs, arena), m.substitute(univs, arena), arena),
            IMax(n, m) => Level::imax(n.substitute(univs, arena), m.substitute(univs, arena), arena),
            Var(var) => univs[var],
        }
    }

    /// Checks whether `self <= rhs + n`, without applying substitution.
    ///
    /// Precisely, it returns:
    /// - `(false, i + 1)` if `Var(i)` needs to be substituted to unstuck the comparison.
    /// - `(true, 0)` if `self <= rhs + n`,
    /// - `(false, 0)` else.
    fn geq_no_subst(self, rhs: Self, n: i64) -> (bool, usize) {
        match (&*self, &*rhs) {
            (Zero, _) if n >= 0 => (true, 0),
            (_, _) if self == rhs && n >= 0 => (true, 0),
            (Succ(l), _) if l.geq_no_subst(rhs, n - 1).0 => (true, 0),
            (_, Succ(l)) if self.geq_no_subst(*l, n + 1).0 => (true, 0),
            (_, Max(l1, l2))
                if self.geq_no_subst(*l1, n).0 || self.geq_no_subst(*l2, n).0 =>
            {
                (true, 0)
            }
            (Max(l1, l2), _) if l1.geq_no_subst(rhs, n).0 && l2.geq_no_subst(rhs, n).0 => {
                (true, 0)
            }
            (_, IMax(_, v)) | (IMax(_, v), _) if let Var(i) = **v => (false, i + 1),
            _ => (false, 0),
        }
    }

    /// Checks whether `self <= rhs + n`.
    // In a case where comparison is stuck because of a variable Var(i), it checks whether the test is correct when Var(i) is
    // substituted for 0 and S(Var(i)).
    pub fn geq(self, rhs: Self, n: i64, arena: &mut Arena<'arena>) -> bool {
        match self.geq_no_subst(rhs, n) {
            (true, _) => true,
            (false, 0) => false,
            (false, i) => {
                let zero = Level::zero(arena);
                let vv = Level::var(i - 1, arena).succ(arena);
                self.substitute_single(i - 1, zero, arena).geq(rhs.substitute_single(i - 1, zero, arena), n, arena)
                    && self.substitute_single(i - 1, vv, arena).geq(rhs.substitute_single(i - 1, vv, arena), n, arena)
            },
        }
    }

    /// Checks whether `self = rhs`.
    ///
    /// This is a "conversion" equality test, not the equality function used by [`PartialEq`].
    pub fn is_eq(self, rhs: Self, arena: &mut Arena<'arena>) -> bool {
        self.geq(rhs, 0, arena) && rhs.geq(self, 0, arena)
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::arena::use_arena;
    use crate::memory::level::builder::raw::*;
    use crate::memory::level::Level;

    #[test]
    fn univ_eq() {
        use_arena(|arena| {
            let one = arena.build_level_raw(succ(zero()));
            let zero_ = arena.build_level_raw(zero());
            let var0 = Level::var(0, arena);
            let var1 = Level::var(1, arena);
            let max0_var0 = Level::max(zero_, var0, arena);
            let max_var0_var1 = Level::max(var0, var1, arena);
            let max_var1_var0 = Level::max(var1, var0, arena);
            let max_var1_var1 = Level::max(var1, var1, arena);
            let succ_max_var0_var1 = Level::succ(max_var0_var1, arena);
            let max_succ_var0_succ_var1 = arena.build_level_raw(max(succ(var(0)), succ(var(1))));

            assert!(!zero_.is_eq(one, arena));
            assert!(!one.is_eq(zero_, arena));
            assert!(var0.is_eq(max0_var0, arena));
            assert!(max0_var0.is_eq(var0, arena));
            assert!(max_var0_var1.is_eq(max_var1_var0, arena));
            assert!(!max_var1_var1.is_eq(max_var1_var0, arena));
            assert!(succ_max_var0_var1.is_eq(max_succ_var0_succ_var1, arena));
            assert!(var0.is_eq(Level::imax(var0, var0, arena), arena));
            assert!(arena.build_level_raw(imax(succ(zero()), max(zero(), zero()))).is_eq(zero_, arena));
            assert!(!Level::imax(var0, var1, arena).is_eq(Level::imax(var1, var0, arena), arena));

            assert!(
                (arena.build_level_raw(max(zero(), imax(zero(), max(succ(zero()), zero())))))
                    .is_eq(arena.build_level_raw(imax(succ(zero()), imax(succ(zero()), succ(zero())))), arena)
            )
        });
    }

    #[test]
    fn subst() {
        use_arena(|arena| {
            let lvl = arena.build_level_raw(imax(zero(), max(succ(zero()), max(var(0), var(1)))));
            let subst = vec![arena.build_level_raw(succ(zero())), arena.build_level_raw(zero())];
            assert_eq!(
                lvl.substitute(&subst, arena),
                arena.build_level_raw(imax(zero(), max(succ(zero()), max(succ(zero()), zero()))))
            )
        })
    }
    #[test]
    fn single_subst() {
        use_arena(|arena| {
            let lvl = arena.build_level_raw(imax(max(succ(zero()), var(0)), var(0)));
            assert_eq!(
                lvl.substitute_single(0, Level::zero(arena), arena),
                arena.build_level_raw(imax(max(succ(zero()), zero()), zero()))
            )
        })
    }
}
