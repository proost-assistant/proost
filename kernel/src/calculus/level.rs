use Payload::*;

use crate::memory::arena::Arena;
use crate::memory::level::{Level, Payload};

impl<'arena> Level<'arena> {
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
    fn substitute_single(self, var: usize, u: Self, arena: &mut Arena<'arena>) -> Self {
        match *self {
            Succ(n) => n.substitute_single(var, u, arena).succ(arena),
            Max(n, m) => Level::max(n.substitute_single(var, u, arena), m.substitute_single(var, u, arena), arena),
            IMax(n, m) => Level::imax(n.substitute_single(var, u, arena), m.substitute_single(var, u, arena), arena),
            Var(n) if n == var => u,
            _ => self,
        }
    }

    /// General universe substitution, given a vector of universe levels, it substitutes each Var(i) with the i-th element of the
    /// vector.
    pub fn substitute(self, univs: &[Self], arena: &mut Arena<'arena>) -> Self {
        match *self {
            Zero => self,
            Succ(n) => n.substitute(univs, arena).succ(arena),
            Max(n, m) => Level::max(n.substitute(univs, arena), m.substitute(univs, arena), arena),
            IMax(n, m) => Level::imax(n.substitute(univs, arena), m.substitute(univs, arena), arena),
            Var(var) => univs[var],
        }
    }

    // checks whether u1 <= u2 + n
    // returns:
    // - (true,0) if u1 <= u2 + n,
    // - (false,0) if !(u1 <= u2 + n),
    // - (false,i+1) if Var(i) needs to be substituted to unstuck the comparison.
    fn geq_no_subst(self, u2: Self, n: i64) -> (bool, usize) {
        match (&*self,&*u2) {
            (Zero, _) if n >= 0 => (true, 0),
            (_, _) if self == u2 && n >= 0 => (true, 0),
            (Succ(l), _) if l.geq_no_subst(u2, n - 1).0 => (true, 0),
            (_, Succ(l)) if self.geq_no_subst(*l, n + 1).0 => (true, 0),
            (_, Max(l1, l2))
                if self.geq_no_subst(*l1, n).0 || self.geq_no_subst(*l2, n).0 =>
            {
                (true, 0)
            }
            (Max(l1, l2), _) if l1.geq_no_subst(u2, n).0 && l2.geq_no_subst(u2, n).0 => {
                (true, 0)
            }
            (_, IMax(_, v)) | (IMax(_, v), _) if let Var(i) = **v => (false, i + 1),
            _ => (false, 0),
        }
    }

    /// Checks whether u1 <= u2 + n
    // In a case where comparison is stuck because of a variable Var(i), it checks whether the test is correct when Var(i) is
    // substituted for 0 and S(Var(i)).
    pub fn geq(self, u2: Self, n: i64, arena: &mut Arena<'arena>) -> bool {
        match self.geq_no_subst(u2, n) {
            (true, _) => true,
            (false, 0) => false,
            (false, i) => {
                let zero = Level::zero(arena);
                let vv = Level::var(i - 1, arena).succ(arena);
                self.substitute_single(i - 1, zero, arena).geq(u2.substitute_single(i - 1, zero, arena), n, arena)
                    && self.substitute_single(i - 1, vv, arena).geq(u2.substitute_single(i - 1, vv, arena), n, arena)
            },
        }
    }

    pub fn is_eq(self, u2: Self, arena: &mut Arena<'arena>) -> bool {
        self.geq(u2, 0, arena) && u2.geq(self, 0, arena)
    }
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn univ_eq() {
//         assert!(!&Zero.is_eq(&Succ(box Zero)));
//         assert!(!&Succ(box Zero).is_eq(&Zero));
//         assert!(&Var(0).is_eq(&Max(box Zero, box Var(0))));
//         assert!(&Max(box Zero, box Var(0)).is_eq(&Var(0)));
//         assert!(&Max(box Var(1), box Var(0)).is_eq(&Max(box Var(0), box Var(1))));
//         assert!(!&Max(box Var(1), box Var(1)).is_eq(&Max(box Var(0), box Var(1))));
//         assert!(&Succ(box Max(box Var(1), box Var(0)))
//             .is_eq(&Max(box Succ(box Var(0)), box Succ(box Var(1)))));
//         assert!(&Max(
//             box Zero,
//             box IMax(box Zero, box Max(box Succ(box Zero), box Zero))
//         )
//         .is_eq(&IMax(
//             box Succ(box Zero),
//             box IMax(box Succ(box Zero), box Succ(box Zero))
//         )));
//         assert!(&Var(0).is_eq(&IMax(box Var(0), box Var(0))));
//         assert!(&IMax(box Succ(box Zero), box Max(box Zero, box Zero)).is_eq(&Zero));
//         assert!(!&IMax(box Var(0), box Var(1)).is_eq(&IMax(box Var(1), box Var(0))))
//     }
//
//     #[test]
//     fn univ_vars_count() {
//         assert_eq!(
//             IMax(
//                 box Zero,
//                 box Max(box Succ(box Zero), box Max(box Var(0), box Var(1)))
//             )
//             .univ_vars(),
//             2
//         )
//     }
//
//     #[test]
//     fn subst() {
//         let lvl = IMax(
//             box Zero,
//             box Max(box Succ(box Zero), box Max(box Var(0), box Var(1))),
//         );
//         let subst = vec![Succ(box Zero), Zero];
//         assert_eq!(
//             lvl.substitute(&subst),
//             IMax(
//                 box Zero,
//                 box Max(box Succ(box Zero), box Max(box Succ(box Zero), box Zero))
//             )
//         )
//     }
//
//     #[test]
//     fn single_subst() {
//         let lvl = IMax(box Max(box Succ(box Zero), box Var(0)), box Var(0));
//         assert_eq!(
//             lvl.substitute_single(0, Zero),
//             IMax(box Max(box Succ(box Zero), box Zero), box Zero)
//         )
//     }
// }
