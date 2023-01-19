//! Set of axioms, typing and reduction rules for the equality type

use derive_more::Display;

use super::false_::False;
use super::natural::Natural;
use super::true_::True;
use super::{Axiom, AxiomKind};
use crate::memory::arena::Arena;
use crate::memory::declaration::Declaration;
use crate::memory::level::Level;
use crate::memory::term::Term;

/// Axioms regarding `Eq`uality
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Equality {
    /// The equality type
    ///
    /// The trailing underscore is needed because naming it `Eq` collides with [Eq](std::cmp::Eq).
    #[display(fmt = "Eq")]
    Eq_,

    /// Transport between proof-relevant types
    Cast,

    /// Transport between proof-irrelevant types
    Transp,

    /// The reflexivity predicate for the equality type
    Refl,
}

impl<'arena> AxiomKind<'arena> for Equality {
    fn append_to_named_axioms(arena: &mut Arena<'arena>) {
        let var0 = Level::var(0, arena);

        let decl = Declaration(Term::axiom(Axiom::Equality(Self::Eq_), &[var0], arena), 1);
        arena.bind_decl("Eq", decl);

        let decl = Declaration(Term::axiom(Axiom::Equality(Self::Cast), &[var0], arena), 1);
        arena.bind_decl("Cast", decl);

        let decl = Declaration(Term::axiom(Axiom::Equality(Self::Transp), &[var0], arena), 1);
        arena.bind_decl("Transp", decl);

        let decl = Declaration(Term::axiom(Axiom::Equality(Self::Refl), &[var0], arena), 1);
        arena.bind_decl("Refl", decl);
    }

    fn get_type(self, arena: &mut Arena<'arena>) -> Term<'arena> {
        match self {
            Self::Eq_ => Self::type_eq(arena),
            Self::Cast => Self::type_cast(arena),
            Self::Transp => Self::type_transp(arena),
            Self::Refl => Self::type_refl(arena),
        }
    }

    fn reduce(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        let recursors = [Self::reduce_eq, Self::reduce_cast];

        recursors.into_iter().find_map(|f| f(term, arena))
    }
}

impl Equality {
    /// Type of the Equality type : `Eq.{u} A x y : Prop`
    fn type_eq<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        let sort_u = Term::sort(Level::var(0, arena), arena);
        let prop = Term::sort_usize(0, arena);

        // Eq : (A : Sort u) -> A -> A -> Prop
        Term::prod(
            sort_u,
            Term::prod(Term::var(1.into(), sort_u, arena), Term::prod(Term::var(2.into(), sort_u, arena), prop, arena), arena),
            arena,
        )
    }

    /// Type of the reflexivity axiom :
    /// `Refl.{u} A x : Eq.{u} A x x`
    fn type_refl<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        let sort_u = Term::sort(Level::var(0, arena), arena);
        // Eq A a a
        let eq_refl = Term::app(
            Term::app(
                Term::app(
                    Term::axiom(Axiom::Equality(Self::Eq_), &[Level::var(0, arena)], arena),
                    Term::var(2.into(), sort_u, arena),
                    arena,
                ),
                Term::var(1.into(), Term::var(2.into(), sort_u, arena), arena),
                arena,
            ),
            Term::var(1.into(), Term::var(2.into(), sort_u, arena), arena),
            arena,
        );
        // (A : Sort u) -> (a : A) -> Eq A a a
        Term::prod(sort_u, Term::prod(Term::var(1.into(), sort_u, arena), eq_refl, arena), arena)
    }

    /// Type of the `Cast` axiom
    /// (A B : Sort u) -> Eq (Sort u) A B -> A -> B
    fn type_cast<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        let sort_u = Term::sort(Level::var(0, arena), arena);
        let a_eq_b = Term::axiom(Axiom::Equality(Self::Eq_), &[Level::succ(Level::var(0, arena), arena)], arena)
            .app(Term::sort(Level::var(0, arena), arena), arena)
            .app(Term::var(2.into(), sort_u, arena), arena)
            .app(Term::var(1.into(), sort_u, arena), arena);

        // (A B : Sort u) -> Eq (Sort u) A B -> A -> B
        sort_u.prod(
            sort_u.prod(
                a_eq_b.prod(Term::var(3.into(), sort_u, arena).prod(Term::var(3.into(), sort_u, arena), arena), arena),
                arena,
            ),
            arena,
        )
    }

    /// Type of the `Transp` axiom
    /// (A : Sort u) -> (t1 : A) -> (B : A -> Prop) -> (u: B t1) -> (t2 : A) -> Eq A t1 t2 -> B t2

    fn type_transp<'arena>(arena: &mut Arena<'arena>) -> Term<'arena> {
        let sort_u = Term::sort(Level::var(0, arena), arena);
        let prop = Term::sort(Level::zero(arena), arena);

        let eq_a_t1_t2 = Term::axiom(Axiom::Equality(Self::Eq_), &[Level::var(0, arena)], arena)
            .app(Term::var(5.into(), sort_u, arena), arena)
            .app(Term::var(4.into(), Term::var(5.into(), sort_u, arena), arena), arena)
            .app(Term::var(1.into(), Term::var(5.into(), sort_u, arena), arena), arena);

        let b = Term::var(2.into(), sort_u, arena).prod(prop, arena);

        let u = Term::var(1.into(), Term::var(3.into(), sort_u, arena).prod(prop, arena), arena)
            .app(Term::var(2.into(), Term::var(3.into(), sort_u, arena), arena), arena);

        let b_t2 = Term::var(4.into(), Term::var(6.into(), sort_u, arena).prod(prop, arena), arena)
            .app(Term::var(2.into(), Term::var(6.into(), sort_u, arena), arena), arena);

        // (A : Sort u) -> (t1 : A) -> (B : A -> Prop) -> (u: B t1) -> (t2 : A) -> Eq A t1 t2 -> B t2
        sort_u.prod(
            Term::var(1.into(), sort_u, arena).prod(
                b.prod(u.prod(Term::var(4.into(), sort_u, arena).prod(eq_a_t1_t2.prod(b_t2, arena), arena), arena), arena),
                arena,
            ),
            arena,
        )
    }

    /// Reduces `Eq.{1} Nat self rhs` by checking the whnf of self and rhs as such: match (self,rhs)
    /// | 0,0 => True
    /// | S k, S n => Eq.{1} Nat k n
    /// | 0, S _ | S _, 0 => False
    fn reduce_nat_eq<'arena>(term: Term<'arena>, rhs: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::{App, Axiom};
        match *term.whnf(arena) {
            Axiom(super::Axiom::Natural(Natural::Zero),_) => {
                if let Axiom(super::Axiom::Natural(Natural::Zero),_) = *rhs.whnf(arena) {
                    Some(Term::axiom(super::Axiom::True(True::True),&[],arena))
                } else {
                    let App(s,_) = *rhs.whnf(arena) else { return None; };
                    let Axiom(super::Axiom::Natural(Natural::Succ),_) = *s.whnf(arena) else { return None; };
                    Some(Term::axiom(super::Axiom::False(False::False),&[],arena))
                }
            },
            App(s,k) if let Axiom(super::Axiom::Natural(Natural::Succ),_) = *s.whnf(arena) => {
                if let App(s,n) = *rhs.whnf(arena) {
                if let Axiom(super::Axiom::Natural(Natural::Succ),_) = *s.whnf(arena) {
                    let eq = Term::axiom(super::Axiom::Equality(Self::Eq_), &[Level::succ(Level::zero(arena),arena)],arena)
                        .app(Term::axiom(super::Axiom::Natural(Natural::Nat),&[],arena),arena)
                        .app(k,arena)
                        .app(n,arena);
                     Some(eq)
                } else { None }}
                else {
                    let Axiom(super::Axiom::Natural(Natural::Zero),_) = *rhs.whnf(arena) else { return None; };
                    Some(Term::axiom(super::Axiom::False(False::False),&[],arena))
                }
            },
            _ => None
        }
    }

    /// reduces relevant functions equality as follows : Eq (A -> B) f g => fun x:A => Eq (B x) (f x) (g x)
    fn reduce_fun_eq<'arena>(
        term: Term<'arena>,
        rhs: Term<'arena>,
        ty1: Term<'arena>,
        ty2: Term<'arena>,
        arena: &mut Arena<'arena>,
    ) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::Sort;
        let sort_ty2 = ty2.infer(arena).ok()?.whnf(arena);
        if let Sort(lvl) = *sort_ty2 {
            let x = Term::var(1.into(), ty1.shift(1, 0, arena), arena);
            let eq = Term::axiom(crate::axiom::Axiom::Equality(Self::Eq_), &[lvl], arena)
                .app(ty2.substitute(x, 1, arena), arena)
                .app(term.shift(1, 0, arena).app(x, arena), arena)
                .app(rhs.shift(1, 0, arena).app(x, arena), arena);
            Some(ty1.prod(eq, arena))
        } else {
            unreachable!("Expected a Sort, found {sort_ty2}, this should definitely never happen")
        }
    }

    // Reduces a term if it is an instance of the Eq reducer, returns None otherwise.
    // I'm marking it as `no_coverage` for now, but proofs that reduction works can be found in `examples/eq.mdln`.
    /// Reduces equality according to the observational equality reduction. This function is extensively big, and will
    /// have to be extended each time a new type gets added to the lot to add according reduction rules
    #[no_coverage]
    fn reduce_eq<'arena>(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::{App, Axiom, Prod};
        // Be aware that this function will not be automatically formatted, because of the
        // experimental status of let-chains, as well as that of if-let conditions in pattern matching.
        let App(f, y) = *term.whnf(arena)  else { return None; };
        let App(f, x) = *f.whnf(arena)  else { return None; };
        let App(f,ty) = *f.whnf(arena)  else { return None; };
        let Axiom(crate::axiom::Axiom::Equality(Self::Eq_), _) = *f.unfold(arena).whnf(arena) else { return None; };
        match *ty.whnf(arena) {
            // Eq-Zero/Succ/Zero-Succ/Succ-Zero
            Axiom(crate::axiom::Axiom::Natural(Natural::Nat), _) => Self::reduce_nat_eq(x, y, arena),
            Prod(ty1, ty2) if x.is_relevant(arena) => Self::reduce_fun_eq(x, y, ty1, ty2, arena),
            // TODO Eq-Fun,Eq-Univ,Eq-univ-!=, Eq-Pi
            _ => None,
        }
    }

    /// Helper function to reduce `Cast`s for `Nat`s
    fn reduce_nat_cast<'arena>(term: Term<'arena>, e: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::{App, Axiom};
        match *term.whnf(arena) {
            Axiom(super::Axiom::Natural(Natural::Zero), _) => Some(Term::axiom(super::Axiom::Natural(Natural::Zero), &[],arena)),
            App(s,k) if let Axiom(super::Axiom::Natural(Natural::Succ),_) = *s.whnf(arena) => {
                let nat = Term::axiom(super::Axiom::Natural(Natural::Nat),&[],arena);
                let cast = Term::axiom(super::Axiom::Equality(Self::Cast), &[Level::succ(Level::zero(arena),arena)],arena)
                    .app(nat, arena)
                    .app(nat, arena)
                    .app(e, arena)
                    .app(k,arena);
                Some(Term::axiom(super::Axiom::Natural(Natural::Succ),&[],arena).app(cast,arena))
            },
            _ => None
        }
    }

    /// Reduction function for `Cast` operations
    fn reduce_cast<'arena>(term: Term<'arena>, arena: &mut Arena<'arena>) -> Option<Term<'arena>> {
        use crate::memory::term::Payload::{App, Axiom, Sort};
        // Be aware that this function will not be automatically formatted, because of the
        // experimental status of let-chains, as well as that of if-let conditions in pattern matching.
        let App(f, x) = *term.whnf(arena) else { return None; };
        let App(f, e) = *f.whnf(arena)  else { return None; };
        let App(f,ty2) = *f.whnf(arena)  else { return None; };
        let App(f,ty1) = *f.whnf(arena)  else { return None; };
        let Axiom(super::Axiom::Equality(Self::Cast), _) = *f.unfold(arena).whnf(arena)  else { return None; };
        match *ty2.whnf(arena) {
                // Cast-Zero/Cast-Succ
                Axiom(super::Axiom::Natural(Natural::Nat),_) if let Axiom(super::Axiom::Natural(Natural::Nat),_) = *ty1.whnf(arena)
                    => Self::reduce_nat_cast(x, e, arena),
                // Cast-Univ
                Sort(_) if ty1.whnf(arena) == ty2.whnf(arena) => Some(x),
                // TODO Cast-Pi
                _ => None
            }
    }
}
