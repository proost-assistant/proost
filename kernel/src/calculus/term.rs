//! A set of lambda-calculus quasi-primitives.
//!
//! This module consists of internal utility functions used by the type checker, and correspond to
//! usual functions over lambda-terms. These functions interact appropriately with a given arena.

use crate::memory::arena::Arena;
use crate::memory::declaration::InstantiatedDeclaration;
use crate::memory::level::Level;
use crate::memory::term::Payload::{Abs, App, Axiom, Decl, Prod, Sort, Var};
use crate::memory::term::Term;

impl<'arena> Term<'arena> {
    /// Unfolds a term.
    ///
    /// Unfolding only happens on instantiated declarations.
    pub(crate) fn unfold(self, arena: &mut Arena<'arena>) -> Self {
        match *self {
            Decl(decl) => decl.get_term(arena),
            _ => self,
        }
    }

    /// Apply one step of β-reduction, using the leftmost-outermost evaluation strategy.
    #[inline]
    #[must_use]
    pub fn beta_reduction(self, arena: &mut Arena<'arena>) -> Self {
        if let Some(red) = crate::axiom::Axiom::reduce_recursor(self, arena) {
            return red;
        };

        match *self {
            App(t1, t2) => {
                if let Abs(_, t1) = *t1.unfold(arena) {
                    t1.substitute(t2, 1, arena)
                } else {
                    let t1_new = t1.beta_reduction(arena);
                    if t1_new == t1 {
                        let t2_new = t2.beta_reduction(arena);
                        t1.app(t2_new, arena)
                    } else {
                        t1_new.app(t2, arena)
                    }
                }
            },
            Abs(arg_type, body) => {
                let body = body.beta_reduction(arena);
                arg_type.abs(body, arena)
            },
            Prod(arg_type, body) => {
                let body = body.beta_reduction(arena);
                arg_type.prod(body, arena)
            },
            Decl(decl) => decl.get_term(arena),
            _ => self,
        }
    }

    /// Returns the term `self` where all variables with de Bruijn index larger than `depth` are offset
    /// by `offset`.
    pub(crate) fn shift(self, offset: usize, depth: usize, arena: &mut Arena<'arena>) -> Self {
        if self.is_certainly_closed() {
            return self;
        }
        let shifted_term = match *self {
            Var(i, type_) if i > depth.into() => Term::var(i + offset.into(), type_, arena),
            App(t1, t2) => {
                let t1 = t1.shift(offset, depth, arena);
                let t2 = t2.shift(offset, depth, arena);
                t1.app(t2, arena)
            },
            Abs(arg_type, body) => {
                let arg_type = arg_type.shift(offset, depth, arena);
                let body = body.shift(offset, depth + 1, arena);
                arg_type.abs(body, arena)
            },
            Prod(arg_type, body) => {
                let arg_type = arg_type.shift(offset, depth, arena);
                let body = body.shift(offset, depth + 1, arena);
                arg_type.prod(body, arena)
            },
            _ => self,
        };
        if (shifted_term == self) && (depth == 0) && (offset > 0) {
            self.set_as_closed();
        };
        shifted_term
    }

    /// Returns the term `self` where all instances of the variable tracked by `depth` are substituted
    /// with `sub`.
    pub(crate) fn substitute(self, sub: Self, depth: usize, arena: &mut Arena<'arena>) -> Self {
        arena.get_subst_or_init(&(self, sub, depth), |arena| match *self {
            Var(i, _) if i == depth.into() => sub.shift(depth - 1, 0, arena),
            Var(i, type_) if i > depth.into() => Term::var(i - 1.into(), type_, arena),
            App(l, r) => {
                let l = l.substitute(sub, depth, arena);
                let r = r.substitute(sub, depth, arena);
                l.app(r, arena)
            },
            Abs(arg_type, body) => {
                let arg_type = arg_type.substitute(sub, depth, arena);
                let body = body.substitute(sub, depth + 1, arena);
                arg_type.abs(body, arena)
            },
            Prod(arg_type, body) => {
                let arg_type = arg_type.substitute(sub, depth, arena);
                let body = body.substitute(sub, depth + 1, arena);
                arg_type.prod(body, arena)
            },
            _ => self,
        })
    }

    /// Substitutes all level variables in `self` according to the correspondence given by
    /// `univs`.
    ///
    /// This function would be safe to use from outside the kernel, but would serve no purpose as
    /// level variables there can only appear behind a Declaration, which prevents the access to
    /// the underlying Term.
    pub(crate) fn substitute_univs(self, univs: &[Level<'arena>], arena: &mut Arena<'arena>) -> Self {
        match *self {
            Axiom(ax, lvl) => {
                let lvl = lvl.iter().map(|l| l.substitute(univs, arena)).collect::<Vec<_>>();
                let lvl = arena.store_level_slice(&lvl);
                Term::axiom(ax, lvl, arena)
            },

            Var(i, ty) => {
                let ty = ty.substitute_univs(univs, arena);
                Term::var(i, ty, arena)
            },

            Sort(level) => {
                let subst = level.substitute(univs, arena);
                Term::sort(subst, arena)
            },
            App(u1, u2) => {
                let u1 = u1.substitute_univs(univs, arena);
                let u2 = u2.substitute_univs(univs, arena);
                u1.app(u2, arena)
            },
            Abs(u1, u2) => {
                let u1 = u1.substitute_univs(univs, arena);
                let u2 = u2.substitute_univs(univs, arena);
                u1.abs(u2, arena)
            },

            Prod(u1, u2) => {
                let u1 = u1.substitute_univs(univs, arena);
                let u2 = u2.substitute_univs(univs, arena);
                u1.prod(u2, arena)
            },

            Decl(decl) => {
                // TODO (#14) this can be slightly optimised in space. Certainly the substitution mapping can be
                // performed in place while allocating the slice in the arena with store_level_slice. This
                // function thus has to be made with templates.
                let params = &*decl.params.iter().map(|level| level.substitute(univs, arena)).collect::<Vec<Level>>();
                let params = arena.store_level_slice(params);
                let inst = InstantiatedDeclaration::instantiate(decl.decl, params, arena);
                Term::decl(inst, arena)
            },
        }
    }

    /// Returns the normal form of a term.
    ///
    /// This function is computationally expensive and should only be used for reduce/eval commands, not when type-checking.
    #[inline]
    #[must_use]
    pub fn normal_form(self, arena: &mut Arena<'arena>) -> Self {
        let mut temp = self;
        let mut res = self.beta_reduction(arena);

        while res != temp {
            temp = res;
            res = res.beta_reduction(arena);
        }
        res
    }

    /// Returns the weak-head normal form of a term.
    #[inline]
    #[must_use]
    pub fn whnf(self, arena: &mut Arena<'arena>) -> Self {
        self.get_whnf_or_init(|| {
            crate::axiom::Axiom::reduce_recursor(self, arena)
                .map(|x| x.whnf(arena))
                .unwrap_or_else(|| match *self {
                    App(t1, t2) => match *t1.unfold(arena).whnf(arena) {
                        Abs(_, t1) => {
                            let subst = t1.substitute(t2, 1, arena);
                            subst.whnf(arena)
                        },
                        _ => self,
                    },
                    _ => self,
                })
        })
    }

    /// Tests whether a term is computationally relevant.
    #[inline]
    pub(crate) fn is_relevant(self, arena: &mut Arena<'arena>) -> bool {
        self.get_relevance_or_try_init(|| match *self {
            Var(_, ty) => ty.is_def_eq(Term::sort_usize(0, arena), arena).map_or(true, |_| false),
            App(t, _) => t.is_relevant(arena),
            Abs(_, t) => t.is_relevant(arena),
            Decl(d) => d.get_term(arena).is_relevant(arena),
            Axiom(ax, lvl) => ax
                .get_type(arena)
                .substitute_univs(lvl, arena)
                .is_def_eq(Term::sort_usize(0, arena), arena)
                .map_or(true, |_| false),
            _ => true,
        })
    }
}

#[cfg(test)]
mod tests {
    // /!\ most terms used in these tests are ill-typed; they should not be used elsewhere
    use crate::memory::arena::use_arena;
    use crate::memory::declaration::{Declaration, InstantiatedDeclaration};
    use crate::memory::term::builder::raw::*;
    use crate::memory::term::Term;

    #[test]
    fn simple_subst() {
        use_arena(|arena| {
            // λx.(λy.x y) x
            let term = arena.build_term_raw(abs(
                prop(),
                app(abs(prop(), app(var(2.into(), prop()), var(1.into(), prop()))), var(1.into(), prop())),
            ));
            // λx.x x
            let reduced = arena.build_term_raw(abs(prop(), app(var(1.into(), prop()), var(1.into(), prop()))));

            assert_eq!(term.beta_reduction(arena), reduced);
        });
    }

    #[test]
    fn complex_subst() {
        use_arena(|arena| {
            // (λa.λb.λc.a (λd.λe.e (d b)) (λ_.c) (λd.d)) (λa.λb.a b)
            let term = arena.build_term_raw(app(
                abs(
                    prop(),
                    abs(
                        prop(),
                        abs(
                            prop(),
                            app(
                                app(
                                    app(
                                        var(3.into(), prop()),
                                        abs(
                                            prop(),
                                            abs(
                                                prop(),
                                                app(var(1.into(), prop()), app(var(2.into(), prop()), var(4.into(), prop()))),
                                            ),
                                        ),
                                    ),
                                    abs(prop(), var(2.into(), prop())),
                                ),
                                abs(prop(), var(1.into(), prop())),
                            ),
                        ),
                    ),
                ),
                abs(prop(), abs(prop(), app(var(2.into(), prop()), var(1.into(), prop())))),
            ));

            let term_step_1 = arena.build_term_raw(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            app(
                                abs(prop(), abs(prop(), app(var(2.into(), prop()), var(1.into(), prop())))),
                                abs(
                                    prop(),
                                    abs(prop(), app(var(1.into(), prop()), app(var(2.into(), prop()), var(4.into(), prop())))),
                                ),
                            ),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_2 = arena.build_term_raw(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            abs(
                                prop(),
                                app(
                                    abs(
                                        prop(),
                                        abs(prop(), app(var(1.into(), prop()), app(var(2.into(), prop()), var(5.into(), prop())))),
                                    ),
                                    var(1.into(), prop()),
                                ),
                            ),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_3 = arena.build_term_raw(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        app(
                            abs(prop(), abs(prop(), app(var(1.into(), prop()), app(var(2.into(), prop()), var(4.into(), prop()))))),
                            abs(prop(), var(2.into(), prop())),
                        ),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_4 = arena.build_term_raw(abs(
                prop(),
                abs(
                    prop(),
                    app(
                        abs(prop(), app(var(1.into(), prop()), app(abs(prop(), var(3.into(), prop())), var(3.into(), prop())))),
                        abs(prop(), var(1.into(), prop())),
                    ),
                ),
            ));

            let term_step_5 = arena.build_term_raw(abs(
                prop(),
                abs(
                    prop(),
                    app(abs(prop(), var(1.into(), prop())), app(abs(prop(), var(2.into(), prop())), var(2.into(), prop()))),
                ),
            ));

            let term_step_6 =
                arena.build_term_raw(abs(prop(), abs(prop(), app(abs(prop(), var(2.into(), prop())), var(2.into(), prop())))));

            // λa.λb.b
            let term_step_7 = arena.build_term_raw(abs(prop(), abs(prop(), var(1.into(), prop()))));

            assert_eq!(term.beta_reduction(arena), term_step_1);
            assert_eq!(term_step_1.beta_reduction(arena), term_step_2);
            assert_eq!(term_step_2.beta_reduction(arena), term_step_3);
            assert_eq!(term_step_3.beta_reduction(arena), term_step_4);
            assert_eq!(term_step_4.beta_reduction(arena), term_step_5);
            assert_eq!(term_step_5.beta_reduction(arena), term_step_6);
            assert_eq!(term_step_6.beta_reduction(arena), term_step_7);
            assert_eq!(term_step_7.beta_reduction(arena), term_step_7);
        });
    }

    #[test]
    fn decl_subst() {
        use_arena(|arena| {
            let prop = arena.build_term_raw(prop());
            let decl = Declaration::new(prop, 0);
            let decl = Term::decl(InstantiatedDeclaration::instantiate(decl, &[], arena), arena);

            assert_eq!(decl.beta_reduction(arena), prop);
        });
    }

    #[test]
    fn decl_app_whnf() {
        use_arena(|arena| {
            let false_ = arena.build_term_raw(prod(prop(), var(0.into(), prop())));

            let decl = Declaration::new(arena.build_term_raw(abs(prop(), prop())), 0);
            let decl = Term::decl(InstantiatedDeclaration::instantiate(decl, &[], arena), arena);

            let app = Term::app(decl, false_, arena);

            let reduced = arena.build_term_raw(prop());

            assert_eq!(app.beta_reduction(arena), reduced);
            assert_eq!(app.whnf(arena), reduced);
        });
    }

    #[test]
    fn shift_prod() {
        use_arena(|arena| {
            let reduced = prod(prop(), var(1.into(), prop()));
            let term = arena.build_term_raw(app(abs(prop(), reduced), prop()));

            let reduced = arena.build_term_raw(prod(prop(), var(1.into(), prop())));
            assert_eq!(term.beta_reduction(arena), reduced);
        });
    }

    #[test]
    fn prod_beta_red() {
        use_arena(|arena| {
            let term = arena.build_term_raw(prod(prop(), app(abs(prop(), var(1.into(), prop())), var(1.into(), prop()))));
            let reduced = arena.build_term_raw(prod(prop(), var(1.into(), prop())));

            assert_eq!(term.beta_reduction(arena), reduced);
        });
    }

    #[test]
    fn app_red_rhs() {
        use_arena(|arena| {
            let term = arena.build_term_raw(abs(
                prop(),
                app(var(1.into(), prop()), app(abs(prop(), var(1.into(), prop())), var(1.into(), prop()))),
            ));
            let reduced = arena.build_term_raw(abs(prop(), app(var(1.into(), prop()), var(1.into(), prop()))));

            assert_eq!(term.beta_reduction(arena), reduced);
        });
    }

    #[test]
    fn normal_form() {
        use_arena(|arena| {
            let term = arena.build_term_raw(app(
                app(app(abs(prop(), abs(prop(), abs(prop(), var(1.into(), prop())))), prop()), prop()),
                prop(),
            ));
            let normal_form = arena.build_term_raw(prop());

            assert_eq!(term.normal_form(arena), normal_form);
        });
    }

    #[test]
    fn subst_univs() {
        use crate::axiom::natural::Natural::Nat;
        use crate::axiom::Axiom;
        use crate::memory::level::builder::raw::*;

        use_arena(|arena| {
            let zero_ = arena.build_level_raw(zero());
            let decl = Declaration::new(arena.build_term_raw(prop()), 2);
            let decl = InstantiatedDeclaration::instantiate(decl, &[zero_, zero_], arena);

            let prop = crate::memory::term::Term::decl(decl, arena);

            assert_eq!(prop.substitute_univs(&[zero_, zero_], arena), prop);

            let vart = crate::memory::term::builder::raw::var;

            let lvl = max(succ(zero()), succ(zero()));
            let term = arena.build_term_raw(abs(
                sort_(lvl),
                abs(
                    type_usize(0),
                    abs(
                        type_usize(1),
                        prod(vart(1.into(), type_usize(1)), app(vart(1.into(), type_usize(1)), vart(2.into(), type_usize(0)))),
                    ),
                ),
            ));

            assert_eq!(term.substitute_univs(&[zero_, zero_], arena), term);

            let nat = Term::axiom(Axiom::Natural(Nat), &[], arena);
            assert_eq!(nat.substitute_univs(&[zero_, zero_], arena), nat);
        });
    }

    #[test]
    fn reduce_nat() {
        use crate::axiom::natural::Natural::{Nat, NatRec, Succ, Zero};
        use crate::axiom::Axiom;
        use crate::memory::level::Level;

        use_arena(|arena| {
            let lvl_one = Level::succ(Level::zero(arena), arena);
            let nat = Term::axiom(Axiom::Natural(Nat), &[], arena);
            let zero = Term::axiom(Axiom::Natural(Zero), &[], arena);
            let one = Term::app(Term::axiom(Axiom::Natural(Succ), &[], arena), zero, arena);
            let to_zero = Term::app(
                Term::app(
                    Term::app(
                        Term::axiom(Axiom::Natural(NatRec), arena.store_level_slice(&[lvl_one]), arena),
                        Term::abs(nat, nat, arena),
                        arena,
                    ),
                    zero,
                    arena,
                ),
                Term::abs(nat, Term::abs(nat, zero, arena), arena),
                arena,
            );
            let zero_to_zero = Term::app(to_zero, zero, arena);
            let one_to_zero = Term::app(to_zero, one, arena);
            let nat_to_zero = Term::app(to_zero, nat, arena);
            assert_eq!(zero_to_zero.normal_form(arena), zero);
            assert_eq!(one_to_zero.normal_form(arena), zero);
            assert_eq!(nat_to_zero.whnf(arena), nat_to_zero);
        });
    }

    #[test]
    fn relevance() {
        use crate::axiom::false_::False::False;
        use crate::axiom::Axiom;

        use_arena(|arena| {
            let false_ = Term::axiom(Axiom::False(False), &[], arena);
            let tt1 = false_.abs(Term::var(1.into(), false_, arena), arena);
            assert!(!tt1.is_relevant(arena));
        });
    }
}
