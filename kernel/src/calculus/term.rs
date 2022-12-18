//! A set of lambda-calculus quasi-primitives.
//!
//! This module consists of internal utility functions used by the type checker, and correspond to
//! usual functions over lambda-terms. These functions interact appropriately with a given arena.

use Payload::*;

use crate::memory::arena::Arena;
use crate::memory::declaration::InstantiatedDeclaration;
use crate::memory::level::Level;
use crate::memory::term::{Payload, Term};

impl<'arena> Arena<'arena> {
    /// Apply one step of β-reduction, using the leftmost-outermost evaluation strategy.
    pub fn beta_reduction(&mut self, t: Term<'arena>) -> Term<'arena> {
        match *t {
            App(t1, t2) => match *t1 {
                Abs(_, t1) => self.substitute(t1, t2, 1),
                _ => {
                    let t1_new = self.beta_reduction(t1);
                    if t1_new == t1 {
                        let t2_new = self.beta_reduction(t2);
                        self.app(t1, t2_new)
                    } else {
                        self.app(t1_new, t2)
                    }
                },
            },
            Abs(arg_type, body) => {
                let body = self.beta_reduction(body);
                self.abs(arg_type, body)
            },
            Prod(arg_type, body) => {
                let body = self.beta_reduction(body);
                self.prod(arg_type, body)
            },
            _ => t,
        }
    }

    /// Returns the term `t` where all variables with de Bruijn index larger than `depth` are offset
    /// by `offset`.
    pub(crate) fn shift(&mut self, t: Term<'arena>, offset: usize, depth: usize) -> Term<'arena> {
        match *t {
            Var(i, type_) if i > depth.into() => self.var(i + offset.into(), type_),
            App(t1, t2) => {
                let t1 = self.shift(t1, offset, depth);
                let t2 = self.shift(t2, offset, depth);
                self.app(t1, t2)
            },
            Abs(arg_type, body) => {
                let arg_type = self.shift(arg_type, offset, depth);
                let body = self.shift(body, offset, depth + 1);
                self.abs(arg_type, body)
            },
            Prod(arg_type, body) => {
                let arg_type = self.shift(arg_type, offset, depth);
                let body = self.shift(body, offset, depth + 1);
                self.prod(arg_type, body)
            },
            _ => t,
        }
    }

    /// Returns the term `t` where all instances of the variable tracked by `depth` are substituted
    /// with `sub`.
    pub(crate) fn substitute(&mut self, t: Term<'arena>, sub: Term<'arena>, depth: usize) -> Term<'arena> {
        self.get_subst_or_init(&(t, sub, depth), |arena| match *t {
            Var(i, _) if i == depth.into() => arena.shift(sub, depth - 1, 0),
            Var(i, type_) if i > depth.into() => arena.var(i - 1.into(), type_),
            App(l, r) => {
                let l = arena.substitute(l, sub, depth);
                let r = arena.substitute(r, sub, depth);
                arena.app(l, r)
            },
            Abs(arg_type, body) => {
                let arg_type = arena.substitute(arg_type, sub, depth);
                let body = arena.substitute(body, sub, depth + 1);
                arena.abs(arg_type, body)
            },
            Prod(arg_type, body) => {
                let arg_type = arena.substitute(arg_type, sub, depth);
                let body = arena.substitute(body, sub, depth + 1);
                arena.prod(arg_type, body)
            },
            _ => t,
        })
    }

    /// TODO(docs)
    pub(crate) fn substitute_univs(&mut self, t: Term<'arena>, univs: &[Level<'arena>]) -> Term<'arena> {
        match *t {
            Var(i, ty) => {
                let ty = self.substitute_univs(ty, univs);
                self.var(i, ty)
            },

            Sort(level) => {
                let subst = level.substitute(univs, self);
                self.sort(subst)
            },
            App(u1, u2) => {
                let u1 = self.substitute_univs(u1, univs);
                let u2 = self.substitute_univs(u2, univs);
                self.app(u1, u2)
            },
            Abs(u1, u2) => {
                let u1 = self.substitute_univs(u1, univs);
                let u2 = self.substitute_univs(u2, univs);
                self.abs(u1, u2)
            },

            Prod(u1, u2) => {
                let u1 = self.substitute_univs(u1, univs);
                let u2 = self.substitute_univs(u2, univs);
                self.prod(u1, u2)
            },
            Decl(decl) => {
                // TODO this can be slightly optimised. Certainly the substitute mapping can be
                // performed in place while allocating the slice with store_level_slice. This
                // function thus has to be made with templates.
                let params = &*decl.params.into_iter().map(|level| level.substitute(univs, self)).collect::<Vec<Level>>();
                let params = self.store_level_slice(params);
                let inst = InstantiatedDeclaration::instantiate((*decl).decl, params, self);
                self.decl(inst)
            },
        }
    }

    /// Returns the normal form of a term.
    ///
    /// This function is computationally expensive and should only be used for reduce/eval commands, not when type-checking.
    pub fn normal_form(&mut self, t: Term<'arena>) -> Term<'arena> {
        let mut temp = t;
        let mut res = self.beta_reduction(t);

        while res != temp {
            temp = res;
            res = self.beta_reduction(res);
        }
        res
    }

    /// Returns the weak-head normal form of a term.
    pub fn whnf(&mut self, t: Term<'arena>) -> Term<'arena> {
        t.get_whnf_or_init(|| match *t {
            App(t1, t2) => match *self.whnf(t1) {
                Abs(_, t1) => {
                    let subst = self.substitute(t1, t2, 1);
                    self.whnf(subst)
                },
                _ => t,
            },
            _ => t,
        })
    }
}

#[cfg(test)]
mod tests {
    // /!\ most terms used in these tests are ill-typed; they should not be used elsewhere
    use crate::memory::arena::use_arena;
    use crate::memory::builders::raw::*;

    #[test]
    fn simple_subst() {
        use_arena(|arena| {
            // λx.(λy.x y) x
            let term = arena
                .build_term_raw(abs(prop(), app(abs(prop(), app(var(2.into(), prop()), var(1.into(), prop()))), var(1.into(), prop()))));
            // λx.x x
            let reduced = arena.build_term_raw(abs(prop(), app(var(1.into(), prop()), var(1.into(), prop()))));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
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

            assert_eq!(arena.beta_reduction(term), term_step_1);
            assert_eq!(arena.beta_reduction(term_step_1), term_step_2);
            assert_eq!(arena.beta_reduction(term_step_2), term_step_3);
            assert_eq!(arena.beta_reduction(term_step_3), term_step_4);
            assert_eq!(arena.beta_reduction(term_step_4), term_step_5);
            assert_eq!(arena.beta_reduction(term_step_5), term_step_6);
            assert_eq!(arena.beta_reduction(term_step_6), term_step_7);
            assert_eq!(arena.beta_reduction(term_step_7), term_step_7);
        })
    }

    #[test]
    fn shift_prod() {
        use_arena(|arena| {
            let reduced = arena.build_term_raw(prod(prop(), var(1.into(), prop())));
            let term = arena.build_term_raw(app(abs(prop(), reduced.into()), prop()));

            assert_eq!(arena.beta_reduction(term), reduced)
        })
    }

    #[test]
    fn prod_beta_red() {
        use_arena(|arena| {
            let term = arena.build_term_raw(prod(prop(), app(abs(prop(), var(1.into(), prop())), var(1.into(), prop()))));
            let reduced = arena.build_term_raw(prod(prop(), var(1.into(), prop())));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
    }

    #[test]
    fn app_red_rhs() {
        use_arena(|arena| {
            let term = arena
                .build_term_raw(abs(prop(), app(var(1.into(), prop()), app(abs(prop(), var(1.into(), prop())), var(1.into(), prop())))));
            let reduced = arena.build_term_raw(abs(prop(), app(var(1.into(), prop()), var(1.into(), prop()))));

            assert_eq!(arena.beta_reduction(term), reduced);
        })
    }

    #[test]
    fn normal_form() {
        use_arena(|arena| {
            let term = arena
                .build_term_raw(app(app(app(abs(prop(), abs(prop(), abs(prop(), var(1.into(), prop())))), prop()), prop()), prop()));
            let normal_form = arena.build_term_raw(prop());

            assert_eq!(arena.normal_form(term), normal_form);
        })
    }
}
