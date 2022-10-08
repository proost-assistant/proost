use crate::term::*;
use std::fmt::{Display, Formatter};
use Term::*;

type Env = Vec<Val>;

#[derive(Clone, Debug)]
pub enum Val {
    VProp,
    VType(usize),
    VVar(Level),
    VApp(Box<Val>, Box<Val>),
    VAbs(Box<Val>, Closure),
    VProd(Box<Val>, Closure),
}

#[derive(Clone, Debug)]
pub struct Closure {
    env: Env,
    term: Term,
}

impl Closure {
    pub fn shift(self, v: Val) -> Val {
        let e = &mut self.env.clone();
        e.push(v);
        eval(e, self.term)
    }
}

use Val::*;

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            VProp => write!(f, "\u{02119}"),
            VVar(i) => write!(f, "{}", i),
            VType(i) => write!(f, "\u{1D54B}({})", i),
            VApp(t1, t2) => write!(f, "({} {})", t1, t2), //TODO figure out how to display Vec<Val> so that I can print the whole closure
            VAbs(t1, t2) => write!(f, "\u{003BB} {} \u{02192} {}", t1, t2.term),
            VProd(t1, t2) => write!(f, "\u{02200} {} \u{02192} {}", t1, t2.term),
        }
    }
}

fn eval(e: &Env, t: Term) -> Val {
    match t {
        Prop => VProp,
        Type(i) => VType(i),
        Var(i) => {
            print!("{:?}", *e);
            (*e)[i].clone()
        }
        App(box t1, box t2) => match eval(e, t1) {
            VAbs(_, t) => t.shift(eval(e, t2)),
            v1 => VApp(box v1, box eval(e, t2)),
        },
        Abs(box a, box b) => VAbs(
            box eval(e, a),
            Closure {
                env: (*e).clone(),
                term: b,
            },
        ),
        Prod(box a, box b) => VProd(
            box eval(e, a),
            Closure {
                env: (*e).clone(),
                term: b,
            },
        ),
    }
}

fn lvl2ix(l1: Level, l2: Level) -> Index {
    l1 - l2 - 1
}

fn quote(l: Level, v: Val) -> Term {
    match v {
        VProp => Prop,
        VType(i) => Type(i),
        VVar(i) => Var(lvl2ix(l, i)),
        VApp(box t, box u) => App(box quote(l, t), box quote(l, u)),
        //prob wrong for 2nd arg, TODO give it a thought
        VAbs(box t, u) => Abs(box quote(l, t), box quote(l + 1, u.shift(VVar(l)))),
        VProd(box t, u) => Prod(box quote(l, t), box quote(l + 1, u.shift(VVar(l)))),
    }
}

//returns normal form of term t in env e
fn nf(e: Env, t: Term) -> Term {
    quote(e.len(), eval(&e, t))
}

// /!\ IMPORTANT /!\
// Conversion function, checks whether two values are equal.
// The conversion is untyped, meaning that it should **Only**
// be called during type-checking when the two vals are already
// known to be of the same type.
fn conv(l: Level, v1: Val, v2: Val) -> bool {
    match (v1, v2) {
        (VType(i), VType(j)) => i == j,

        (VProp, VProp) => true,

        (VVar(i), VVar(j)) => i == j,

        (VProd(box a1, b1), VProd(box a2, b2)) => {
            conv(l, a1, a2) && conv(l + 1, b1.shift(VVar(l)), b2.shift(VVar(l)))
        }

        //Since we assume that both vals already have the same type,
        //checking conversion over the argument type is useless.
        //However, this doesn't mean we can simply remove the arg type
        //from the type constructor in the enum, it is needed to quote back to terms.
        (VAbs(_, t), VAbs(_, u)) => conv(l + 1, t.shift(VVar(l)), u.shift(VVar(l))),

        (VAbs(_, t), u) | (u, VAbs(_, t)) => {
            conv(l + 1, t.shift(VVar(l)), VApp(box u, box VVar(l)))
        }

        (VApp(box t1, box u1), VApp(box t2, box u2)) => conv(l, t1, t2) && conv(l, u1, u2),

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    // TODO: Correctly types lambda terms.
    use crate::type_checker::*;

    fn assert_def_eq(t1: Term, t2: Term) {
        assert_eq!(conv(0, eval(&Vec::new(), t1), eval(&Vec::new(), t2)), true)
    }

    #[test]
    fn simple() {
        let t1 = App(box Abs(box Type(0), box Var(0)), box Prop);
        let t2 = Prop;
        assert_eq!(conv(0, eval(&Vec::new(), t1), eval(&Vec::new(), t2)), true)
    }

    #[test]
    fn simple_subst() {
        env::set_var("RUST_BACKTRACE", "1");
        // λx.(λy.x y) x
        let term = Abs(
            box Prop,
            box App(
                box Abs(box Prop, box App(box Var(1), box Var(0))),
                box Var(0),
            ),
        );

        // λx.x x
        let reduced = Abs(box Prop, box App(box Var(0), box Var(0)));

        assert_def_eq(term, reduced);
    }

    //#[test]
    fn complex_subst() {
        // (λa.λb.λc.(a (λd.λe.e (d b) (λ_.c)) (λd.d)) (λa.λb.a b)
        let term = App(
            box Abs(
                box Prop,
                box Abs(
                    box Prop,
                    box Abs(
                        box Prop,
                        box App(
                            box App(
                                box App(
                                    box Var(2),
                                    box Abs(
                                        box Prop,
                                        box Abs(
                                            box Prop,
                                            box App(box Var(0), box App(box Var(1), box Var(3))),
                                        ),
                                    ),
                                ),
                                box Abs(box Prop, box Var(1)),
                            ),
                            box Abs(box Prop, box Var(0)),
                        ),
                    ),
                ),
            ),
            box Abs(box Prop, box Abs(box Prop, box App(box Var(1), box Var(0)))),
        );

        // λa.λb.b
        let term_step_7 = Abs(box Prop, box Abs(box Prop, box Var(0)));

        assert_def_eq(term, term_step_7);
    }
}
