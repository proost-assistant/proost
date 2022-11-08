#![feature(box_syntax)]

use kernel::Command::*;
use kernel::Environment;
use kernel::Term::{self, *};
use lazy_static::lazy_static;

lazy_static! {
    static ref FALSE: Term = Prod(box Prop, box Var(1.into()));
    static ref TRUE: Term = Prod(box Const("False".into()), box Const("False".into()));
}

fn setup_env() -> Environment {
    let mut env = Environment::new();

    assert!(Define("False".into(), None, FALSE.clone())
        .process(&mut env)
        .unwrap()
        .is_none());

    assert!(Define("True".into(), None, TRUE.clone())
        .process(&mut env)
        .unwrap()
        .is_none());

    let and = Abs(
        box Prop,
        box Abs(
            box Prop,
            box Prod(
                box Prop,
                box Prod(
                    // A -> B -> C
                    box Prod(
                        box Var(3.into()),
                        box Prod(box Var(3.into()), box Var(3.into())),
                    ),
                    box Var(2.into()),
                ),
            ),
        ),
    );

    assert!(Define("and".into(), None, and)
        .process(&mut env)
        .unwrap()
        .is_none());

    env
}

#[test]
fn and_true_true() {
    let mut env = setup_env();

    let goal = App(
        box App(box Const("and".into()), box Const("True".into())),
        box Const("True".into()),
    );

    let hypothesis = Abs(box Const("False".into()).clone(), box Var(1.into()));

    assert!(Define("hyp".into(), Some(TRUE.clone()), hypothesis)
        .process(&mut env)
        .is_ok());

    let proof = Abs(
        box Prop,
        box Abs(
            box Prod(
                box Const("True".into()),
                box Prod(box Const("True".into()), box Var(3.into())),
            ),
            box App(
                box App(box Var(1.into()), box Const("hyp".into())),
                box Const("hyp".into()),
            ),
        ),
    );

    assert!(CheckType(proof, goal).process(&mut env).is_ok());
}

#[test]
fn and_intro() {
    let mut env = setup_env();

    let goal = Prod(
        // A : Prop
        box Prop,
        box Prod(
            // B : Prop
            box Prop,
            box Prod(
                // a : A
                box Var(2.into()),
                box Prod(
                    // b : B
                    box Var(2.into()),
                    box App(
                        box App(box Const("and".into()), box Var(4.into())),
                        box Var(3.into()),
                    ),
                ),
            ),
        ),
    );

    let proof = Abs(
        // A : Prop
        box Prop,
        box Abs(
            // B : Prop
            box Prop,
            box Abs(
                // a : A
                box Var(2.into()),
                box Abs(
                    // b : B
                    box Var(2.into()),
                    box Abs(
                        // C : Prop
                        box Prop,
                        box Abs(
                            // p : A -> B -> C
                            box Prod(
                                box Var(5.into()),
                                box Prod(box Var(5.into()), box Var(3.into())),
                            ),
                            // p a b
                            box App(
                                box App(box Var(1.into()), box Var(4.into())),
                                box Var(3.into()),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    );

    assert!(CheckType(proof, goal).process(&mut env).is_ok());
}

#[test]
fn and_elim_1() {
    let mut env = setup_env();

    let goal = Prod(
        // A : Prop
        box Prop,
        box Prod(
            // B : Prop
            box Prop,
            box Prod(
                box App(
                    box App(box Const("and".into()), box Var(2.into())),
                    box Var(1.into()),
                ),
                box Var(3.into()),
            ),
        ),
    );

    let proof = Abs(
        // A : Prop
        box Prop,
        box Abs(
            // B : Prop
            box Prop,
            box Abs(
                // p : and A B
                box App(
                    box App(box Const("and".into()), box Var(2.into())),
                    box Var(1.into()),
                ),
                box App(
                    box App(box Var(1.into()), box Var(3.into())),
                    box Abs(
                        // a : A
                        box Var(3.into()),
                        box Abs(
                            // b : B
                            box Var(3.into()),
                            box Var(2.into()),
                        ),
                    ),
                ),
            ),
        ),
    );

    assert!(CheckType(proof, goal).process(&mut env).is_ok());
}

#[test]
fn and_elim_2() {
    let mut env = setup_env();

    let goal = Prod(
        // A : Prop
        box Prop,
        box Prod(
            // B : Prop
            box Prop,
            box Prod(
                // p : and A B
                box App(
                    box App(box Const("and".into()), box Var(2.into())),
                    box Var(1.into()),
                ),
                box Var(2.into()),
            ),
        ),
    );

    let proof = Abs(
        // A : Prop
        box Prop,
        box Abs(
            // B : Prop
            box Prop,
            box Abs(
                // p : and A B
                box App(
                    box App(box Const("and".into()), box Var(2.into())),
                    box Var(1.into()),
                ),
                box App(
                    box App(box Var(1.into()), box Var(2.into())),
                    box Abs(
                        // a : A
                        box Var(3.into()),
                        box Abs(
                            // b : B
                            box Var(3.into()),
                            box Var(1.into()),
                        ),
                    ),
                ),
            ),
        ),
    );

    assert!(CheckType(proof, goal).process(&mut env).is_ok());
}
