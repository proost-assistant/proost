use kernel::memory::arena::{use_arena, Arena};
use kernel::memory::term::builder::*;

fn use_and_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    use_arena(|arena| {
        let false_ = arena.build(prod("P", prop(), var("P".into()))).unwrap();

        arena.bind("False", false_);

        let true_ = arena.build(prod("_", var("False".into()), var("False".into()))).unwrap();

        arena.bind("True", true_);

        let and = arena
            .build(abs(
                "A",
                prop(),
                abs(
                    "B",
                    prop(),
                    prod(
                        "C",
                        prop(),
                        prod("_", prod("_", var("A".into()), prod("_", var("B".into()), var("C".into()))), var("C".into())),
                    ),
                ),
            ))
            .unwrap();

        arena.bind("and", and);

        f(arena)
    })
}

#[test]
fn and_true_true() {
    use_and_arena(|arena| {
        let goal = arena.build(app(app(var("and".into()), var("True".into())), var("True".into()))).unwrap();

        let hypothesis = arena.build(abs("x", var("False".into()), var("x".into()))).unwrap();
        let true_ = arena.build(var("True".into())).unwrap();

        assert!(hypothesis.check(true_, arena).is_ok());
        arena.bind("hyp", hypothesis);

        let proof = arena
            .build(abs(
                "a",
                prop(),
                abs(
                    "b",
                    prod("_", var("True".into()), prod("_", var("True".into()), var("a".into()))),
                    app(app(var("b".into()), var("hyp".into())), var("hyp".into())),
                ),
            ))
            .unwrap();

        assert!(proof.check(goal, arena).is_ok());
    })
}

#[test]
fn and_intro() {
    use_and_arena(|arena| {
        let goal = arena
            .build(prod(
                "A", // A : prop()
                prop(),
                prod(
                    "B", // B : prop()
                    prop(),
                    prod(
                        "_",
                        var("A".into()),
                        prod("_", var("B".into()), app(app(var("and".into()), var("A".into())), var("B".into()))),
                    ),
                ),
            ))
            .unwrap();

        let proof = arena
            .build(abs(
                "A",
                // A : prop()
                prop(),
                abs(
                    "B",
                    // B : prop()
                    prop(),
                    abs(
                        "a",
                        // a : A
                        var("A".into()),
                        abs(
                            "b",
                            // b : B
                            var("B".into()),
                            abs(
                                "C",
                                // C : prop()
                                prop(),
                                abs(
                                    "p",
                                    // p : A -> B -> C
                                    prod("_", var("A".into()), prod("_", var("B".into()), var("C".into()))),
                                    // p a b
                                    app(app(var("p".into()), var("a".into())), var("b".into())),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(proof.check(goal, arena).is_ok());
    })
}

#[test]
fn and_elim_1() {
    use_and_arena(|arena| {
        let goal = arena
            .build(prod(
                "A",
                // A : prop()
                prop(),
                prod(
                    "B",
                    // B : prop()
                    prop(),
                    prod("_", app(app(var("and".into()), var("A".into())), var("B".into())), var("A".into())),
                ),
            ))
            .unwrap();

        let proof = arena
            .build(abs(
                "A",
                // A : prop()
                prop(),
                abs(
                    "B",
                    // B : prop()
                    prop(),
                    abs(
                        "p",
                        // p : and A B
                        app(app(var("and".into()), var("A".into())), var("B".into())),
                        app(
                            app(var("p".into()), var("A".into())),
                            abs(
                                "a",
                                // a : A
                                var("A".into()),
                                abs(
                                    "b",
                                    // b : B
                                    var("B".into()),
                                    var("a".into()),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(proof.check(goal, arena).is_ok());
    })
}

#[test]
fn and_elim_2() {
    use_and_arena(|arena| {
        let goal = arena
            .build(prod(
                "A",
                // A : prop()
                prop(),
                prod(
                    "B",
                    // B : prop()
                    prop(),
                    prod(
                        "p",
                        // p : and A B
                        app(app(var("and".into()), var("A".into())), var("B".into())),
                        var("B".into()),
                    ),
                ),
            ))
            .unwrap();

        let proof = arena
            .build(abs(
                "A",
                // A : prop()
                prop(),
                abs(
                    "B",
                    // B : prop()
                    prop(),
                    abs(
                        "p",
                        // p : and A B
                        app(app(var("and".into()), var("A".into())), var("B".into())),
                        app(
                            app(var("p".into()), var("B".into())),
                            abs(
                                "a",
                                // a : A
                                var("A".into()),
                                abs(
                                    "b",
                                    // b : B
                                    var("B".into()),
                                    var("b".into()),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(proof.check(goal, arena).is_ok());
    })
}
