use kernel::term::arena::{use_arena, Arena};
use kernel::term::builders::*;

fn use_and_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    use_arena(|arena| {
        let false_ = arena.build(prod("P", prop(), var("P"))).unwrap();

        arena.bind("False", false_);

        let true_ = arena.build(prod("_", var("False"), var("False"))).unwrap();

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
                        prod(
                            "_",
                            prod("_", var("A"), prod("_", var("B"), var("C"))),
                            var("C"),
                        ),
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
        let goal = arena
            .build(app(app(var("and"), var("True")), var("True")))
            .unwrap();

        let hypothesis = arena.build(abs("x", var("False"), var("x"))).unwrap();
        let true_ = arena.build(var("True")).unwrap();

        assert!(arena.check(hypothesis, true_).is_ok());
        arena.bind("hyp", hypothesis);

        let proof = arena
            .build(abs(
                "a",
                prop(),
                abs(
                    "b",
                    prod("_", var("True"), prod("_", var("True"), var("a"))),
                    app(app(var("b"), var("hyp")), var("hyp")),
                ),
            ))
            .unwrap();

        assert!(arena.check(proof, goal).is_ok());
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
                        var("A"),
                        prod("_", var("B"), app(app(var("and"), var("A")), var("B"))),
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
                        var("A"),
                        abs(
                            "b",
                            // b : B
                            var("B"),
                            abs(
                                "C",
                                // C : prop()
                                prop(),
                                abs(
                                    "p",
                                    // p : A -> B -> C
                                    prod("_", var("A"), prod("_", var("B"), var("C"))),
                                    // p a b
                                    app(app(var("p"), var("a")), var("b")),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(arena.check(proof, goal).is_ok());
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
                    prod("_", app(app(var("and"), var("A")), var("B")), var("A")),
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
                        app(app(var("and"), var("A")), var("B")),
                        app(
                            app(var("p"), var("A")),
                            abs(
                                "a",
                                // a : A
                                var("A"),
                                abs(
                                    "b",
                                    // b : B
                                    var("B"),
                                    var("a"),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(arena.check(proof, goal).is_ok());
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
                        app(app(var("and"), var("A")), var("B")),
                        var("B"),
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
                        app(app(var("and"), var("A")), var("B")),
                        app(
                            app(var("p"), var("B")),
                            abs(
                                "a",
                                // a : A
                                var("A"),
                                abs(
                                    "b",
                                    // b : B
                                    var("B"),
                                    var("b"),
                                ),
                            ),
                        ),
                    ),
                ),
            ))
            .unwrap();

        assert!(arena.check(proof, goal).is_ok());
    })
}
