#![feature(box_syntax)]

use kernel::Command::*;

use kernel::term::extern_build::*;
use kernel::term::use_arena;
use kernel::term::Arena;

fn use_and_arena<F, T>(f: F) -> T
where
    F: for<'arena> FnOnce(&mut Arena<'arena>) -> T,
{
    use_arena(|arena| {
        let false_ = arena
            .build_from_extern(prod("P", prop(), var("P")))
            .unwrap();
        assert!(Define("False", None, false_)
            .process(arena)
            .unwrap()
            .is_none());

        let true_ = arena
            .build_from_extern(prod("_", var("False"), var("False")))
            .unwrap();
        assert!(Define("True", None, true_)
            .process(arena)
            .unwrap()
            .is_none());

        let and = arena
            .build_from_extern(abs(
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
        assert!(Define("and", None, and).process(arena).unwrap().is_none());

        f(arena)
    })
}

#[test]
fn and_true_true() {
    use_and_arena(|arena| {
        let goal = arena
            .build_from_extern(app(app(var("and"), var("True")), var("True")))
            .unwrap();

        let hypothesis = arena
            .build_from_extern(abs("x", var("False"), var("x")))
            .unwrap();
        let true_ = arena.build_from_extern(var("True")).unwrap();

        assert!(Define("hyp", Some(true_), hypothesis)
            .process(arena)
            .is_ok());

        let proof = arena
            .build_from_extern(abs(
                "a",
                prop(),
                abs(
                    "b",
                    prod("_", var("True"), prod("_", var("True"), var("a"))),
                    app(app(var("b"), var("hyp")), var("hyp")),
                ),
            ))
            .unwrap();

        assert!(CheckType(proof, goal).process(arena).is_ok());
    })
}

#[test]
fn and_intro() {
    use_and_arena(|arena| {
        let goal = arena
            .build_from_extern(prod(
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
            .build_from_extern(abs(
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

        assert!(CheckType(proof, goal).process(arena).is_ok());
    })
}

#[test]
fn and_elim_1() {
    use_and_arena(|arena| {
        let goal = arena
            .build_from_extern(prod(
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
            .build_from_extern(abs(
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

        assert!(CheckType(proof, goal).process(arena).is_ok());
    })
}

#[test]
fn and_elim_2() {
    use_and_arena(|arena| {
        let goal = arena
            .build_from_extern(prod(
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
            .build_from_extern(abs(
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

        assert!(CheckType(proof, goal).process(arena).is_ok());
    })
}
