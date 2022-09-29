#[test]
fn parse() {
    use std::fs;
    let contents: &str =
        &*fs::read_to_string("tests/example.mdln").expect("Should have been able to read the file");

    assert_eq!(
        format!("{:?}", parser::parse_file(contents)),
        "Ok([Define(\"Test\", Prop), GetType(Prop), Define(\"T2\", App(Prop, Prop))])"
    );
}
