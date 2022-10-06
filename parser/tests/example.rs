use core::{Command, Term};

#[test]
fn parse() {
    use std::fs;
    let contents: &str =
        &fs::read_to_string("tests/example.mdln").expect("Should have been able to read the file");
    assert_eq!(
        parser::parse_file(contents),
        Ok(vec![Command::Define("Test".to_string(), Term::Prop)])
    );
}
