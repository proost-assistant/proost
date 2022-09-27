#[test]
fn print() {
    use std::fs;
    let contents: &str =
        &*fs::read_to_string("test.mdln").expect("Should have been able to read the file");

    print!(" \n{:?}", parse_file(contents));
    assert_eq!(1, 1);
}
