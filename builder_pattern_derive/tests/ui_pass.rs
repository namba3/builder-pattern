#[test]
fn compile_pass_cases() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/pass/*.rs");
}
