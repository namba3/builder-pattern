#[test]
fn compile_fail_cases() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/*.rs");
}

#[test]
fn compile_pass_cases() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/pass/*.rs");
}
