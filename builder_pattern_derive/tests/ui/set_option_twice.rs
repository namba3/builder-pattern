use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Options {
    value: Option<u8>,
}

fn main() {
    let _ = Options::builder().value(1).value(2).build();
}
