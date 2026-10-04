use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder()]
    value: u8,
}

fn main() {}
