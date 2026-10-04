use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(name = "first")]
    #[builder(name = "second")]
    value: u8,
}

fn main() {}
