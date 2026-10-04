use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(name = "set_value")]
    first: u8,
    #[builder(name = "set_value")]
    second: u8,
}

fn main() {}
