use builder_pattern_derive::Builder;

#[derive(Builder)]
#[builder(name = "create")]
struct Record {
    value: u8,
}

fn main() {}
