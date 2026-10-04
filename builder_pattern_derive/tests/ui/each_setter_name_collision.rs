use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(each = "value")]
    values: Vec<u8>,
    #[builder(name = "value")]
    value: u8,
}

fn main() {}
