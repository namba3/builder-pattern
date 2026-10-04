use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(as_is, each = "item")]
    values: Vec<u8>,
}

fn main() {}
