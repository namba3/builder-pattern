use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(as_is, fixed = vec![1], each = "item")]
    values: Vec<u8>,
}

fn main() {}
