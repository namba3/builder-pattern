use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(default = vec![1], each = "item")]
    items: Vec<u8>,
}

fn main() {}
