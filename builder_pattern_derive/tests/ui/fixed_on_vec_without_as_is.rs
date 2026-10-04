use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(fixed = vec![1, 2])]
    values: Vec<u8>,
}

fn main() {}
