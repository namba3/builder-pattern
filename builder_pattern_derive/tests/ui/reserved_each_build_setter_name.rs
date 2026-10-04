use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(each = "build")]
    values: Vec<u8>,
}

fn main() {}
