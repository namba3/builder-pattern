use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(each = "r#type")]
    values: Vec<u8>,
    r#type: u8,
}

fn main() {}
