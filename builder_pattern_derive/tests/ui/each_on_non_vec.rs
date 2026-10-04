use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(each = "entry")]
    entry: u8,
}

fn main() {}
