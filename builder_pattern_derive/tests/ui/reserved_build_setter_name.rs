use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(name = "build")]
    value: u8,
}

fn main() {}
