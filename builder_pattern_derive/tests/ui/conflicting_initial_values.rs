use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(default = 1, fixed = 2)]
    value: u8,
}

fn main() {}
