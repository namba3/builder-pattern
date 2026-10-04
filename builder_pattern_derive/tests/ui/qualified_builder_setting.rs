use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(other::name = "set_value")]
    value: u8,
}

fn main() {}
