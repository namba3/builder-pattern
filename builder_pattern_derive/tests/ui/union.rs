use builder_pattern_derive::Builder;

#[derive(Builder)]
union Value {
    integer: u32,
    float: f32,
}

fn main() {}
