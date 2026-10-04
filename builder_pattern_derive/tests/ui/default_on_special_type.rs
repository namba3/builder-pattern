use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Flags {
    #[builder(default = true)]
    enabled: bool,
}

fn main() {}
