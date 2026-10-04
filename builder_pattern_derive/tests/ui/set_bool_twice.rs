use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Flags {
    enabled: bool,
}

fn main() {
    let _ = Flags::builder().enabled().enabled().build();
}
