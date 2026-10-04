use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Pair<T> {
    first: T,
    second: T,
}

fn main() {
    let _ = Pair::<u8>::builder().first(1).build();
}
