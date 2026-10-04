use builder_pattern_derive::Builder;

type Maybe<T> = Option<T>;

#[derive(Builder)]
struct Record {
    optional: Maybe<u8>,
}

fn main() {
    let _ = Record::builder().build();
}
