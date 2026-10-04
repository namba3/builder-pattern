use builder_pattern_derive::Builder;

type Items<T> = Vec<T>;

#[derive(Builder)]
struct Record {
    values: Items<u8>,
}

fn main() {
    let _ = Record::builder()
        .values(vec![1])
        .values(vec![2])
        .build();
}
