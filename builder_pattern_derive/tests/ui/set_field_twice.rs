use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    id: u64,
}

fn main() {
    let _ = Record::builder().id(1).id(2).build();
}
