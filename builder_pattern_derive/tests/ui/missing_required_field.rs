use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    id: u64,
    name: String,
}

fn main() {
    let _ = Record::builder().id(1).build();
}
