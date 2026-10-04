use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(fixed = "1")]
    value: u8,
}

fn main() {
    let _record = Record::builder().value(2).build();
}
