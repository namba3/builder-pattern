use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(default = "1")]
    value: u8,
}

fn main() {
    let _record = Record::builder().value(2).value(3).build();
}
