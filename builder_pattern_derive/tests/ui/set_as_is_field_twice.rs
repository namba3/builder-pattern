use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(as_is)]
    value: Option<u8>,
}

fn main() {
    let _record = Record::builder()
        .value(Some(1))
        .value(Some(2))
        .build();
}
