#![deny(warnings)]

use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    enabled: bool,
    value: u8,
}

fn main() {
    let record = Record::builder().value(7).build();

    assert!(!record.enabled);
    assert_eq!(record.value, 7);
}
