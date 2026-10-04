#![deny(non_snake_case)]

use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    #[builder(name = "setValue")]
    value: u8,
    #[builder(each = "addItem")]
    items: Vec<u8>,
}

fn main() {
    let _record = Record::builder()
        .setValue(7)
        .addItem(1)
        .items([2, 3])
        .build();
}
