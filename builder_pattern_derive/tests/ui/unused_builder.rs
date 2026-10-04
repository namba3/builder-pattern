#![deny(unused_must_use)]

use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Record {
    value: u8,
}

fn main() {
    Record::builder().value(1);
}
