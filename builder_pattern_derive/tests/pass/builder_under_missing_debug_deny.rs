#![deny(missing_copy_implementations, missing_debug_implementations)]

use builder_pattern_derive::Builder;

#[derive(Clone, Copy, Debug, Builder)]
pub struct Record {
    pub value: u8,
}

fn main() {
    let _record = Record::builder().value(7).build();
}
