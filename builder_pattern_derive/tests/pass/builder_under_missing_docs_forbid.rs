#![forbid(missing_docs)]

//! Check that generated public builder APIs are documented.

use builder_pattern_derive::Builder;

/// A documented input type.
#[derive(Builder)]
pub struct Record {
    /// The value stored in the record.
    pub value: u8,
}

fn main() {
    let _record = Record::builder().value(7).build();
}
