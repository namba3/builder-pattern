#![forbid(missing_docs)]

//! Check that generated public builder APIs are documented.

use builder_pattern_derive::Builder;

/// A documented input type.
#[derive(Builder)]
pub struct Record {
    /// The value stored in the record.
    pub value: u8,
    /// Whether the record is enabled.
    pub enabled: bool,
    /// An optional record number.
    pub optional: Option<u8>,
    /// The items stored in the record.
    #[builder(each = "item")]
    pub items: Vec<u8>,
    /// The value with a default.
    #[builder(default = 3)]
    pub defaulted: u8,
    /// The fixed value.
    #[builder(fixed = 4)]
    pub fixed: u8,
}

fn main() {
    let record = Record::builder()
        .value(7)
        .enabled()
        .optional(2)
        .item(1)
        .items([2, 3])
        .defaulted(5)
        .build();

    assert_eq!(record.fixed, 4);
}
