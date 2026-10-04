#![deny(missing_docs)]

//! Check that generated public builder items do not trigger documentation lints.

use builder_pattern_derive::Builder;

/// A type with a generated builder.
#[derive(Builder)]
pub struct Documented {
    /// The value stored in the type.
    pub value: u8,
}

fn main() {
    let _value = Documented::builder().value(7).build();
}
