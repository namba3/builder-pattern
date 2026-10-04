#![deny(non_camel_case_types)]

use builder_pattern_derive::Builder;

#[allow(non_camel_case_types)]
#[derive(Builder)]
struct r#type {
    value: u8,
}

fn main() {
    let _value = r#type::builder().value(7).build();
}
