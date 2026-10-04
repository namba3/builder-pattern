extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};

mod generator;

/// # Builder derive macro
///
/// Generates a type-state builder for a named-field struct. Required fields must be set
/// exactly once before `build()` can be called. Special field handling and all supported
/// attributes are documented in the [Japanese README](https://github.com/namba3/builder-pattern/blob/master/README.md)
/// and [English README](https://github.com/namba3/builder-pattern/blob/master/README.en.md).
///
/// ```
/// use builder_pattern_derive::Builder;
///
/// #[derive(Builder)]
/// struct Person {
///     id: u64,
///     name: String,
/// }
///
/// let person = Person::builder()
///     .id(7)
///     .name("Ada".to_owned())
///     .build();
/// assert_eq!(person.id, 7);
/// ```
///
/// Calling `build()` before all required fields are set does not compile:
///
/// ```compile_fail
/// use builder_pattern_derive::Builder;
/// #[derive(Builder)]
/// struct Person {
///     id: u64,
///     name: String,
/// }
/// fn main() {
///     let _ = Person::builder().id(7).build();
/// }
/// ```
#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder_derive(input: TokenStream) -> TokenStream {
    let support_crate = match crate_name("builder_pattern") {
        Ok(FoundCrate::Itself) => "crate".to_owned(),
        Ok(FoundCrate::Name(name)) => format!("::{}", name.replace('-', "_")),
        Err(_) => {
            return "compile_error!(\"Builder derive requires the `builder_pattern` crate as a direct dependency.\");"
                .parse()
                .expect("static compile_error invocation parses");
        }
    };

    match generator::impl_builder_with_support_path(input, &support_crate) {
        Ok(code) => code,
        Err(why) => why,
    }
}
