extern crate proc_macro;
use builder_pattern::impl_builder;
use proc_macro::TokenStream;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder_derive(input: TokenStream) -> TokenStream {
    match impl_builder(input) {
        Ok(code) => code,
        Err(why) => why,
    }
}
