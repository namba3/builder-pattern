extern crate proc_macro;
use builder_pattern::impl_builder;

#[proc_macro_derive(Builder)]
pub fn builder_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_builder(input)
}
