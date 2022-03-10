extern crate proc_macro;

use core::panic;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{Data, DataStruct, Fields, FieldsNamed, GenericArgument, PathArguments, Type, TypePath};

pub mod parts {
    /// marker trait
    pub trait Ready {}

    #[repr(transparent)]
    pub struct Certain<T>(T);
    impl<T> Certain<T> {
        pub const fn new(t: T) -> Self {
            Self(t)
        }
    }
    impl<T> Ready for Certain<T> {}

    /// behaves like core::mem::MaybeUninit.
    /// The inner data will be ignored with code::mem::forget when drops,
    /// so you MUST use core::mem::transmute or something to safely drop the inner data
    ///
    /// # Exmple
    /// ```
    /// use crate::Uninit;
    ///
    /// fn main() {
    ///     let data = unsafe { Uninit::<String>::uninit() };
    ///     drop(data); // ok
    ///
    ///     let data = Uninit::new(String::from("test"));
    ///     // drop(data); // leaks inner string!!!
    ///     let data: String = unsafe{ core::memm::transmute(data) }; // transmute
    ///     drop(data); // ok, not leaks the string
    /// }
    /// ```
    #[repr(transparent)]
    pub struct Uninit<T>(core::mem::ManuallyDrop<T>);
    impl<T> Uninit<T> {
        pub unsafe fn uninit() -> Self {
            core::mem::MaybeUninit::uninit().assume_init()
        }
        pub const unsafe fn new(t: T) -> Self {
            Self(core::mem::ManuallyDrop::new(t))
        }
    }

    #[repr(transparent)]
    pub struct None<T>(Option<T>);
    impl<T> None<T> {
        pub const fn new() -> None<T> {
            Self(Option::None)
        }
    }
    impl<T> Ready for None<T> {}

    #[repr(transparent)]
    pub struct Some<T>(Option<T>);
    impl<T> Some<T> {
        pub const fn new(t: T) -> Self {
            Self(Option::Some(t))
        }
    }
    impl<T> Ready for Some<T> {}

    #[repr(transparent)]
    pub struct Default<T>(T);
    impl<T> Default<T> {
        pub const fn new(t: T) -> Self {
            Self(t)
        }
    }
    impl<T> Ready for Default<T> {}
}

pub fn impl_builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let original_name = &ast.ident;
    let original_generic_args = &ast.generics;
    if 1 <= original_generic_args.params.len() {
        panic!("structs with generic parameters are not yet suppoted.");
    }

    let builder_name = quote::format_ident!("{}Builder", original_name);

    let fields = fields(&ast.data);
    let fields = &fields.named;

    // let unit = Type::Tuple(TypeTuple {
    //     paren_token: Paren::default(),
    //     elems: Punctuated::new(),
    // });

    let builder = {
        let builder_generics = fields
            .iter()
            .map(|field| {
                let field_name = field.ident.as_ref().unwrap();
                to_camel_case(&field_name.to_string())
            })
            .collect::<Vec<_>>();

        let generics = builder_generics
            .iter()
            .map(|g| Ident::new(&g, Span::call_site()));

        let fields = fields
            .iter()
            .map(|field| {
                let field_name = field.ident.as_ref().unwrap();
                let type_name = to_camel_case(&field_name.to_string());
                let type_name = Ident::new(&type_name, Span::call_site());
                quote! { #field_name: #type_name }
            })
            .collect::<Vec<_>>();

        quote! {
            struct #builder_name < #(#generics),* > {
                #(#fields,)*
            }
        }
    };

    let initial_generic_args = fields.iter().map(|field| {
        let ty = &field.ty;
        quote! { ::builder_pattern::parts::Uninit< #ty > }
    });
    let initialize_builder_fields = {
        let initilizes = fields.iter().map(|field| {
            let name = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            quote! { #name: unsafe { ::builder_pattern::parts::Uninit::< #ty >::uninit() } }
        });

        quote! { #(#initilizes),* }
    };

    let impl_setters = {
        fields.iter().flat_map(|target| {
            let name = target.ident.as_ref().unwrap();
            let ty = &target.ty;

            let impl_generic_args = fields.iter().filter_map(|field| {
                if field.ident == target.ident {
                    None
                } else {
                    let type_name = to_camel_case(&field.ident.as_ref().unwrap().to_string());
                    let type_name = Ident::new(&type_name, Span::call_site());
                    quote! { #type_name }.into()
                }
            });
            let current_builder_generic_args = fields.iter().map(|field| {
                if field.ident == target.ident {
                    let name = field.ident.as_ref().unwrap();
                    let ty = &field.ty;
                    quote! { ::builder_pattern::parts::Uninit::< #ty >}
                } else {
                    let type_name = to_camel_case(&field.ident.as_ref().unwrap().to_string());
                    let type_name = Ident::new(&type_name, Span::call_site());
                    quote! { #type_name }
                }
            });
            let next_builder_generic_args = fields.iter().map(|field| {
                if field.ident == target.ident {
                    let name = field.ident.as_ref().unwrap();
                    let ty = &field.ty;
                    quote! { ::builder_pattern::parts::Certain::< #ty >}
                } else {
                    let type_name = to_camel_case(&field.ident.as_ref().unwrap().to_string());
                    let type_name = Ident::new(&type_name, Span::call_site());
                    quote! { #type_name }
                }
            });

            quote! {
                impl< #(#impl_generic_args),* > #builder_name < #(#current_builder_generic_args),* > {
                    pub fn #name(mut self, value: #ty) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            self.#name = ::builder_pattern::parts::Uninit::new(value);
                            let builder = core::mem::transmute_copy(&self);
                            core::mem::forget(self);
                            builder
                        }
                    }
                }
            }
        })
    };

    let impl_final_build = {
        let impl_generic_args = fields
            .iter()
            .map(|field| {
                let type_name = to_camel_case(&field.ident.as_ref().unwrap().to_string());
                let type_name = Ident::new(&type_name, Span::call_site());
                quote! { #type_name }
            })
            .collect::<Vec<_>>();
        let constraints = fields.iter().map(|field| {
            let type_name = to_camel_case(&field.ident.as_ref().unwrap().to_string());
            let type_name = Ident::new(&type_name, Span::call_site());
            quote! { #type_name: ::builder_pattern::parts::Ready }
        });

        quote! {
            impl < #(#impl_generic_args),* > #builder_name < #(#impl_generic_args),* >
                where #(#constraints),*
            {
                pub fn build(self) -> #original_name {
                    unsafe {
                        let builder = core::mem::transmute_copy(&self);
                        core::mem::forget(self);
                        builder
                    }
                }
            }
        }
    };

    let code = quote! {
        impl #original_name {
            fn builder() -> #builder_name < #(#initial_generic_args),* > {
                #builder_name {
                    #initialize_builder_fields
                }
            }
        }

        #builder
        #(#impl_setters)*
        #impl_final_build
    };

    code.into()
}

/// extract FieldsNamed from the given struct
///
fn fields(data: &Data) -> &FieldsNamed {
    let fields = match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => fields,
        Data::Struct(_) => panic!("unit structs and tuple structs are not allowed."),
        Data::Enum(_) => panic!("expected struct, found enum."),
        Data::Union(_) => panic!("expected struct, found union."),
    };
    if fields.named.is_empty() {
        panic!("structs with no fields are not allowed.");
    }
    fields
}

fn check_option(ty: &Type) -> Option<&GenericArgument> {
    let maybe_opt = match ty {
        Type::Path(TypePath { qself: None, path }) => path,
        _ => return None,
    };

    let ty: String = maybe_opt
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<String>>()
        .join("::");

    let is_option = ["Option", "std::option::Option", "core::option::Option"]
        .into_iter()
        .find(|s| s == &ty)
        .is_some();
    if !is_option {
        return None;
    }

    let seg = maybe_opt.segments.iter().last().unwrap();
    if let PathArguments::AngleBracketed(a) = &seg.arguments {
        a.args.first().unwrap().into()
    } else {
        None
    }
}

fn check_vec(ty: &Type) -> Option<(&GenericArgument, Option<&GenericArgument>)> {
    let maybe_vec = match ty {
        Type::Path(TypePath { qself: None, path }) => path,
        _ => return None,
    };

    let ty: String = maybe_vec
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<String>>()
        .join("::");

    let is_vec = ["Vec", "std::vec::Vec"]
        .into_iter()
        .find(|s| s == &ty)
        .is_some();
    if !is_vec {
        return None;
    }

    let seg = maybe_vec.segments.iter().last().unwrap();
    if let PathArguments::AngleBracketed(a) = &seg.arguments {
        let ty = a.args.first().unwrap();
        let allocator = a.args.iter().skip(1).take(1).last();
        (ty, allocator).into()
    } else {
        None
    }
}

fn to_camel_case(str: &str) -> String {
    str.split('_')
        .map(|s| {
            let mut s = s.to_ascii_lowercase();
            let (head, _) = s.split_at_mut(1);
            head.make_ascii_uppercase();
            s
        })
        .collect::<Vec<_>>()
        .join("")
}

#[cfg(test)]
mod tests {}
