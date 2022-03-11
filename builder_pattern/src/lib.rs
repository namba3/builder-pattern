#![feature(allocator_api)]
extern crate proc_macro;

use core::panic;

use proc_macro2::{Ident, Span};
use quote::{format_ident, quote, ToTokens};
use syn::{
    Data, DataStruct, Field, Fields, FieldsNamed, GenericArgument, PathArguments, Type, TypePath,
};

pub mod parts {
    /// marker trait
    pub trait Ready {}

    #[repr(transparent)]
    pub struct Certain<T>(T);
    impl<T> Certain<T> {
        #[inline]
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
        #[inline]
        pub unsafe fn uninit() -> Self {
            core::mem::MaybeUninit::uninit().assume_init()
        }
        #[inline]
        pub const unsafe fn new(t: T) -> Self {
            Self(core::mem::ManuallyDrop::new(t))
        }
    }

    #[repr(transparent)]
    pub struct None<T>(Option<T>);
    impl<T> None<T> {
        #[inline]
        pub const fn new() -> None<T> {
            Self(Option::None)
        }
    }
    impl<T> Ready for None<T> {}

    #[repr(transparent)]
    pub struct Some<T>(Option<T>);
    impl<T> Some<T> {
        #[inline]
        pub const fn new(t: T) -> Self {
            Self(Option::Some(t))
        }
    }
    impl<T> Ready for Some<T> {}

    #[repr(transparent)]
    pub struct Vec<T>(std::vec::Vec<T>);
    impl<T> Vec<T> {
        #[inline]
        pub const fn new() -> Self {
            Self(std::vec::Vec::new())
        }
        #[inline]
        pub fn push(&mut self, t: T) {
            self.0.push(t);
        }
        #[inline]
        pub fn extend<Iter: core::iter::IntoIterator<Item = T>>(&mut self, iter: Iter) {
            self.0.extend(iter)
        }
    }
    impl<T> Ready for Vec<T> {}

    #[repr(transparent)]
    pub struct Default<T>(T);
    impl<T> Default<T> {
        #[inline]
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
    let builder_items = fields
        .named
        .iter()
        .map(|field| BuilderItem::from(field))
        .collect::<Vec<_>>();

    let builder = {
        let generics = builder_items.iter().map(|item| &item.generics_ident);

        let fields = builder_items.iter().map(
            |BuilderItem {
                 field_name,
                 generics_ident,
                 ..
             }| {
                quote! { #field_name: #generics_ident }
            },
        );

        quote! {
            struct #builder_name < #(#generics),* > {
                #(#fields,)*
            }
        }
    };

    let initial_generic_args = builder_items.iter().map(|BuilderItem { ty, .. }| match ty {
        BuilderItemType::Option { inner_type } => {
            quote! { ::builder_pattern::parts::None< #inner_type > }
        }

        BuilderItemType::Vec { inner_type, .. } => {
            quote! { ::builder_pattern::parts::Vec< #inner_type > }
        }
        BuilderItemType::Other(ty) => quote! { ::builder_pattern::parts::Uninit< #ty > },
    });
    let initialize_builder_fields = {
        let initilizes = builder_items.iter().map(|BuilderItem {field_name, ty, ..}| {
            match ty {
                BuilderItemType::Option { inner_type } => {
                    quote! { #field_name: ::builder_pattern::parts::None::< #inner_type > ::new() }
                }
                BuilderItemType::Vec { inner_type, .. } => {
                    quote! { #field_name: ::builder_pattern::parts::Vec::< #inner_type > ::new() }
                }
                BuilderItemType::Other(ty) => quote! { #field_name: unsafe { ::builder_pattern::parts::Uninit::< #ty >::uninit() } }
            }
        });

        quote! { #(#initilizes),* }
    };

    let impl_setters = builder_items.iter().flat_map(|target| {
        let target_name = target.field_name;
        let target_type = &target.ty;

        let impl_generics = builder_items.iter().filter_map(|BuilderItem{field_name, generics_ident, ..}| {
            if *field_name == target_name {
                None
            } else {
                quote! { #generics_ident }.into()
            }
        });
        let current_builder_generic_args = builder_items.iter().map(|BuilderItem{field_name, ty, generics_ident}| {
            if *field_name == target_name {
                match ty {
                    BuilderItemType::Option { inner_type } => {
                        quote! { ::builder_pattern::parts::None< #inner_type > }
                    }
                    BuilderItemType::Vec { inner_type, .. } => {
                        quote! { ::builder_pattern::parts::Vec< #inner_type > }
                    }
                    BuilderItemType::Other(ty) => quote! { ::builder_pattern::parts::Uninit< #ty >}
                }
            } else {
                quote! { #generics_ident }
            }
        });
        let next_builder_generic_args = builder_items.iter().map(|BuilderItem{field_name, ty, generics_ident}| {
            if *field_name == target_name {
                match ty {
                    BuilderItemType::Option { inner_type } => {
                        quote! { ::builder_pattern::parts::Some< #inner_type > }
                    }
                    BuilderItemType::Vec { inner_type, .. } => {
                        quote! { ::builder_pattern::parts::Vec< #inner_type > }
                    }
                    BuilderItemType::Other(ty) => quote! { ::builder_pattern::parts::Certain< #ty >}
                }
            } else {
                quote! { #generics_ident }
            }
        }).collect::<Vec<_>>();

        match target_type {
            BuilderItemType::Option { inner_type} => quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_name(mut self, value: #inner_type) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            let mut builder: #builder_name < #(#next_builder_generic_args),* > = core::mem::transmute_copy(&self);
                            core::mem::forget(self);
                            builder.#target_name = ::builder_pattern::parts::Some::new(value);
                            builder
                        }
                    }
                }
            },
            BuilderItemType::Vec {inner_type, ..} => {
                let target_name_append = format_ident!("{target_name}_append");
                quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_name(mut self, value: #inner_type) -> #builder_name < #(#next_builder_generic_args),* > {
                        self.#target_name.push(value);
                        self
                    }
                    pub fn #target_name_append<Iter: core::iter::IntoIterator<Item=#inner_type>>(mut self, iter: Iter) -> #builder_name < #(#next_builder_generic_args),* > {
                        self.#target_name.extend(iter);
                        self
                    }
                }}
            },
            BuilderItemType::Other (ty) => quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_name(mut self, value: #ty) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            self.#target_name = ::builder_pattern::parts::Uninit::new(value);
                            let builder = core::mem::transmute_copy(&self);
                            core::mem::forget(self);
                            builder
                        }
                    }
                }
            },
        }
    });

    let impl_final_build = {
        let impl_generics = builder_items
            .iter()
            .map(|BuilderItem { generics_ident, .. }| {
                quote! { #generics_ident }
            })
            .collect::<Vec<_>>();
        let constraints = builder_items
            .iter()
            .map(|BuilderItem { generics_ident, .. }| {
                quote! { #generics_ident: ::builder_pattern::parts::Ready }
            });

        quote! {
            impl < #(#impl_generics),* > #builder_name < #(#impl_generics),* >
                where #(#constraints),*
            {
                #[inline]
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

    // let code = quote! {
    //     impl #original_name {
    //         fn builder() {
    //             println!("{}", stringify!(#code));
    //         }
    //     }
    // };

    code.into()
}

///
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

struct BuilderItem<'a> {
    field_name: &'a Ident,
    ty: BuilderItemType<'a>,
    generics_ident: Ident,
}
impl<'a> From<&'a Field> for BuilderItem<'a> {
    fn from(field: &'a Field) -> Self {
        let field_name = field.ident.as_ref().unwrap();
        let generics_str = to_camel_case(&field_name.to_string());
        Self {
            field_name: field.ident.as_ref().unwrap(),
            ty: BuilderItemType::from(&field.ty),
            generics_ident: Ident::new(&generics_str, Span::call_site()),
        }
    }
}

enum BuilderItemType<'a> {
    Option {
        inner_type: &'a GenericArgument,
    },
    Vec {
        inner_type: &'a GenericArgument,
        allocator: Option<&'a GenericArgument>,
    },
    Other(&'a Type),
}
impl<'a> From<&'a Type> for BuilderItemType<'a> {
    fn from(ty: &'a Type) -> Self {
        let path = if let Type::Path(TypePath { qself: None, path }) = ty {
            path
        } else {
            return BuilderItemType::Other(ty);
        };

        let path_str: String = path
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect::<Vec<String>>()
            .join("::");

        let last_seg = path.segments.iter().last().unwrap();
        match path_str {
            maybe_opt if is_option(&maybe_opt) => {
                if let PathArguments::AngleBracketed(a) = &last_seg.arguments {
                    BuilderItemType::Option {
                        inner_type: a.args.first().unwrap(),
                    }
                } else {
                    BuilderItemType::Other(ty)
                }
            }
            maybe_vec if is_vec(&maybe_vec) => {
                if let PathArguments::AngleBracketed(a) = &last_seg.arguments {
                    let inner_type = a.args.first().unwrap();
                    let allocator = a.args.iter().skip(1).take(1).last();

                    if allocator.is_some() {
                        panic!("Vec with custom allocator is not suppoted.");
                    }

                    BuilderItemType::Vec {
                        inner_type,
                        allocator,
                    }
                } else {
                    BuilderItemType::Other(ty)
                }
            }
            _other => BuilderItemType::Other(ty),
        }
    }
}
impl<'a> ToTokens for BuilderItemType<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ts = match self {
            BuilderItemType::Option { inner_type } => {
                quote! { core::option::Option< #inner_type >}
            }
            BuilderItemType::Vec {
                inner_type,
                allocator: None,
            } => quote! { std::vec::Vec< #inner_type >},
            BuilderItemType::Vec {
                inner_type,
                allocator: Some(allocator),
            } => quote! { std::vec::Vec< #inner_type, #allocator >},
            BuilderItemType::Other(ty) => quote! { #ty },
        };
        tokens.extend(ts);
    }
}

fn is_option(path: &str) -> bool {
    ["Option", "std::option::Option", "core::option::Option"]
        .into_iter()
        .find(|s| *s == path)
        .is_some()
}

fn is_vec(path: &str) -> bool {
    ["Vec", "std::vec::Vec"]
        .into_iter()
        .find(|s| *s == path)
        .is_some()
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
