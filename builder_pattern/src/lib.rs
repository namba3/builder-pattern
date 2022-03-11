#![feature(allocator_api)]
extern crate proc_macro;

use core::panic;

use proc_macro2::{Ident};
use quote::{format_ident, quote, ToTokens};
use syn::{
    Data, DataStruct, Field, Fields, FieldsNamed, GenericArgument, Lit, Meta,
    MetaNameValue, NestedMeta, PathArguments, Type, TypePath,
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
    pub struct False(bool);
    impl False {
        #[inline]
        pub const fn new() -> False {
            Self(false)
        }
    }
    impl Ready for False {}

    #[repr(transparent)]
    pub struct True(bool);
    impl True {
        #[inline]
        pub const fn new() -> True {
            Self(true)
        }
    }
    impl Ready for True {}

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

pub fn impl_builder(
    input: proc_macro::TokenStream,
) -> Result<proc_macro::TokenStream, proc_macro::TokenStream> {
    let ast: syn::DeriveInput = syn::parse(input).map_err(|err| err.to_compile_error())?;

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
        BuilderItemType::Flag => quote! { ::builder_pattern::parts::False },
        BuilderItemType::Option { inner_type } => {
            quote! { ::builder_pattern::parts::None< #inner_type > }
        }

        BuilderItemType::Vec { inner_type, .. } => {
            quote! { ::builder_pattern::parts::Vec< #inner_type > }
        }
        BuilderItemType::AsIs(ty) => quote! { ::builder_pattern::parts::Uninit< #ty > },
    });
    let initialize_builder_fields = {
        let initilizes = builder_items.iter().map(|BuilderItem {field_name, ty, ..}| {
            match ty {
                BuilderItemType::Flag => quote!{ #field_name: ::builder_pattern::parts::False::new() },
                BuilderItemType::Option { inner_type } => {
                    quote! { #field_name: ::builder_pattern::parts::None::< #inner_type > ::new() }
                }
                BuilderItemType::Vec { inner_type, .. } => {
                    quote! { #field_name: ::builder_pattern::parts::Vec::< #inner_type > ::new() }
                }
                BuilderItemType::AsIs(ty) => quote! { #field_name: unsafe { ::builder_pattern::parts::Uninit::< #ty >::uninit() } }
            }
        });

        quote! { #(#initilizes),* }
    };

    let impl_setters = builder_items.iter().flat_map(|target| {
        let target_field_name = target.field_name;
        let target_ty = &target.ty;
        let target_method_name = &target.method_name;

        let impl_generics = builder_items.iter().filter_map(|BuilderItem{field_name, generics_ident, ..}| {
            if *field_name == target_field_name {
                None
            } else {
                quote! { #generics_ident }.into()
            }
        });
        let current_builder_generic_args = builder_items.iter().map(|BuilderItem{field_name, ty, generics_ident, ..}| {
            if *field_name == target_field_name {
                match ty {
                    BuilderItemType::Flag => quote!{ ::builder_pattern::parts::False },
                    BuilderItemType::Option { inner_type } => {
                        quote! { ::builder_pattern::parts::None< #inner_type > }
                    }
                    BuilderItemType::Vec { inner_type, .. } => {
                        quote! { ::builder_pattern::parts::Vec< #inner_type > }
                    }
                    BuilderItemType::AsIs(ty) => quote! { ::builder_pattern::parts::Uninit< #ty >}
                }
            } else {
                quote! { #generics_ident }
            }
        });
        let next_builder_generic_args = builder_items.iter().map(|BuilderItem{field_name, ty, generics_ident, ..}| {
            if *field_name == target_field_name {
                match ty {
                    BuilderItemType::Flag => quote!{ ::builder_pattern::parts::True },
                    BuilderItemType::Option { inner_type } => {
                        quote! { ::builder_pattern::parts::Some< #inner_type > }
                    }
                    BuilderItemType::Vec { inner_type, .. } => {
                        quote! { ::builder_pattern::parts::Vec< #inner_type > }
                    }
                    BuilderItemType::AsIs(ty) => quote! { ::builder_pattern::parts::Certain< #ty >}
                }
            } else {
                quote! { #generics_ident }
            }
        }).collect::<Vec<_>>();

        match target_ty {
            BuilderItemType::Flag => quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_method_name(mut self) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            let mut builder: #builder_name < #(#next_builder_generic_args),* > = core::mem::transmute_copy(&self);
                            core::mem::forget(self);
                            builder.#target_field_name = ::builder_pattern::parts::True::new();
                            builder
                        }
                    }
                }
            },
            BuilderItemType::Option { inner_type} => quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_method_name(mut self, value: #inner_type) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            let mut builder: #builder_name < #(#next_builder_generic_args),* > = core::mem::transmute_copy(&self);
                            core::mem::forget(self);
                            builder.#target_field_name = ::builder_pattern::parts::Some::new(value);
                            builder
                        }
                    }
                }
            },
            BuilderItemType::Vec {inner_type,  ..} => {
                let each = if let Some(target_each_method_name) = &target.each_method_name {
                    quote!{
                        #[inline]
                        pub fn #target_each_method_name(mut self, value: #inner_type) -> #builder_name < #(#next_builder_generic_args),* > {
                            self.#target_field_name.push(value);
                            self
                        }
                    }.into()
                } else {
                    None
                };
                quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #each

                    #[inline]
                    pub fn #target_method_name<Iter: core::iter::IntoIterator<Item=#inner_type>>(mut self, iter: Iter) -> #builder_name < #(#next_builder_generic_args),* > {
                        self.#target_field_name.extend(iter);
                        self
                    }
                }}
            },
            BuilderItemType::AsIs (ty) => quote!{
                impl< #(#impl_generics),* > #builder_name < #(#current_builder_generic_args),* > {
                    #[inline]
                    pub fn #target_method_name(mut self, value: #ty) -> #builder_name < #(#next_builder_generic_args),* > {
                        unsafe {
                            self.#target_field_name = ::builder_pattern::parts::Uninit::new(value);
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

    Ok(code.into())
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
    method_name: Ident,
    each_method_name: Option<Ident>,
    ty: BuilderItemType<'a>,
    generics_ident: Ident,
}
impl<'a> From<&'a Field> for BuilderItem<'a> {
    fn from(field: &'a Field) -> Self {
        let attr = field.attrs.iter().find(|attr| {
            if let Some(name) = attr.path.segments.last() {
                let name = &name.ident.to_string();
                name == "builder"
            } else {
                false
            }
        });
        struct BuilderAttribute {
            as_is_denoted: bool,
            name: Option<String>,
            each: Option<String>,
        }

        let builder_attr: Option<BuilderAttribute> = if let Some(attr) = attr {
            let meta = attr.parse_meta();
            match meta {
                Ok(meta) => match meta {
                    Meta::List(list) => {
                        let items = list
                            .nested
                            .iter()
                            .map(|meta| match meta {
                                NestedMeta::Lit(lit) => {
                                    Err(format!("found literal '{}'", lit.to_token_stream()))
                                }
                                NestedMeta::Meta(meta) => match meta {
                                    Meta::NameValue(MetaNameValue { path, lit, .. }) => {
                                        Ok((path, Some(lit)))
                                    }
                                    Meta::Path(path) => Ok((path, None)),
                                    Meta::List(list) => {
                                        Err(format!("found list '{}'", list.into_token_stream()))
                                    }
                                },
                            })
                            .collect::<Result<Vec<_>, _>>();
                        let items = match items {
                            Err(why) => {
                                panic!("{}", why);
                            }
                            Ok(l) => l,
                        };

                        enum ItemType {
                            AsIs,
                            Name,
                            Each,
                        }
                        let items = items
                            .into_iter()
                            .map(|(path, lit)| {
                                let path = path.segments.last().unwrap().ident.to_string();
                                match path.as_str() {
                                    "as_is" => Ok((ItemType::AsIs, lit)).into(),
                                    "name" => Ok((ItemType::Name, lit)).into(),
                                    "each" => Ok((ItemType::Each, lit)).into(),
                                    _ => Err(format!("unexpected attribute '{}'.", path)),
                                }
                            })
                            .collect::<Result<Vec<_>, _>>();
                        let items = match items {
                            Ok(x) => x,
                            Err(why) => panic!("{why}"),
                        };

                        let as_is_denoted = items
                            .iter()
                            .filter_map(|(item_type, lit)| match item_type {
                                ItemType::AsIs => {
                                    if let Some(lit) = lit {
                                        Err(format!(
                                            "expected 'as_is', found 'as_is = {}'.",
                                            lit.into_token_stream()
                                        ))
                                        .into()
                                    } else {
                                        Ok(()).into()
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Result<Vec<_>, _>>();
                        let as_is_denoted = match as_is_denoted {
                            Ok(x) => x,
                            Err(why) => panic!("{why}"),
                        };
                        let as_is_denoted = as_is_denoted.into_iter().any(|_| true);

                        let name = items
                            .iter()
                            .filter_map(|(item_type, lit)| {
                                match item_type {
                                    ItemType::Name => {
                                        if let Some(lit) = lit {
                                            match lit {
                                                Lit::Str(str) => Ok(str.value()).into(),
                                                _ => Err(format!(
                                                    "expected 'name = \"method_name\"', found 'name = {}'",
                                                    lit.into_token_stream()
                                                )).into(),
                                            }
                                        } else {
                                            Err(format!(
                                                "expected 'name = \"method_name\"', found 'name = {}'",
                                                lit.into_token_stream()
                                            )).into()
                                        }
                                    }
                                    _ => None,
                                }
                            })
                            .collect::<Result<Vec<_>, String>>();
                        let name = match name {
                            Ok(x) => x,
                            Err(why) => panic!("{why}"),
                        };
                        let name = name.into_iter().last().map(|s| s);

                        let each = items
                        .iter()
                        .filter_map(|(item_type, lit)| {
                            match item_type {
                                ItemType::Each => {
                                    if let Some(lit) = lit {
                                        match lit {
                                            Lit::Str(str) => Ok(str.value()).into(),
                                            _ => Err(format!(
                                                "expected 'each = \"method_name\"', found 'each = {}'",
                                                lit.into_token_stream()
                                            )).into(),
                                        }
                                    } else {
                                        Err(format!(
                                            "expected 'each = \"method_name\"', found 'each = {}'",
                                            lit.into_token_stream()
                                        )).into()
                                    }
                                }
                                _ => None,
                            }
                        })
                        .collect::<Result<Vec<_>, String>>();
                        let each = match each {
                            Ok(x) => x,
                            Err(why) => panic!("{why}"),
                        };
                        let each = each.into_iter().last().map(|s| s);

                        BuilderAttribute {
                            as_is_denoted,
                            name,
                            each,
                        }
                        .into()
                    }
                    Meta::Path(path) => {
                        panic!(
                            "expected 'builder = \"setter_name\"' or 'builder(name = \"setter_name\")', found '{}'.",
                            path.into_token_stream()
                        );
                    }
                    Meta::NameValue(MetaNameValue { lit, .. }) => {
                        if let Lit::Str(str) = lit {
                            BuilderAttribute {
                                as_is_denoted: false,
                                name: str.value().into(),
                                each: None,
                            }
                            .into()
                        } else {
                            panic!(
                                "expected 'builder = \"setter_name\"', found 'builder = {}'.",
                                lit.into_token_stream()
                            );
                        }
                    }
                },
                Err(why) => {
                    panic!("{why}",);
                }
            }
        } else {
            None
        };

        let field_name = field.ident.as_ref().unwrap();
        let generics_ident = format_ident!("{}", to_camel_case(&field_name.to_string()));

        let ty = if let Some(BuilderAttribute {
            as_is_denoted: true,
            ..
        }) = builder_attr
        {
            BuilderItemType::AsIs(&field.ty)
        } else {
            BuilderItemType::from(&field.ty)
        };

        let method_name = builder_attr
            .as_ref()
            .and_then(|attr| attr.name.as_ref())
            .map(|name| format_ident!("{name}"))
            .unwrap_or_else(|| field_name.clone());

        let each_method_name = match ty {
            BuilderItemType::Vec { .. } => builder_attr
                .as_ref()
                .and_then(|attr| attr.each.as_ref())
                .map(|each| format_ident!("{each}")),
            _ => None,
        };

        Self {
            field_name,
            method_name,
            each_method_name,
            ty,
            generics_ident,
        }
    }
}

enum BuilderItemType<'a> {
    Flag,
    Option {
        inner_type: &'a GenericArgument,
    },
    Vec {
        inner_type: &'a GenericArgument,
        allocator: Option<&'a GenericArgument>,
    },
    AsIs(&'a Type),
}
impl<'a> From<&'a Type> for BuilderItemType<'a> {
    fn from(ty: &'a Type) -> Self {
        let path = if let Type::Path(TypePath { qself: None, path }) = ty {
            path
        } else {
            return BuilderItemType::AsIs(ty);
        };

        let path_str: String = path
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect::<Vec<String>>()
            .join("::");

        let last_seg = path.segments.iter().last().unwrap();
        match path_str {
            maybe_bool if is_bool(&maybe_bool) => BuilderItemType::Flag,
            maybe_opt if is_option(&maybe_opt) => {
                if let PathArguments::AngleBracketed(a) = &last_seg.arguments {
                    BuilderItemType::Option {
                        inner_type: a.args.first().unwrap(),
                    }
                } else {
                    BuilderItemType::AsIs(ty)
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
                    BuilderItemType::AsIs(ty)
                }
            }
            _other => BuilderItemType::AsIs(ty),
        }
    }
}
impl<'a> ToTokens for BuilderItemType<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ts = match self {
            BuilderItemType::Flag => quote! { core::primitive::bool },
            BuilderItemType::Option { inner_type } => {
                quote! { core::option::Option< #inner_type >}
            }
            BuilderItemType::Vec {
                inner_type,
                allocator: None,
                ..
            } => quote! { std::vec::Vec< #inner_type >},
            BuilderItemType::Vec {
                inner_type,
                allocator: Some(allocator),
                ..
            } => quote! { std::vec::Vec< #inner_type, #allocator >},
            BuilderItemType::AsIs(ty) => quote! { #ty },
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

fn is_bool(path: &str) -> bool {
    ["bool", "core::primitive::bool", "std::primitive::bool"]
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
