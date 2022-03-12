#![feature(allocator_api)]
extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DataStruct, Error, Fields, FieldsNamed};

mod builder_item;
pub mod parts;

use builder_item::{BuilderItem, BuilderItemType};

pub fn impl_builder(
    input: proc_macro::TokenStream,
) -> Result<proc_macro::TokenStream, proc_macro::TokenStream> {
    let ast: syn::DeriveInput = syn::parse(input).map_err(|err| err.to_compile_error())?;

    let original_name = &ast.ident;
    let original_generic_args = &ast.generics;
    if 1 <= original_generic_args.params.len() {
        return Err(to_compile_error(
            original_generic_args,
            "structs with generic parameters are not yet supported.",
        )
        .into());
    }

    let builder_name = quote::format_ident!("{}Builder", original_name);

    let fields = fields(&ast.data).map_err(|message| to_compile_error(&ast, message))?;
    let builder_items = fields
        .named
        .iter()
        .map(|field| BuilderItem::try_from(field))
        .collect::<Result<Vec<_>, _>>()?;

    let builder_struct = {
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
            pub fn builder() -> #builder_name < #(#initial_generic_args),* > {
                #builder_name {
                    #initialize_builder_fields
                }
            }
        }

        #builder_struct

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
fn fields(data: &Data) -> Result<&FieldsNamed, &'static str> {
    match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            if fields.named.is_empty() {
                Err("structs with no fields are not allowed.")
            } else {
                Ok(fields)
            }
        }
        Data::Struct(_) => Err("unit structs and tuple structs are not allowed."),
        Data::Enum(_) => Err("expected struct, found enum."),
        Data::Union(_) => Err("expected struct, found union."),
    }
}

fn to_compile_error<T, U>(tokens: T, message: U) -> TokenStream
where
    T: ToTokens,
    U: core::fmt::Display,
{
    Error::new_spanned(tokens, message).to_compile_error()
}

#[cfg(test)]
mod tests {}
