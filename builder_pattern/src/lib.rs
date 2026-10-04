extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Data, DataStruct, Error, Fields, FieldsNamed, GenericParam, Generics, Path, visit::Visit,
};

mod builder_item;
pub mod parts;

use crate::builder_item::{BuilderItem, BuilderItemType, InitialExpr};

#[derive(Default)]
struct IdentCollector(std::collections::HashSet<String>);

impl<'ast> Visit<'ast> for IdentCollector {
    fn visit_ident(&mut self, ident: &'ast syn::Ident) {
        self.0.insert(ident.to_string());
    }
}

pub fn impl_builder(
    input: proc_macro::TokenStream,
) -> Result<proc_macro::TokenStream, proc_macro::TokenStream> {
    impl_builder_with_support_path(input, "::builder_pattern")
}

pub fn impl_builder_with_support_path(
    input: proc_macro::TokenStream,
    support_crate_path: &str,
) -> Result<proc_macro::TokenStream, proc_macro::TokenStream> {
    let ast: syn::DeriveInput = syn::parse(input).map_err(|err| err.to_compile_error())?;
    let support_crate_path = syn::parse_str::<Path>(support_crate_path)
        .map_err(|err| proc_macro::TokenStream::from(err.to_compile_error()))?;
    let parts_path = quote!(#support_crate_path::parts);

    let original_name = &ast.ident;
    let original_visibility = &ast.vis;
    let original_generics = &ast.generics;
    let original_generic_args = generic_arguments(original_generics);
    let (original_impl_generics, original_type_generics, original_where_clause) =
        original_generics.split_for_impl();

    let builder_name = quote::format_ident!("{}Builder", original_name);

    let fields = fields(&ast.data).map_err(|message| to_compile_error(&ast, message))?;
    let mut builder_items = fields
        .named
        .iter()
        .map(|field| BuilderItem::try_from(field))
        .collect::<Result<Vec<_>, _>>()?;
    let mut used_identifiers = IdentCollector::default();
    used_identifiers.visit_derive_input(&ast);
    let mut next_state_index = 0;
    for item in &mut builder_items {
        loop {
            let name = format!("__BuilderState{next_state_index}");
            next_state_index += 1;
            if used_identifiers.0.insert(name.clone()) {
                item.generics_ident = quote::format_ident!("{name}");
                break;
            }
        }
    }
    let phantom_field = loop {
        let name = format!("__builder_generics{next_state_index}");
        next_state_index += 1;
        if used_identifiers.0.insert(name.clone()) {
            break quote::format_ident!("{name}");
        }
    };

    let builder_generics = with_state_generics(
        original_generics,
        builder_items.iter().map(|item| &item.generics_ident),
    );

    let builder_struct = {
        let (_, _, builder_where_clause) = builder_generics.split_for_impl();
        let fields = builder_items.iter().map(
            |BuilderItem {
                 field_name,
                 generics_ident,
                 ..
             }| {
                quote! { #field_name: #generics_ident }
            },
        );
        let phantom_type = quote! {
            ::core::marker::PhantomData<fn() -> #original_name #original_type_generics>
        };

        quote! {
            #original_visibility struct #builder_name #builder_generics #builder_where_clause {
                #(#fields,)*
                #phantom_field: #phantom_type,
            }
        }
    };

    let initial_generic_args = builder_items.iter().map(
        |BuilderItem {
             ty, initial_expr, ..
         }| match ty {
            BuilderItemType::Flag => quote! { #parts_path::False },
            BuilderItemType::Option { inner_type } => {
                quote! { #parts_path::None< #inner_type > }
            }

            BuilderItemType::Vec { inner_type, .. } => {
                quote! { #parts_path::Vec< #inner_type > }
            }
            BuilderItemType::AsIs(ty) => match initial_expr {
                Some(InitialExpr::Default(_)) => {
                    quote! { #parts_path::Default< #ty > }
                }
                Some(InitialExpr::Fixed(_)) => quote! { #parts_path::Fixed< #ty > },
                None => quote! { #parts_path::Uninit< #ty > },
            },
        },
    );
    let initial_builder_args = original_generic_args
        .iter()
        .cloned()
        .chain(initial_generic_args)
        .collect::<Vec<_>>();
    let initialize_builder_fields = {
        let initilizes = builder_items.iter().map(
            |BuilderItem {
                 field_name,
                 ty,
                 initial_expr,
                 ..
             }| {
                match ty {
                    BuilderItemType::Flag => quote! { #field_name: #parts_path::False::new() },
                    BuilderItemType::Option { inner_type } => {
                        quote! { #field_name: #parts_path::None::< #inner_type > ::new() }
                    }
                    BuilderItemType::Vec { inner_type, .. } => {
                        quote! { #field_name: #parts_path::Vec::< #inner_type > ::new() }
                    }
                    BuilderItemType::AsIs(ty) => match initial_expr {
                        Some(InitialExpr::Default(expr)) => {
                            quote! { #field_name: #parts_path::Default::< #ty >::new( { #expr } ) }
                        }
                        Some(InitialExpr::Fixed(expr)) => {
                            quote! { #field_name: #parts_path::Fixed::< #ty >::new( { #expr } )}
                        }
                        None => quote! { #field_name: #parts_path::Uninit::< #ty >::uninit() },
                    },
                }
            },
        );

        quote! { #(#initilizes),* }
    };
    let initialize_builder_fields = quote! {
        #initialize_builder_fields,
        #phantom_field: ::core::marker::PhantomData,
    };

    let impl_setters = builder_items
        .iter()
        .filter(|BuilderItem {
                    ty,
                    initial_expr,
                    ..
                }| !matches!((ty, initial_expr), (BuilderItemType::AsIs(_), Some(InitialExpr::Fixed(_)))))
        .map(|target| {
            let target_field_name = target.field_name;
            let target_ty = &target.ty;
            let target_method_name = &target.method_name;

            let setter_generics = with_state_generics(
                original_generics,
                builder_items
                    .iter()
                    .filter(|item| item.field_name != target_field_name)
                    .map(|item| &item.generics_ident),
            );
            let (setter_impl_generics, _, setter_where_clause) = setter_generics.split_for_impl();
            let current_builder_generic_args = original_generic_args
                .iter()
                .cloned()
                .chain(builder_items.iter().map(
                |BuilderItem {
                     field_name,
                     ty,
                     generics_ident,
                     initial_expr,
                     ..
                 }| {
                    if *field_name != target_field_name {
                        return quote! { #generics_ident };
                    }
                    match ty {
                        BuilderItemType::Flag => quote! { #parts_path::False },
                        BuilderItemType::Option { inner_type } => {
                            quote! { #parts_path::None<#inner_type> }
                        }
                        BuilderItemType::Vec { inner_type, .. } => {
                            quote! { #parts_path::Vec<#inner_type> }
                        }
                        BuilderItemType::AsIs(ty) => match initial_expr {
                            Some(InitialExpr::Default(_)) => {
                                quote! { #parts_path::Default<#ty> }
                            }
                            Some(InitialExpr::Fixed(_)) => unreachable!(),
                            None => quote! { #parts_path::Uninit<#ty> },
                        },
                    }
                },
            ))
                .collect::<Vec<_>>();
            let next_builder_generic_args = original_generic_args
                .iter()
                .cloned()
                .chain(builder_items
                .iter()
                .map(
                    |BuilderItem {
                         field_name,
                         ty,
                         generics_ident,
                         initial_expr,
                         ..
                     }| {
                        if *field_name != target_field_name {
                            return quote! { #generics_ident };
                        }
                        match ty {
                            BuilderItemType::Flag => quote! { #parts_path::True },
                            BuilderItemType::Option { inner_type } => {
                                quote! { #parts_path::Some<#inner_type> }
                            }
                            BuilderItemType::Vec { inner_type, .. } => {
                                quote! { #parts_path::Vec<#inner_type> }
                            }
                            BuilderItemType::AsIs(ty) => match initial_expr {
                                Some(InitialExpr::Default(_)) | None => {
                                    quote! { #parts_path::Certain<#ty> }
                                }
                                Some(InitialExpr::Fixed(_)) => unreachable!(),
                            },
                        }
                    },
                ))
                .collect::<Vec<_>>();
            let value_ident = quote::format_ident!("__builder_value");
            let mut moved_fields = builder_items.iter().enumerate().map(|(index, item)| {
                let field_name = item.field_name;
                let local_name = quote::format_ident!("__builder_field_{index}");
                if field_name == target_field_name {
                    quote! { #field_name: _, }
                } else {
                    quote! { #field_name: #local_name, }
                }
            }).collect::<Vec<_>>();
            moved_fields.push(quote! { #phantom_field: __builder_phantom, });
            let replacement = match target_ty {
                BuilderItemType::Flag => quote! { #parts_path::True::new() },
                BuilderItemType::Option { .. } => {
                    quote! { #parts_path::Some::new(#value_ident) }
                }
                BuilderItemType::AsIs(_) => {
                    quote! { #parts_path::Certain::new(#value_ident) }
                }
                BuilderItemType::Vec { .. } => quote! {},
            };
            let mut output_fields = builder_items.iter().enumerate().map(|(index, item)| {
                let field_name = item.field_name;
                let local_name = quote::format_ident!("__builder_field_{index}");
                if field_name == target_field_name {
                    quote! { #field_name: #replacement, }
                } else {
                    quote! { #field_name: #local_name, }
                }
            }).collect::<Vec<_>>();
            output_fields.push(quote! { #phantom_field: __builder_phantom, });

            match target_ty {
                BuilderItemType::Flag => quote! {
                    impl #setter_impl_generics #builder_name<#(#current_builder_generic_args),*> #setter_where_clause {
                        #[inline]
                        pub fn #target_method_name(self) -> #builder_name<#(#next_builder_generic_args),*> {
                            let #builder_name { #(#moved_fields)* } = self;
                            #builder_name { #(#output_fields)* }
                        }
                    }
                },
                BuilderItemType::Option { inner_type } => quote! {
                    impl #setter_impl_generics #builder_name<#(#current_builder_generic_args),*> #setter_where_clause {
                        #[inline]
                        pub fn #target_method_name(self, #value_ident: #inner_type) -> #builder_name<#(#next_builder_generic_args),*> {
                            let #builder_name { #(#moved_fields)* } = self;
                            #builder_name { #(#output_fields)* }
                        }
                    }
                },
                BuilderItemType::Vec { inner_type, .. } => {
                    let each = target.each_method_name.as_ref().map(|each_method_name| quote! {
                        #[inline]
                        pub fn #each_method_name(mut self, #value_ident: #inner_type) -> #builder_name<#(#next_builder_generic_args),*> {
                            self.#target_field_name.push(#value_ident);
                            self
                        }
                    });
                    quote! {
                        impl #setter_impl_generics #builder_name<#(#current_builder_generic_args),*> #setter_where_clause {
                            #each

                            #[inline]
                            pub fn #target_method_name(mut self, __builder_iter: impl core::iter::IntoIterator<Item = #inner_type>) -> #builder_name<#(#next_builder_generic_args),*> {
                                self.#target_field_name.extend(__builder_iter);
                                self
                            }
                        }
                    }
                }
                BuilderItemType::AsIs(ty) => quote! {
                    impl #setter_impl_generics #builder_name<#(#current_builder_generic_args),*> #setter_where_clause {
                        #[inline]
                        pub fn #target_method_name(self, #value_ident: #ty) -> #builder_name<#(#next_builder_generic_args),*> {
                            let #builder_name { #(#moved_fields)* } = self;
                            #builder_name { #(#output_fields)* }
                        }
                    }
                },
            }
        });

    let impl_final_build = {
        let original_struct_constructor = if original_generic_args.is_empty() {
            quote! { #original_name }
        } else {
            quote! { #original_name::<#(#original_generic_args),*> }
        };
        let mut build_generics = with_state_generics(
            original_generics,
            builder_items.iter().map(|item| &item.generics_ident),
        );
        for item in &builder_items {
            let generics_ident = &item.generics_ident;
            let ty = &item.ty;
            build_generics
                .make_where_clause()
                .predicates
                .push(syn::parse_quote!(#generics_ident: #parts_path::Ready<#ty>));
        }
        let (build_impl_generics, _, build_where_clause) = build_generics.split_for_impl();
        let builder_type_args = original_generic_args
            .iter()
            .cloned()
            .chain(builder_items.iter().map(|item| {
                let generics_ident = &item.generics_ident;
                quote! { #generics_ident }
            }))
            .collect::<Vec<_>>();
        let builder_fields = builder_items.iter().enumerate().map(|(index, item)| {
            let field_name = item.field_name;
            let local_name = quote::format_ident!("__builder_field_{index}");
            quote! { #field_name: #local_name, }
        });
        let builder_fields = builder_fields
            .chain(std::iter::once(quote! { #phantom_field: _, }))
            .collect::<Vec<_>>();
        let built_fields = builder_items.iter().enumerate().map(
            |(index,
              BuilderItem {
                 field_name,
                 generics_ident,
                 ty,
                 ..
             })| {
                let local_name = quote::format_ident!("__builder_field_{index}");
                quote! {
                    #field_name: <#generics_ident as #parts_path::Ready<#ty>>::into_inner(#local_name),
                }
            },
        );

        quote! {
            impl #build_impl_generics #builder_name<#(#builder_type_args),*> #build_where_clause
            {
                #[inline]
                pub fn build(self) -> #original_name #original_type_generics {
                    let #builder_name { #(#builder_fields)* } = self;
                    #original_struct_constructor {
                        #(#built_fields)*
                    }
                }
            }
        }
    };

    let code = quote! {
        impl #original_impl_generics #original_name #original_type_generics #original_where_clause {
            #original_visibility fn builder() -> #builder_name < #(#initial_builder_args),* > {
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
/// Extract the fields from the given structure definition
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

fn generic_arguments(generics: &Generics) -> Vec<TokenStream> {
    generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Lifetime(param) => {
                let lifetime = &param.lifetime;
                quote! { #lifetime }
            }
            GenericParam::Type(param) => {
                let ident = &param.ident;
                quote! { #ident }
            }
            GenericParam::Const(param) => {
                let ident = &param.ident;
                quote! { #ident }
            }
        })
        .collect()
}

fn with_state_generics<'a>(
    original: &Generics,
    state_parameters: impl IntoIterator<Item = &'a syn::Ident>,
) -> Generics {
    let mut generics = original.clone();
    for param in &mut generics.params {
        match param {
            GenericParam::Type(param) => param.default = None,
            GenericParam::Const(param) => param.default = None,
            GenericParam::Lifetime(_) => {}
        }
    }
    for state_parameter in state_parameters {
        generics.params.push(syn::parse_quote!(#state_parameter));
    }
    generics
}

///
/// Generate a token stream representing a compilation error from tokens and message
///
fn to_compile_error<T, U>(tokens: T, message: U) -> TokenStream
where
    T: ToTokens,
    U: core::fmt::Display,
{
    Error::new_spanned(tokens, message).to_compile_error()
}

///
/// Convert path to string
///
fn path_to_string(path: &Path, separater: &str) -> String {
    path.segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>()
        .join(separater)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn field_error(data: &Data) -> &'static str {
        match fields(data) {
            Ok(_) => panic!("expected the input to be rejected"),
            Err(error) => error,
        }
    }

    #[test]
    fn fields_returns_named_non_empty_struct_fields() {
        let input: syn::DeriveInput = syn::parse_quote! {
            struct Example { first: u8, second: String }
        };

        let named = fields(&input.data).unwrap();

        assert_eq!(named.named.len(), 2);
        assert_eq!(named.named[0].ident.as_ref().unwrap(), "first");
        assert_eq!(named.named[1].ident.as_ref().unwrap(), "second");
    }

    #[test]
    fn fields_rejects_empty_unit_and_tuple_structs() {
        let empty: syn::DeriveInput = syn::parse_quote! { struct Empty {} };
        let unit: syn::DeriveInput = syn::parse_quote! { struct Unit; };
        let tuple: syn::DeriveInput = syn::parse_quote! { struct Tuple(u8); };

        assert_eq!(
            field_error(&empty.data),
            "structs with no fields are not allowed."
        );
        assert_eq!(
            field_error(&unit.data),
            "unit structs and tuple structs are not allowed."
        );
        assert_eq!(
            field_error(&tuple.data),
            "unit structs and tuple structs are not allowed."
        );
    }

    #[test]
    fn fields_rejects_enums_and_unions() {
        let enumeration: syn::DeriveInput = syn::parse_quote! { enum Example { A } };
        let union: syn::DeriveInput = syn::parse_quote! { union Example { value: u32 } };

        assert_eq!(
            field_error(&enumeration.data),
            "expected struct, found enum."
        );
        assert_eq!(field_error(&union.data), "expected struct, found union.");
    }

    #[test]
    fn path_to_string_joins_qualified_segments() {
        let path: Path = syn::parse_quote!(std::option::Option);

        assert_eq!(path_to_string(&path, "::"), "std::option::Option");
        assert_eq!(path_to_string(&path, "."), "std.option.Option");
    }

    #[test]
    fn to_compile_error_includes_the_requested_message() {
        let error = to_compile_error(quote!(field), "unsupported field");

        assert!(error.to_string().contains("unsupported field"));
    }
}
