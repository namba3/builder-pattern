use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Expr, ExprLit, Field, GenericArgument, Lit, Meta, MetaNameValue, Path, PathArguments, Type,
    TypePath, punctuated::Punctuated, token::Comma,
};

use super::{path_matches, to_compile_error};

pub(super) struct BuilderItem<'a> {
    pub(super) field_name: &'a Ident,
    pub(super) method_name: Ident,
    pub(super) each_method_name: Option<Ident>,
    pub(super) ty: BuilderItemType<'a>,
    pub(super) generics_ident: Ident,
    pub(super) initial_expr: Option<InitialExpr>,
}
impl<'a> TryFrom<&'a Field> for BuilderItem<'a> {
    type Error = TokenStream;
    fn try_from(field: &'a Field) -> Result<Self, Self::Error> {
        let mut builder_attrs = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("builder"));
        let attr = builder_attrs.next();
        if builder_attrs.next().is_some() {
            return Err(to_compile_error(
                field,
                "only one #[builder(...)] attribute is allowed per field.",
            ));
        }

        let meta = attr.map(|attr| &attr.meta);

        let builder_attr: Option<BuilderAttribute> =
            meta.map(BuilderAttribute::try_from).transpose()?;
        if matches!(
            builder_attr.as_ref(),
            Some(BuilderAttribute {
                name: Some(_),
                initial_expr: Some((InitialExpr::Fixed(_), _)),
                ..
            })
        ) {
            return Err(to_compile_error(
                field,
                "'name' cannot be used with 'fixed' because fixed fields do not have setters.",
            ));
        }
        if matches!(
            builder_attr.as_ref(),
            Some(BuilderAttribute {
                each: Some(_),
                initial_expr: Some((InitialExpr::Fixed(_), _)),
                ..
            })
        ) {
            return Err(to_compile_error(
                field,
                "'each' cannot be used with 'fixed' because fixed fields do not have setters.",
            ));
        }
        if matches!(
            builder_attr.as_ref(),
            Some(BuilderAttribute {
                each: Some(_),
                initial_expr: Some((InitialExpr::Default(_), _)),
                ..
            })
        ) {
            return Err(to_compile_error(
                field,
                "'each' cannot be used with 'default' because 'default' requires 'as_is', which disables the Vec behavior required by 'each'.",
            ));
        }
        if matches!(
            builder_attr.as_ref(),
            Some(BuilderAttribute {
                as_is_denoted: true,
                each: Some(_),
                ..
            })
        ) {
            return Err(to_compile_error(
                field,
                "'each' cannot be used with 'as_is'; 'as_is' disables the Vec behavior required by 'each'.",
            ));
        }

        let field_name = field
            .ident
            .as_ref()
            .ok_or_else(|| to_compile_error(field, "Builder only supports named fields."))?;
        let generics_ident = format_ident!("__BuilderState");

        let ty = if let Some(BuilderAttribute {
            as_is_denoted: true,
            ..
        }) = builder_attr
        {
            BuilderItemType::AsIs(&field.ty)
        } else {
            BuilderItemType::try_from(&field.ty)?
        };

        let (name, each, initial_expr) = if let Some(BuilderAttribute {
            name,
            each,
            initial_expr,
            ..
        }) = builder_attr
        {
            (name, each, initial_expr)
        } else {
            (None, None, None)
        };

        let method_name = name.unwrap_or_else(|| field_name.clone());

        let each_method_name = each
            .map(|each| match ty {
                BuilderItemType::Vec { .. } => Ok(each),
                _ => Err(to_compile_error(
                    &each,
                    "'each' attribute is only allowed for Vec<T> fields.",
                )),
            })
            .transpose()?;

        let initial_expr = initial_expr
            .map(|(i, attr_name)| match ty {
                BuilderItemType::Flag
                | BuilderItemType::Option { .. }
                | BuilderItemType::Vec { .. } => Err(to_compile_error(
                    &attr_name,
                    "'as_is' attribute is required to specify 'default' or 'fixed' attribute for bool, Option and Vec<T> fields",
                )),
                _ => Ok(i),
            })
            .transpose()?;

        Ok(Self {
            field_name,
            method_name,
            each_method_name,
            ty,
            generics_ident,
            initial_expr,
        })
    }
}

pub(super) enum BuilderItemType<'a> {
    Flag,
    Option { inner_type: &'a GenericArgument },
    Vec { inner_type: &'a GenericArgument },
    AsIs(&'a Type),
}
impl<'a> TryFrom<&'a Type> for BuilderItemType<'a> {
    type Error = TokenStream;
    fn try_from(ty: &'a Type) -> Result<Self, Self::Error> {
        let path = if let Type::Path(TypePath {
            qself: None, path, ..
        }) = ty
        {
            path
        } else {
            return Ok(BuilderItemType::AsIs(ty));
        };

        let Some(last_seg) = path.segments.last() else {
            return Ok(BuilderItemType::AsIs(ty));
        };
        let item_type = match path {
            maybe_bool if is_bool(maybe_bool) => BuilderItemType::Flag,
            maybe_opt if is_option(maybe_opt) => {
                if let PathArguments::AngleBracketed(a) = &last_seg.arguments {
                    BuilderItemType::Option {
                        inner_type: single_type_argument(a, ty, "Option")?,
                    }
                } else {
                    BuilderItemType::AsIs(ty)
                }
            }
            maybe_vec if is_vec(maybe_vec) => {
                if let PathArguments::AngleBracketed(a) = &last_seg.arguments {
                    if let Some(allocator) = a.args.iter().nth(1) {
                        return Err(to_compile_error(
                            allocator,
                            "Vec with custom allocator is not supported.",
                        ));
                    }

                    BuilderItemType::Vec {
                        inner_type: single_type_argument(a, ty, "Vec")?,
                    }
                } else {
                    BuilderItemType::AsIs(ty)
                }
            }
            _other => BuilderItemType::AsIs(ty),
        };
        Ok(item_type)
    }
}

fn single_type_argument<'a>(
    arguments: &'a syn::AngleBracketedGenericArguments,
    ty: &Type,
    type_name: &str,
) -> Result<&'a GenericArgument, TokenStream> {
    if arguments.args.len() == 1 {
        if let Some(argument @ GenericArgument::Type(_)) = arguments.args.first() {
            return Ok(argument);
        }
    }

    Err(to_compile_error(
        ty,
        format!("expected {type_name}<T> with exactly one type argument."),
    ))
}
impl<'a> ToTokens for BuilderItemType<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ts = match self {
            BuilderItemType::Flag => quote! { ::core::primitive::bool },
            BuilderItemType::Option { inner_type } => {
                quote! { ::core::option::Option< #inner_type >}
            }
            BuilderItemType::Vec { inner_type } => quote! { ::std::vec::Vec< #inner_type >},
            BuilderItemType::AsIs(ty) => quote! { #ty },
        };
        tokens.extend(ts);
    }
}

pub(super) enum InitialExpr {
    Default(Expr),
    Fixed(Expr),
}

struct BuilderAttribute {
    as_is_denoted: bool,
    name: Option<Ident>,
    each: Option<Ident>,
    initial_expr: Option<(InitialExpr, Ident)>,
}

impl TryFrom<&Meta> for BuilderAttribute {
    type Error = TokenStream;
    fn try_from(meta: &Meta) -> Result<Self, Self::Error> {
        let list = match meta {
            Meta::Path(path) => {
                return Err(to_compile_error(
                    path,
                    format!(
                        "expected 'builder = \"setter_name\"' or 'builder(name = \"setter_name\")', found '{}'.",
                        path.into_token_stream()
                    ),
                ));
            }
            Meta::NameValue(MetaNameValue { value, .. }) => {
                return if let Expr::Lit(ExprLit {
                    lit: Lit::Str(str), ..
                }) = value
                {
                    Ok(BuilderAttribute {
                        as_is_denoted: false,
                        name: Some(parse_method_name(&str.value(), value, "name")?),
                        each: None,
                        initial_expr: None,
                    })
                } else {
                    Err(to_compile_error(
                        value,
                        format!(
                            "expected 'builder = \"setter_name\"', found 'builder = {}'.",
                            value.into_token_stream()
                        ),
                    ))
                };
            }
            Meta::List(list) => list,
        };

        let nested = list
            .parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)
            .map_err(|err| to_compile_error(list, err))?;
        if nested.is_empty() {
            return Err(to_compile_error(
                list,
                "expected at least one builder attribute inside #[builder(...)].",
            ));
        }
        let items = nested
            .iter()
            .map(|meta| match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) => Ok((path, Some(value), meta)),
                Meta::Path(path) => Ok((path, None, meta)),
                Meta::List(list) => Err(to_compile_error(
                    list,
                    format!(
                        "expected 'attr = value' format, found list '{}'",
                        list.into_token_stream()
                    ),
                )),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let items = items.into_iter().try_fold(
            std::collections::HashMap::new(),
            |mut acc, (path, value, meta)| {
                if path.leading_colon.is_some() || path.segments.len() != 1 {
                    return Err(to_compile_error(
                        path,
                        "builder setting names must be unqualified.",
                    ));
                }
                let Some(last_segment) = path.segments.last() else {
                    return Err(to_compile_error(path, "expected an attribute name."));
                };
                let name = last_segment.ident.to_string();
                match name.as_str() {
                    _name @ ("as_is" | "name" | "each" | "default" | "fixed") => {
                        if let Some(_prev) = acc.get(&name) {
                            Err(to_compile_error(path, format!("'{name}' attribute can be specified at most once.")))
                        } else {
                            let _ = acc.insert(name, (path, value, meta));
                            match (acc.get("default"), acc.get("fixed")) {
                                (Some(_),Some(_)) => Err(to_compile_error(path, "specifying both 'default' and 'fixed' attributes at the same time is not allowed")),
                                _ => Ok(acc),
                            }
                        }
                    }
                    _ => Err(to_compile_error(
                        path,
                        format!("expected 'as_is', 'name', 'each', 'default', or 'fixed', found '{name}'."),
                    )),
                }
            },
        )?;

        let as_is_denoted = items
            .get("as_is")
            .map(|x| expect_flag(x, "as_is"))
            .transpose()?
            .is_some();

        let name = items
            .get("name")
            .map(|x| {
                let (name, _, value, _) = expect_string_literal(x, "name", "setter_name")?;
                parse_method_name(&name, value, "name")
            })
            .transpose()?;

        let each = items
            .get("each")
            .map(|x| {
                let (name, _, value, _) = expect_string_literal(x, "each", "setter_name")?;
                parse_method_name(&name, value, "each")
            })
            .transpose()?;

        let default = items
            .get("default")
            .map(|x| expect_expression(x, "default"))
            .transpose()?
            .map(|(expr, path)| {
                let Some(attr_name) = path.segments.last().map(|segment| segment.ident.clone())
                else {
                    return Err(to_compile_error(path, "expected an attribute name."));
                };
                Ok((expr, attr_name))
            })
            .transpose()?;

        let fixed = items
            .get("fixed")
            .map(|x| expect_expression(x, "fixed"))
            .transpose()?
            .map(|(expr, path)| {
                let Some(attr_name) = path.segments.last().map(|segment| segment.ident.clone())
                else {
                    return Err(to_compile_error(path, "expected an attribute name."));
                };
                Ok((expr, attr_name))
            })
            .transpose()?;
        let initial_expr = match (default, fixed) {
            (Some((expr, attr_name)), None) => (InitialExpr::Default(expr), attr_name).into(),
            (None, Some((expr, attr_name))) => (InitialExpr::Fixed(expr), attr_name).into(),
            _ => None,
        };

        Ok(BuilderAttribute {
            as_is_denoted,
            name,
            each,
            initial_expr,
        })
    }
}

fn expect_expression<'a>(
    (path, value, meta): &(&'a Path, Option<&'a Expr>, &'a Meta),
    attr_name: &str,
) -> Result<(Expr, &'a Path), TokenStream> {
    if let Some(value) = value {
        let expression = match value {
            // Preserve the original string-based syntax, where the string contents are parsed as
            // Rust expression tokens. A string literal expression can be written in a block to
            // distinguish it from this legacy form, e.g. `default = { let s = "text"; s }`.
            Expr::Lit(ExprLit {
                lit: Lit::Str(str), ..
            }) => {
                syn::parse_str::<Expr>(&str.value()).map_err(|err| to_compile_error(value, err))?
            }
            expression => (*expression).clone(),
        };
        Ok((expression, path))
    } else {
        Err(to_compile_error(
            meta,
            format!("expected '{attr_name} = <expression>', found '{attr_name}'"),
        ))
    }
}

fn parse_method_name(name: &str, value: &Expr, attribute: &str) -> Result<Ident, TokenStream> {
    syn::parse_str::<syn::ItemFn>(&format!("fn {name}() {{}}"))
        .map(|function| function.sig.ident)
        .map_err(|_| {
            to_compile_error(
                value,
                format!("expected a valid Rust method name for '{attribute}', found '{name}'."),
            )
        })
}

fn expect_flag<'a>(
    (path, value, meta): &(&'a Path, Option<&Expr>, &Meta),
    attr_name: &str,
) -> Result<&'a Path, TokenStream> {
    if let Some(value) = value {
        Err(to_compile_error(
            meta,
            format!(
                "expected '{attr_name}', found '{attr_name} = {}'.",
                value.into_token_stream()
            ),
        ))
    } else {
        Ok(path)
    }
}

fn expect_string_literal<'a>(
    (path, value, meta): &(&'a Path, Option<&'a Expr>, &'a Meta),
    attr_name: &str,
    value_name: &str,
) -> Result<(String, &'a Path, &'a Expr, &'a Meta), TokenStream> {
    if let Some(value) = value {
        match value {
            Expr::Lit(ExprLit {
                lit: Lit::Str(str), ..
            }) => Ok((str.value(), path, value, meta)),
            _ => Err(to_compile_error(
                value,
                format!(
                    "expected '\"{value_name}\"', found '{}'",
                    value.into_token_stream()
                ),
            )),
        }
    } else {
        Err(to_compile_error(
            path,
            format!("expected '{attr_name} = \"{value_name}\"', found '{attr_name}'",),
        ))
    }
}

fn is_option(path: &Path) -> bool {
    path_matches(path, &["Option"])
        || path_matches(path, &["std", "option", "Option"])
        || path_matches(path, &["core", "option", "Option"])
}

fn is_vec(path: &Path) -> bool {
    path_matches(path, &["Vec"]) || path_matches(path, &["std", "vec", "Vec"])
}

fn is_bool(path: &Path) -> bool {
    path_matches(path, &["bool"])
        || path_matches(path, &["core", "primitive", "bool"])
        || path_matches(path, &["std", "primitive", "bool"])
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::Fields;

    fn item<'a>(field: &'a Field) -> Result<BuilderItem<'a>, TokenStream> {
        BuilderItem::try_from(field)
    }

    fn parse_field(tokens: TokenStream) -> Field {
        let item: syn::ItemStruct = syn::parse2(quote! { struct Holder { #tokens } }).unwrap();
        match item.fields {
            Fields::Named(fields) => fields.named.into_iter().next().unwrap(),
            _ => unreachable!("the test helper always parses named fields"),
        }
    }

    fn error(field: &Field) -> TokenStream {
        match item(field) {
            Ok(_) => panic!("expected field parsing to fail"),
            Err(error) => error,
        }
    }

    fn error_message(error: TokenStream) -> String {
        error.to_string()
    }

    #[test]
    fn recognizes_special_field_types_and_leaves_other_types_unchanged() {
        let bool_field = parse_field(quote!(enabled: bool));
        let option_field = parse_field(quote!(value: Option<u32>));
        let vec_field = parse_field(quote!(values: Vec<String>));
        let custom_field = parse_field(quote!(value: crate::MyType));
        let option_alias_field = parse_field(quote!(value: Maybe<u32>));
        let vec_alias_field = parse_field(quote!(values: Items<String>));

        assert!(matches!(
            item(&bool_field).unwrap().ty,
            BuilderItemType::Flag
        ));
        assert!(matches!(
            item(&option_field).unwrap().ty,
            BuilderItemType::Option { .. }
        ));
        assert!(matches!(
            item(&vec_field).unwrap().ty,
            BuilderItemType::Vec { .. }
        ));
        assert!(matches!(
            item(&custom_field).unwrap().ty,
            BuilderItemType::AsIs(_)
        ));
        assert!(matches!(
            item(&option_alias_field).unwrap().ty,
            BuilderItemType::AsIs(_)
        ));
        assert!(matches!(
            item(&vec_alias_field).unwrap().ty,
            BuilderItemType::AsIs(_)
        ));
    }

    #[test]
    fn recognizes_qualified_special_field_types() {
        let bool_field = parse_field(quote!(enabled: core::primitive::bool));
        let std_bool_field = parse_field(quote!(enabled: std::primitive::bool));
        let option_field = parse_field(quote!(value: std::option::Option<u32>));
        let core_option_field = parse_field(quote!(value: core::option::Option<u32>));
        let vec_field = parse_field(quote!(values: std::vec::Vec<String>));

        assert!(matches!(
            item(&bool_field).unwrap().ty,
            BuilderItemType::Flag
        ));
        assert!(matches!(
            item(&std_bool_field).unwrap().ty,
            BuilderItemType::Flag
        ));
        assert!(matches!(
            item(&option_field).unwrap().ty,
            BuilderItemType::Option { .. }
        ));
        assert!(matches!(
            item(&core_option_field).unwrap().ty,
            BuilderItemType::Option { .. }
        ));
        assert!(matches!(
            item(&vec_field).unwrap().ty,
            BuilderItemType::Vec { .. }
        ));
    }

    #[test]
    fn builder_attributes_override_setter_name_and_define_each_setter() {
        let field = parse_field(quote! {
            #[builder(name = "set_values", each = "value")]
            values: Vec<u8>
        });

        let item = item(&field).unwrap();

        assert_eq!(item.method_name, "set_values");
        assert_eq!(item.each_method_name.unwrap(), "value");
        assert_eq!(item.field_name, "values");
    }

    #[test]
    fn ignores_namespaced_builder_attributes() {
        let field = parse_field(quote! {
            #[tool::builder(name = "set_value")]
            value: u8
        });

        assert_eq!(item(&field).unwrap().method_name, "value");
    }

    #[test]
    fn builder_default_and_fixed_attributes_parse_expressions() {
        let default_field = parse_field(quote! {
            #[builder(default = "2 + 2")]
            value: u32
        });
        let fixed_field = parse_field(quote! {
            #[builder(fixed = "2 + 2")]
            value: u32
        });
        let native_default_field = parse_field(quote! {
            #[builder(default = 2 + 2)]
            value: u32
        });
        let native_fixed_field = parse_field(quote! {
            #[builder(fixed = 2 + 2)]
            value: u32
        });

        assert!(matches!(
            item(&default_field).unwrap().initial_expr,
            Some(InitialExpr::Default(_))
        ));
        assert!(matches!(
            item(&fixed_field).unwrap().initial_expr,
            Some(InitialExpr::Fixed(_))
        ));
        assert!(matches!(
            item(&native_default_field).unwrap().initial_expr,
            Some(InitialExpr::Default(_))
        ));
        assert!(matches!(
            item(&native_fixed_field).unwrap().initial_expr,
            Some(InitialExpr::Fixed(_))
        ));
    }

    #[test]
    fn as_is_keeps_special_types_as_regular_fields() {
        let field = parse_field(quote! {
            #[builder(as_is)]
            enabled: bool
        });

        assert!(matches!(item(&field).unwrap().ty, BuilderItemType::AsIs(_)));
    }

    #[test]
    fn each_is_rejected_for_non_vec_fields() {
        let field = parse_field(quote! {
            #[builder(each = "value")]
            value: u8
        });

        let error = error(&field);

        assert!(
            error_message(error).contains("'each' attribute is only allowed for Vec<T> fields.")
        );
    }

    #[test]
    fn default_and_fixed_require_as_is_on_special_fields() {
        let default_field = parse_field(quote! {
            #[builder(default = "true")]
            enabled: bool
        });
        let fixed_field = parse_field(quote! {
            #[builder(fixed = "1")]
            value: Option<u8>
        });
        let default_vec_field = parse_field(quote! {
            #[builder(default = "vec![]")]
            values: Vec<u8>
        });
        let fixed_vec_field = parse_field(quote! {
            #[builder(fixed = "vec![]")]
            values: Vec<u8>
        });

        assert!(error_message(error(&default_field)).contains("'as_is' attribute is required"));
        assert!(error_message(error(&fixed_field)).contains("'as_is' attribute is required"));
        assert!(error_message(error(&default_vec_field)).contains("'as_is' attribute is required"));
        assert!(error_message(error(&fixed_vec_field)).contains("'as_is' attribute is required"));
    }

    #[test]
    fn duplicate_and_conflicting_attributes_are_rejected() {
        let duplicate = parse_field(quote! {
            #[builder(name = "first", name = "second")]
            value: u8
        });
        let conflicting = parse_field(quote! {
            #[builder(default = "1", fixed = "2")]
            value: u8
        });

        assert!(
            error_message(error(&duplicate))
                .contains("'name' attribute can be specified at most once.")
        );
        assert!(error_message(error(&conflicting)).contains(
            "specifying both 'default' and 'fixed' attributes at the same time is not allowed"
        ));
    }

    #[test]
    fn repeated_builder_attributes_are_rejected() {
        let field = parse_field(quote! {
            #[builder(name = "first")]
            #[builder(name = "second")]
            value: u8
        });

        assert!(
            error_message(error(&field))
                .contains("only one #[builder(...)] attribute is allowed per field.")
        );
    }

    #[test]
    fn invalid_method_names_are_reported_as_attribute_errors() {
        let invalid_name = parse_field(quote! {
            #[builder(name = "not a method")]
            value: u8
        });
        let invalid_each = parse_field(quote! {
            #[builder(each = "not-a-method")]
            values: Vec<u8>
        });
        let invalid_legacy_name = parse_field(quote! {
            #[builder = "not a method"]
            value: u8
        });
        let keyword_name = parse_field(quote! {
            #[builder(name = "type")]
            value: u8
        });

        assert!(
            error_message(error(&invalid_name))
                .contains("expected a valid Rust method name for 'name'")
        );
        assert!(
            error_message(error(&invalid_each))
                .contains("expected a valid Rust method name for 'each'")
        );
        assert!(
            error_message(error(&invalid_legacy_name))
                .contains("expected a valid Rust method name for 'name'")
        );
        assert!(
            error_message(error(&keyword_name))
                .contains("expected a valid Rust method name for 'name'")
        );
    }

    #[test]
    fn unknown_attributes_and_custom_vec_allocators_are_rejected() {
        let unknown = parse_field(quote! {
            #[builder(unknown)]
            value: u8
        });
        let allocator = parse_field(quote!(values: Vec<u8, CustomAllocator>));

        assert!(
            error_message(error(&unknown)).contains(
                "expected 'as_is', 'name', 'each', 'default', or 'fixed', found 'unknown'."
            )
        );
        assert!(
            error_message(error(&allocator))
                .contains("Vec with custom allocator is not supported.")
        );
    }

    #[test]
    fn special_types_with_invalid_generic_arguments_return_errors() {
        let empty_option = parse_field(quote!(value: Option<>));
        let multiple_option_arguments = parse_field(quote!(value: Option<u8, u16>));
        let non_type_option_argument = parse_field(quote!(value: Option<'static>));
        let empty_vec = parse_field(quote!(values: Vec<>));

        for (field, type_name) in [
            (&empty_option, "Option"),
            (&multiple_option_arguments, "Option"),
            (&non_type_option_argument, "Option"),
            (&empty_vec, "Vec"),
        ] {
            let message = error_message(error(field));
            assert!(
                message.contains(&format!(
                    "expected {type_name}<T> with exactly one type argument."
                )),
                "unexpected diagnostic for {type_name}: {message}"
            );
        }
    }
}
