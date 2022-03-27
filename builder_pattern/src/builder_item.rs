use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    Expr, Field, GenericArgument, Lit, Meta, MetaNameValue, NestedMeta, Path, PathArguments, Type,
    TypePath,
};

use crate::{path_to_string, to_compile_error};

pub(crate) struct BuilderItem<'a> {
    pub field_name: &'a Ident,
    pub method_name: Ident,
    pub each_method_name: Option<Ident>,
    pub ty: BuilderItemType<'a>,
    pub generics_ident: Ident,
    pub initial_expr: Option<InitialExpr>,
}
impl<'a> TryFrom<&'a Field> for BuilderItem<'a> {
    type Error = TokenStream;
    fn try_from(field: &'a Field) -> Result<Self, Self::Error> {
        let attr = field.attrs.iter().find(|attr| {
            if let Some(name) = attr.path.segments.last() {
                let name = &name.ident.to_string();
                name == "builder"
            } else {
                false
            }
        });

        let meta = attr
            .map(|attr| attr.parse_meta())
            .transpose()
            .map_err(|err| to_compile_error(attr, err))?;

        let builder_attr: Option<BuilderAttribute> = meta
            .as_ref()
            .map(|meta| BuilderAttribute::try_from(meta))
            .transpose()?;

        let field_name = field.ident.as_ref().unwrap();
        let generics_ident = format_ident!("{}", to_camel_case(&field_name.to_string()));

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
            .map(|(each, path)| match ty {
                BuilderItemType::Vec { .. } => Ok(each),
                _ => Err(to_compile_error(
                    path,
                    "'each' attribute is only allowed for Vec<T> fields.",
                )),
            })
            .transpose()?;

        let initial_expr = initial_expr
            .map(|(i, meta)| match ty {
                BuilderItemType::Flag | BuilderItemType::Option { .. } => Err(to_compile_error(
                    meta,
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

pub(crate) enum BuilderItemType<'a> {
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
impl<'a> TryFrom<&'a Type> for BuilderItemType<'a> {
    type Error = TokenStream;
    fn try_from(ty: &'a Type) -> Result<Self, Self::Error> {
        let path = if let Type::Path(TypePath { qself: None, path }) = ty {
            path
        } else {
            return Ok(BuilderItemType::AsIs(ty));
        };

        let last_seg = path.segments.iter().last().unwrap();
        let item_type = match path {
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

                    if let Some(allocator) = allocator {
                        return Err(to_compile_error(
                            allocator,
                            "Vec with custom allocator is not supported.",
                        ));
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
        };
        Ok(item_type)
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

pub enum InitialExpr {
    Default(Expr),
    Fixed(Expr),
}

struct BuilderAttribute<'a> {
    as_is_denoted: bool,
    name: Option<Ident>,
    each: Option<(Ident, &'a Path)>,
    initial_expr: Option<(InitialExpr, &'a Meta)>,
}

impl<'a> TryFrom<&'a Meta> for BuilderAttribute<'a> {
    type Error = TokenStream;
    fn try_from(meta: &'a Meta) -> Result<Self, Self::Error> {
        let list = match meta {
            Meta::Path(path) => {
                return Err(to_compile_error(path,
                    format!(
                    "expected 'builder = \"setter_name\"' or 'builder(name = \"setter_name\")', found '{}'.",
                    path.into_token_stream())
                ))
            }
            Meta::NameValue(MetaNameValue { lit, .. }) => {
                return if let Lit::Str(str) = lit {
                    Ok(BuilderAttribute {
                        as_is_denoted: false,
                        name: format_ident!("{}", str.value()).into(),
                        each: None,
                        initial_expr: None,
                    })
                } else {
                    Err(to_compile_error(
                        lit,
                        format!(
                            "expected 'builder = \"setter_name\"', found 'builder = {}'.",
                            lit.into_token_stream()
                        ),
                    ))
                }
            }
            Meta::List(list) => list,
        };

        let items = list
            .nested
            .iter()
            .map(|meta| match meta {
                NestedMeta::Lit(lit) => Err(to_compile_error(
                    lit,
                    format!(
                        "expected 'attr = value' format, found literal '{}'",
                        lit.to_token_stream()
                    ),
                )),
                NestedMeta::Meta(meta) => match meta {
                    Meta::NameValue(MetaNameValue { path, lit, .. }) => Ok((path, Some(lit), meta)),
                    Meta::Path(path) => Ok((path, None, meta)),
                    Meta::List(list) => Err(to_compile_error(
                        list,
                        format!(
                            "expected 'attr = value' format, found list '{}'",
                            list.into_token_stream()
                        ),
                    )),
                },
            })
            .collect::<Result<Vec<_>, _>>()?;

        let items = items.into_iter().try_fold(
            std::collections::HashMap::new(),
            |mut acc, (path, lit, meta)| {
                let name = path.segments.last().unwrap().ident.to_string();
                match name.as_str() {
                    _name @ ("as_is" | "name" | "each" | "default" | "fixed") => {
                        if let Some(_prev) = acc.get(&name) {
                            Err(to_compile_error(path, format!("'{name}' attribute can be specified at most once.")))
                        } else {
                            let _ = acc.insert(name, (path, lit, meta));
                            match (acc.get("default"), acc.get("fixed")) {
                                (Some(_),Some(_)) => Err(to_compile_error(path, "specifying both 'default' and 'fixed' attributes at the same time is not allowed")),
                                _ => Ok(acc),
                            }
                        }
                    }
                    _ => Err(to_compile_error(
                        path,
                        format!("expected 'as_is', 'name', 'each', 'default', or 'fixed',  found '{name}'."),
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
            .map(|x| expect_string_literal(x, "name", "setter_name"))
            .transpose()?
            .map(|(name, ..)| format_ident!("{name}"));

        let each = items
            .get("each")
            .map(|x| expect_string_literal(x, "each", "setter_name"))
            .transpose()?
            .map(|(each, path, ..)| (format_ident!("{each}"), path));

        let default = items
            .get("default")
            .map(|x| expect_string_literal(x, "default", "expression"))
            .transpose()?
            .map(|(expr, _, lit, meta)| parse_expr(&expr, lit).map(|e| (e, lit, meta)))
            .transpose()?;

        let fixed = items
            .get("fixed")
            .map(|x| expect_string_literal(x, "fixed", "expression"))
            .transpose()?
            .map(|(expr, _, lit, meta)| parse_expr(&expr, lit).map(|e| (e, lit, meta)))
            .transpose()?;
        let initial_expr = match (default, fixed) {
            (Some((expr, _, meta)), None) => (InitialExpr::Default(expr), meta).into(),
            (None, Some((expr, _, meta))) => (InitialExpr::Fixed(expr), meta).into(),
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

fn parse_expr<'a>(expr: &str, lit: &'a Lit) -> Result<Expr, TokenStream> {
    syn::parse_str(expr).map_err(|err| to_compile_error(lit, err))
}

fn expect_flag<'a>(
    (path, lit, meta): &(&'a Path, Option<&Lit>, &Meta),
    attr_name: &str,
) -> Result<&'a Path, TokenStream> {
    if let Some(lit) = lit {
        Err(to_compile_error(
            meta,
            format!(
                "expected '{attr_name}', found '{attr_name} = {}'.",
                lit.into_token_stream()
            ),
        ))
        .into()
    } else {
        Ok(path)
    }
}

fn expect_string_literal<'a>(
    (path, lit, meta): &(&'a Path, Option<&'a Lit>, &'a Meta),
    attr_name: &str,
    value_name: &str,
) -> Result<(String, &'a Path, &'a Lit, &'a Meta), TokenStream> {
    if let Some(lit) = lit {
        match lit {
            Lit::Str(str) => Ok((str.value(), path, *lit, meta)),
            _ => Err(to_compile_error(
                lit,
                format!(
                    "expected '\"{value_name}\"', found '{}'",
                    lit.into_token_stream()
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
    let path = path_to_string(path, "::");
    ["Option", "std::option::Option", "core::option::Option"]
        .into_iter()
        .find(|s| *s == path)
        .is_some()
}

fn is_vec(path: &Path) -> bool {
    let path = path_to_string(path, "::");
    ["Vec", "std::vec::Vec"]
        .into_iter()
        .find(|s| *s == path)
        .is_some()
}

fn is_bool(path: &Path) -> bool {
    let path = path_to_string(path, "::");
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
