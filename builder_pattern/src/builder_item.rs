use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    Field, GenericArgument, Lit, Meta, MetaNameValue, NestedMeta, Path, PathArguments, Type,
    TypePath,
};

use crate::to_compile_error;

pub(crate) struct BuilderItem<'a> {
    pub field_name: &'a Ident,
    pub method_name: Ident,
    pub each_method_name: Option<Ident>,
    pub ty: BuilderItemType<'a>,
    pub generics_ident: Ident,
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

        let method_name = builder_attr
            .as_ref()
            .and_then(|attr| attr.name.as_ref())
            .map(|name| format_ident!("{name}"))
            .unwrap_or_else(|| field_name.clone());

        let each = builder_attr
            .as_ref()
            .and_then(|attr| attr.each.as_ref())
            .map(|(each, path)| (format_ident!("{each}"), path));
        let each_method_name = each
            .map(|(each, path)| match ty {
                BuilderItemType::Vec { .. } => Ok(each),
                _ => Err(to_compile_error(
                    path,
                    "'each' attribute is only allowed for Vec<T> fields.",
                )),
            })
            .transpose()?;

        Ok(Self {
            field_name,
            method_name,
            each_method_name,
            ty,
            generics_ident,
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

        let path_str: String = path
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect::<Vec<String>>()
            .join("::");

        let last_seg = path.segments.iter().last().unwrap();
        let item_type = match path_str {
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

struct BuilderAttribute<'a> {
    as_is_denoted: bool,
    name: Option<String>,
    each: Option<(String, &'a Path)>,
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
                        name: str.value().into(),
                        each: None,
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
                    Meta::NameValue(MetaNameValue { path, lit, .. }) => Ok((path, Some(lit))),
                    Meta::Path(path) => Ok((path, None)),
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

        #[derive(PartialEq, PartialOrd)]
        enum ItemType {
            AsIs,
            Name,
            Each,
        }
        let items = items
            .into_iter()
            .map(|(path, lit)| {
                let name = path.segments.last().unwrap().ident.to_string();
                match name.as_str() {
                    "as_is" => Ok((ItemType::AsIs, path, lit)).into(),
                    "name" => Ok((ItemType::Name, path, lit)).into(),
                    "each" => Ok((ItemType::Each, path, lit)).into(),
                    _ => Err(to_compile_error(
                        path,
                        format!("expected 'as_is', 'name' or 'each', found '{name}'."),
                    )),
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let as_is_list = items
            .iter()
            .filter_map(|(item_type, path, lit)| match item_type {
                ItemType::AsIs => {
                    if let Some(lit) = lit {
                        Err(to_compile_error(
                            lit,
                            format!(
                                "expected 'as_is', found 'as_is = {}'.",
                                lit.into_token_stream()
                            ),
                        ))
                        .into()
                    } else {
                        Ok(*path).into()
                    }
                }
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut as_is_iter = as_is_list.into_iter();
        let as_is_denoted = as_is_iter.next().is_some();
        if let Some(path) = as_is_iter.next() {
            return Err(to_compile_error(
                path,
                "'as_is' attribute is only allowed  once.",
            ));
        }

        let name_list = items
            .iter()
            .filter_map(|(item_type, path, lit)| match item_type {
                ItemType::Name => {
                    if let Some(lit) = lit {
                        match lit {
                            Lit::Str(str) => Ok((str.value(), *path)).into(),
                            _ => Err(to_compile_error(
                                lit,
                                format!(
                                    "expected '\"setter_name\"', found '{}'",
                                    lit.into_token_stream()
                                ),
                            ))
                            .into(),
                        }
                    } else {
                        Err(to_compile_error(
                            path,
                            format!("expected 'name = \"setter_name\"', found 'name'",),
                        ))
                        .into()
                    }
                }
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut name_iter = name_list.into_iter();
        let name = name_iter.next().map(|(name, _)| name);
        if let Some((_, path)) = name_iter.next() {
            return Err(to_compile_error(
                path,
                "'name' attribute is only allowed once.",
            ));
        }

        let each_list = items
            .iter()
            .filter_map(|(item_type, path, lit)| match item_type {
                ItemType::Each => {
                    if let Some(lit) = lit {
                        match lit {
                            Lit::Str(str) => Ok((str.value(), *path)).into(),
                            _ => Err(to_compile_error(
                                lit,
                                format!(
                                    "expected 'each = \"setter_name\"', found 'each = {}'",
                                    lit.into_token_stream()
                                ),
                            ))
                            .into(),
                        }
                    } else {
                        Err(to_compile_error(
                            path,
                            format!("expected 'each = \"setter_name\"', found 'each'",),
                        ))
                        .into()
                    }
                }
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut each_iter = each_list.into_iter();
        let each = each_iter.next();
        if let Some((_, path)) = each_iter.next() {
            return Err(to_compile_error(
                path,
                "'each' attribute is only allowed once.",
            ));
        }

        Ok(BuilderAttribute {
            as_is_denoted,
            name,
            each,
        })
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
