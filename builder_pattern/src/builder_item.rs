use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    Error, Field, GenericArgument, Lit, Meta, MetaNameValue, NestedMeta, PathArguments, Type,
    TypePath,
};

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
        struct BuilderAttribute {
            as_is_denoted: bool,
            name: Option<String>,
            each: Option<String>,
        }

        let builder_attr: Option<BuilderAttribute> = if let Some(attr) = attr {
            let meta = attr
                .parse_meta()
                .map_err(|err| Error::new_spanned(attr, format!("{err}")).to_compile_error())?;
            match &meta {
                Meta::List(list) => {
                    let items = list
                        .nested
                        .iter()
                        .map(|meta| match meta {
                            NestedMeta::Lit(lit) => Err(Error::new_spanned(
                                lit,
                                format!(
                                    "expected 'attr = value' format, found literal '{}'",
                                    lit.to_token_stream()
                                ),
                            )
                            .to_compile_error()),
                            NestedMeta::Meta(meta) => match meta {
                                Meta::NameValue(MetaNameValue { path, lit, .. }) => {
                                    Ok((path, Some(lit)))
                                }
                                Meta::Path(path) => Ok((path, None)),
                                Meta::List(list) => Err(Error::new_spanned(
                                    list,
                                    format!(
                                        "expected 'attr = value' format, found list '{}'",
                                        list.into_token_stream()
                                    ),
                                )
                                .to_compile_error()),
                            },
                        })
                        .collect::<Result<Vec<_>, _>>()?;

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
                                _ => Err(Error::new_spanned(
                                    path,
                                    format!("expected 'as_is', 'name' or 'each', found '{name}'."),
                                )
                                .to_compile_error()),
                            }
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let as_is_denoted = items
                        .iter()
                        .filter_map(|(item_type, _path, lit)| match item_type {
                            ItemType::AsIs => {
                                if let Some(lit) = lit {
                                    Err(Error::new_spanned(
                                        lit,
                                        format!(
                                            "expected 'as_is', found 'as_is = {}'.",
                                            lit.into_token_stream()
                                        ),
                                    )
                                    .to_compile_error())
                                    .into()
                                } else {
                                    Ok(()).into()
                                }
                            }
                            _ => None,
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let as_is_denoted = as_is_denoted.into_iter().any(|_| true);

                    let name = items
                        .iter()
                        .filter_map(|(item_type, path, lit)| match item_type {
                            ItemType::Name => {
                                if let Some(lit) = lit {
                                    match lit {
                                        Lit::Str(str) => Ok(str.value()).into(),
                                        _ => Err(Error::new_spanned(
                                            lit,
                                            format!(
                                                "expected '\"setter_name\"', found '{}'",
                                                lit.into_token_stream()
                                            ),
                                        )
                                        .to_compile_error())
                                        .into(),
                                    }
                                } else {
                                    Err(Error::new_spanned(
                                        path,
                                        format!("expected 'name = \"setter_name\"', found 'name'",),
                                    )
                                    .to_compile_error())
                                    .into()
                                }
                            }
                            _ => None,
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let name = name.into_iter().last().map(|s| s);

                    let each = items
                                .iter()
                                .filter_map(|(item_type, path, lit)| {
                                    match item_type {
                                        ItemType::Each => {
                                            if let Some(lit) = lit {
                                                match lit {
                                                    Lit::Str(str) => Ok(str.value()).into(),
                                                    _ => Err(Error::new_spanned(lit, format!(
                                                        "expected 'each = \"setter_name\"', found 'each = {}'",
                                                        lit.into_token_stream()
                                                    )).to_compile_error()).into(),
                                                }
                                            } else {
                                                Err(Error::new_spanned(path, format!(
                                                    "expected 'each = \"setter_name\"', found 'each'",
                                                )).to_compile_error()).into()
                                            }
                                        }
                                        _ => None,
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                    let each = each.into_iter().last().map(|s| s);

                    BuilderAttribute {
                        as_is_denoted,
                        name,
                        each,
                    }
                    .into()
                }
                Meta::Path(path) => {
                    return Err(Error::new_spanned(path,
                                format!(
                                "expected 'builder = \"setter_name\"' or 'builder(name = \"setter_name\")', found '{}'.",
                                path.into_token_stream())
                            ).to_compile_error());
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
                        return Err(Error::new_spanned(
                            lit,
                            format!(
                                "expected 'builder = \"setter_name\"', found 'builder = {}'.",
                                lit.into_token_stream()
                            ),
                        )
                        .to_compile_error());
                    }
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
            BuilderItemType::try_from(&field.ty)?
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
                        return Err(Error::new_spanned(
                            allocator,
                            "Vec with custom allocator is not suppoted.",
                        )
                        .to_compile_error());
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
