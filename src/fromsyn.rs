use super::*;

impl StructKind {
    pub(crate) fn from_syn(f: &syn::Fields, semi_token: Option<syn::token::Semi>) -> Self {
        match f {
            syn::Fields::Named(n) => Self::Struct(n.brace_token),
            syn::Fields::Unnamed(u) => Self::Tuple(u.paren_token),
            syn::Fields::Unit => Self::Unit(semi_token.unwrap()),
        }
    }
}

impl VariantKind {
    pub(crate) fn from_syn(f: &syn::Fields, span: proc_macro2::Span) -> Self {
        match f {
            syn::Fields::Named(n) => Self::Struct(n.brace_token),
            syn::Fields::Unnamed(u) => Self::Tuple(u.paren_token),
            syn::Fields::Unit => Self::Unit(span),
        }
    }
}

impl DeriveInput {
    pub(crate) fn from_syn(input: syn::DeriveInput) -> Self {
        let syn::DeriveInput {
            attrs,
            vis,
            ident,
            generics,
            data,
        } = input;
        match data {
            syn::Data::Struct(syn::DataStruct {
                struct_token,
                fields,
                semi_token,
            }) => Self::Struct(Struct {
                attrs: Attributes::from_syn(attrs),
                vis,
                struct_token,
                name: ident,
                generics,
                kind: StructKind::from_syn(&fields, semi_token),
                fields: Field::from_syn(fields),
            }),
            syn::Data::Enum(syn::DataEnum {
                enum_token,
                brace_token: _,
                variants,
            }) => Self::Enum(Enum {
                attrs: Attributes::from_syn(attrs),
                vis,
                enum_token,
                name: ident,
                generics,
                variants: variants
                    .into_pairs()
                    .map(|p| p.into_tuple())
                    .map(
                        |(
                            syn::Variant {
                                attrs,
                                ident,
                                fields,
                                discriminant,
                            },
                            comma,
                        )| Variant {
                            attrs: Attributes::from_syn(attrs),
                            kind: VariantKind::from_syn(
                                &fields,
                                comma.map_or_else(|| ident.span(), |c| c.spans[0]),
                            ),
                            name: ident,
                            fields: Field::from_syn(fields),
                            discriminant: discriminant.map(|(_eq, expr)| expr),
                        },
                    )
                    .collect(),
            }),
            syn::Data::Union(syn::DataUnion {
                union_token,
                fields,
            }) => Self::Union(Union {
                attrs: Attributes::from_syn(attrs),
                vis,
                union_token,
                name: ident,
                generics,
                fields: Field::from_syn(syn::Fields::Named(fields)),
            }),
        }
    }
}

impl Field {
    pub(crate) fn from_syn(f: syn::Fields) -> Vec<Self> {
        f.into_iter()
            .enumerate()
            .map(
                |(
                    i,
                    syn::Field {
                        attrs,
                        vis,
                        ident,
                        colon_token: _,
                        ty,
                    },
                )| {
                    Self {
                        attrs: Attributes::from_syn(attrs),
                        vis,
                        name: ident.map_or_else(
                            || {
                                syn::Member::Unnamed(syn::Index {
                                    index: i as u32,
                                    span: proc_macro2::Span::call_site(),
                                })
                            },
                            syn::Member::Named,
                        ),
                        ty,
                    }
                },
            )
            .collect()
    }
}

impl Attributes {
    pub(crate) fn from_syn(a: Vec<syn::Attribute>) -> Self {
        Self {
            attrs: a.into_iter().map(|attr| Attribute { attr }).collect(),
        }
    }
}
