extern crate proc_macro;

mod fromsyn;

use quote::{quote, ToTokens};

#[macro_export]
macro_rules! make_derive_macro {
    ($(#[doc=$doc:literal])* $name:ident, $fn:path $(,)?) => {
        $crate::make_derive_macro! { $(#[doc=$doc])* $name(), $fn }
    };
    ($(#[doc=$doc:literal])* $name:ident($($attr:ident),*$(,)?), $fn:path $(,)?) => {
        $(#[doc=$doc])*
        #[proc_macro_derive($name, attributes($($attr),*))]
        #[allow(non_snake_case)]
        pub fn $name(input: proc_macro::TokenStream)
            -> proc_macro::TokenStream
        {
            match $crate::parse_derive_input(input).and_then($fn) {
                Ok(t) => t.into(),
                Err(e) => e.into_compile_error().into(),
            }
        }
    };
}

pub type Result<T, E = syn::Error> = std::result::Result<T, E>;

pub fn parse_derive_input(input: proc_macro::TokenStream) -> Result<DeriveInput> {
    match syn::parse::<syn::DeriveInput>(input) {
        Ok(input) => Ok(DeriveInput::from_syn(input)),
        Err(e) => Err(e),
    }
}

pub enum DeriveInput {
    Struct(Struct),
    Enum(Enum),
    Union(Union),
}

pub struct Struct {
    pub attrs: Attributes,
    pub vis: syn::Visibility,
    pub struct_token: syn::token::Struct,
    pub name: syn::Ident,
    pub generics: syn::Generics,
    pub kind: StructKind,
    pub fields: Vec<Field>,
}

pub struct Enum {
    pub attrs: Attributes,
    pub vis: syn::Visibility,
    pub enum_token: syn::token::Enum,
    pub name: syn::Ident,
    pub generics: syn::Generics,
    pub variants: Vec<Variant>,
}

pub struct Union {
    pub attrs: Attributes,
    pub vis: syn::Visibility,
    pub union_token: syn::token::Union,
    pub name: syn::Ident,
    pub generics: syn::Generics,
    pub fields: Vec<Field>,
}

pub struct Attributes {
    pub attrs: Vec<Attribute>,
}

pub struct Attribute {
    pub attr: syn::Attribute,
}

pub enum StructKind {
    Unit(syn::token::Semi),
    Tuple(syn::token::Paren),
    Struct(syn::token::Brace),
}

pub enum VariantKind {
    Unit(proc_macro2::Span),
    Tuple(syn::token::Paren),
    Struct(syn::token::Brace),
}

pub struct Field {
    pub attrs: Attributes,
    pub vis: syn::Visibility,
    pub name: syn::Member,
    pub ty: syn::Type,
}

pub struct Variant {
    pub attrs: Attributes,
    pub name: syn::Ident,
    pub kind: VariantKind,
    pub fields: Vec<Field>,
    pub discriminant: Option<syn::Expr>,
}

impl DeriveInput {
    pub fn attrs(&self) -> &Attributes {
        match self {
            Self::Struct(s) => &s.attrs,
            Self::Enum(e) => &e.attrs,
            Self::Union(u) => &u.attrs,
        }
    }

    pub fn vis(&self) -> &syn::Visibility {
        match self {
            Self::Struct(s) => &s.vis,
            Self::Enum(e) => &e.vis,
            Self::Union(u) => &u.vis,
        }
    }

    pub fn name(&self) -> &syn::Ident {
        match self {
            Self::Struct(s) => &s.name,
            Self::Enum(e) => &e.name,
            Self::Union(u) => &u.name,
        }
    }

    pub fn generics(&self) -> &syn::Generics {
        match self {
            Self::Struct(s) => &s.generics,
            Self::Enum(e) => &e.generics,
            Self::Union(u) => &u.generics,
        }
    }

    pub fn expect_struct(self) -> Result<Struct> {
        match self {
            Self::Struct(s) => Ok(s),
            Self::Enum(e) => Err(syn::Error::new(e.enum_token.span, "expected struct")),
            Self::Union(u) => Err(syn::Error::new(u.union_token.span, "expected struct")),
        }
    }

    pub fn expect_enum(self) -> Result<Enum> {
        match self {
            Self::Struct(s) => Err(syn::Error::new(s.struct_token.span, "expected enum")),
            Self::Enum(e) => Ok(e),
            Self::Union(u) => Err(syn::Error::new(u.union_token.span, "expected enum")),
        }
    }
}

impl Struct {
    pub fn field_names(&self) -> impl Iterator<Item = &syn::Member> {
        self.fields.iter().map(|f| &f.name)
    }

    pub fn field_types(&self) -> impl Iterator<Item = &syn::Type> {
        self.fields.iter().map(|f| &f.ty)
    }
}

impl Enum {
    pub fn variant_names(&self) -> impl Iterator<Item = &syn::Ident> {
        self.variants.iter().map(|f| &f.name)
    }

    /// Get an iterator over the types of all fields of all variants.
    ///
    /// The iterator does not perform any de-duplication of identical types.
    pub fn all_field_types(&self) -> impl Iterator<Item = &syn::Type> {
        self.variants
            .iter()
            .flat_map(|variant| variant.fields.iter().map(|field| &field.ty))
    }
}

impl Union {
    pub fn field_names(&self) -> impl Iterator<Item = &syn::Ident> {
        self.fields.iter().map(|f| match &f.name {
            syn::Member::Named(n) => n,
            syn::Member::Unnamed(_) => unreachable!(),
        })
    }

    pub fn field_types(&self) -> impl Iterator<Item = &syn::Type> {
        self.fields.iter().map(|f| &f.ty)
    }
}

impl Attributes {
    pub fn check_no(&self, attr: &str) -> Result<()> {
        let mut error = None::<syn::Error>;
        for a in &self.attrs {
            if let Some(ident) = a.attr.path.get_ident() {
                if ident == attr {
                    let e =
                        syn::Error::new(ident.span(), format!("#[{}] not supported here", attr));
                    match &mut error {
                        Some(error) => error.combine(e),
                        None => error = Some(e),
                    }
                }
            }
        }
        match error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    pub fn find_single(
        &self,
        attr: &'static str,
    ) -> Result<Option<(proc_macro2::Span, &syn::Attribute)>> {
        let mut attrs = self.find_all(attr);

        let first = attrs.next();

        if let Some(first) = first {
            if let Some(dup) = attrs.next() {
                let mut e = syn::Error::new(dup.0, format!("only one #[{}] is allowed here", attr));
                e.combine(syn::Error::new(first.0, format!("first #[{}] here", attr)));
                return Err(e);
            }
        }

        Ok(first)
    }

    pub fn find_all(
        &self,
        attr: &'static str,
    ) -> impl Iterator<Item = (proc_macro2::Span, &syn::Attribute)> {
        self.attrs.iter().filter_map(move |a| {
            if let Some(ident) = a.attr.path.get_ident() {
                if ident == attr {
                    return Some((ident.span(), &a.attr));
                }
            }
            None
        })
    }
}

impl Attribute {}

impl StructKind {
    pub fn expect_unit(&self) -> Result<()> {
        match self {
            Self::Unit(_) => Ok(()),
            Self::Tuple(p) => Err(syn::Error::new(
                p.span,
                "expected unit struct, but got `(..)` instead of `;`",
            )),
            Self::Struct(b) => Err(syn::Error::new(
                b.span,
                "expected unit struct, but got `{..}` instead of `;`",
            )),
        }
    }

    pub fn expect_tuple(&self) -> Result<()> {
        match self {
            Self::Unit(s) => Err(syn::Error::new(
                s.span,
                "expected tuple struct, but missing `(..)`",
            )),
            Self::Tuple(_) => Ok(()),
            Self::Struct(b) => Err(syn::Error::new(
                b.span,
                "expected tuple struct, but got `{..}` instead of `(..)`",
            )),
        }
    }

    pub fn expect_struct(&self) -> Result<()> {
        match self {
            Self::Unit(s) => Err(syn::Error::new(
                s.span,
                "expected regular struct, but got `;` instead of `{..}`",
            )),
            Self::Tuple(p) => Err(syn::Error::new(
                p.span,
                "expected regular struct, but got `(..)` instead of `{..}`",
            )),
            Self::Struct(_) => Ok(()),
        }
    }
}

impl Variant {
    pub fn field_names(&self) -> impl Iterator<Item = &syn::Member> {
        self.fields.iter().map(|f| &f.name)
    }

    pub fn field_types(&self) -> impl Iterator<Item = &syn::Type> {
        self.fields.iter().map(|f| &f.ty)
    }
}

impl VariantKind {
    pub fn expect_unit(&self) -> Result<()> {
        match self {
            Self::Unit(_) => Ok(()),
            Self::Tuple(p) => Err(syn::Error::new(
                p.span,
                "expected unit variant, but got `(..)` instead of `,`",
            )),
            Self::Struct(b) => Err(syn::Error::new(
                b.span,
                "expected unit variant, but got `{..}` instead of `,`",
            )),
        }
    }

    pub fn expect_tuple(&self) -> Result<()> {
        match self {
            Self::Unit(span) => Err(syn::Error::new(
                *span,
                "expected tuple variant, but missing `(..)`",
            )),
            Self::Tuple(_) => Ok(()),
            Self::Struct(b) => Err(syn::Error::new(
                b.span,
                "expected tuple varaiant, but got `{..}` instead of `(..)`",
            )),
        }
    }

    pub fn expect_struct(&self) -> Result<()> {
        match self {
            Self::Unit(span) => Err(syn::Error::new(
                *span,
                "expected struct variant, but missing `{..}`",
            )),
            Self::Tuple(p) => Err(syn::Error::new(
                p.span,
                "expected struct variant, but got `(..)` instead of `{..}`",
            )),
            Self::Struct(_) => Ok(()),
        }
    }
}

pub fn combine_generics(one: syn::Generics, another: syn::Generics) -> syn::Generics {
    let mut gen = one;

    if gen.params.is_empty() {
        gen.params = another.params;
    } else if !another.params.is_empty() {
        let mut params = syn::punctuated::Punctuated::new();

        let mut a = std::mem::take(&mut gen.params).into_pairs().peekable();
        let mut b = another.params.into_pairs().peekable();

        // Add the lifetimes from either side first, then the types, then the rest (consts).
        for &predicate in &[
            |p: &syn::GenericParam| matches!(p, syn::GenericParam::Lifetime(_)),
            |p: &syn::GenericParam| matches!(p, syn::GenericParam::Type(_)),
            |_: &syn::GenericParam| true,
        ] {
            for source in &mut [&mut a, &mut b] {
                while let Some(pair) = source.next_if(|p| predicate(p.value())) {
                    if !params.empty_or_trailing() {
                        params.push_punct(Default::default());
                    }
                    params.extend(Some(pair));
                }
            }
        }

        gen.params = params;
    }

    if let Some(w) = &mut gen.where_clause {
        if let Some(w2) = another.where_clause {
            w.predicates.extend(w2.predicates);
        }
    } else {
        gen.where_clause = another.where_clause;
    }

    gen
}

pub fn where_clause(w: syn::WhereClause) -> syn::Generics {
    let mut g = syn::Generics::default();
    g.where_clause = Some(w);
    g
}

pub trait Type {
    fn name(&self) -> &syn::Ident;
    fn generics(&self) -> &syn::Generics;
}

impl Type for (&syn::Ident, &syn::Generics) {
    fn name(&self) -> &syn::Ident {
        self.0
    }
    fn generics(&self) -> &syn::Generics {
        self.1
    }
}

impl Type for DeriveInput {
    fn name(&self) -> &syn::Ident {
        self.name()
    }
    fn generics(&self) -> &syn::Generics {
        self.generics()
    }
}

impl Type for Struct {
    fn name(&self) -> &syn::Ident {
        &self.name
    }
    fn generics(&self) -> &syn::Generics {
        &self.generics
    }
}

impl Type for Enum {
    fn name(&self) -> &syn::Ident {
        &self.name
    }
    fn generics(&self) -> &syn::Generics {
        &self.generics
    }
}

impl Type for Union {
    fn name(&self) -> &syn::Ident {
        &self.name
    }
    fn generics(&self) -> &syn::Generics {
        &self.generics
    }
}

pub fn impl_trait(
    trait_path: impl ToTokens,
    ty: &impl Type,
    block: impl ToTokens,
) -> proc_macro2::TokenStream {
    let (impl_gen, ty_gen, where_clause) = ty.generics().split_for_impl();
    let ty = ty.name();
    quote! {
        impl #impl_gen #trait_path for #ty #ty_gen #where_clause {
            #block
        }
    }
}

pub fn impl_generic_trait(
    trait_path: impl ToTokens,
    generics: syn::Generics,
    ty: &impl Type,
    block: impl ToTokens,
) -> proc_macro2::TokenStream {
    let generics = combine_generics(ty.generics().clone(), generics);
    let (_, ty_gen, _) = ty.generics().split_for_impl();
    let (impl_gen, _, where_clause) = generics.split_for_impl();
    let ty = ty.name();
    quote! {
        impl #impl_gen #trait_path for #ty #ty_gen #where_clause {
            #block
        }
    }
}
