use proc_macro2::TokenStream;
use drv::{Result, make_derive_macro, DeriveInput};
use quote::quote;

make_derive_macro!(Example(hello, example), example_derive);

fn example_derive(input: DeriveInput) -> Result<TokenStream> {
    let s = input.expect_struct()?;

    s.kind.expect_struct()?;

    let name = &s.name;
    let field_name = s.field_names();
    let field_ty = s.field_types();

    s.attrs.check_no("hello")?;

    for f in &s.fields {
        f.attrs.find_single("hello")?;
    }

    Ok(quote! {
        impl Default for #name {
            fn default() -> Self {
                #name {
                    #(
                        #field_name: <#field_ty as Default>::default(),
                    )*
                }
            }
        }
    })
}

make_derive_macro!(Example2, example2_derive);

fn example2_derive(input: DeriveInput) -> Result<TokenStream> {
    let e = input.expect_enum()?;

    for v in e.variants {
        v.kind.expect_tuple()?;
    }

    Ok(TokenStream::new())
}

#[proc_macro_derive(Hello, attributes(hello))]
pub fn hello(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::new()
}
