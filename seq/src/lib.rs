use proc_macro::TokenStream;

use syn::Token;
use syn::parse::{Parse, ParseStream};
use syn::parse_macro_input;

struct Seq {
    pub ident: syn::Ident,
    pub start: i64,
    pub end: i64,
    pub block: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start_lit: syn::LitInt = input.parse()?;
        let start = start_lit.base10_parse()?;
        input.parse::<Token![..]>()?;
        let end_lit: syn::LitInt = input.parse()?;
        let end = end_lit.base10_parse()?;
        let content;
        syn::braced!(content in input);
        let block = content.parse()?;
        Ok(Seq {
            ident,
            start,
            end,
            block,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Seq);
    eprintln!("TOKENS: {}", input.block);

    let seq_tokens = (input.start..input.end).map(|n| {
        
    });

    TokenStream::new()
}
