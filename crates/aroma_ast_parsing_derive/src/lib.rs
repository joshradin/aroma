use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(ToTokens)]
pub fn derive_to_tokens(tokens: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(tokens as DeriveInput);
    let ident = &derive_input.ident;
    match &derive_input.data {
        Data::Struct(s) => {
            let generics = derive_input.generics.type_params();
            let generic_uses = derive_input.generics.type_params().map(|t| &t.ident);
            let fields = s.fields.iter().enumerate().map(|(index, field)| {
                field
                    .ident
                    .as_ref()
                    .map(|id| quote!( #id ))
                    .unwrap_or(quote! { #index })
            });

            quote! (
                #[automatically_derived]
                impl <'p #(, #generics)*> aroma_ast::token::ToTokens<'p> for #ident <'p, #(, #generic_uses)*> {
                    fn to_tokens(&self) -> TokenStream<'p, 'p> {
                        TokenStream::from_iter(
                            [#(
                                self.#fields.to_tokens()
                            ),*]
                            .into_iter()
                            .flatten()
                        )
                    }
                }
            )
            .into()
        }
        Data::Enum(en) => {
            let generics = derive_input.generics.type_params();
            let generic_uses = derive_input.generics.type_params().map(|t| &t.ident);
            let mut matches = vec![];
            for variant in &en.variants {
                let id = &variant.ident;
                let fields = &variant.fields;

                let match_case = match fields {
                    Fields::Named(named) => {
                        let bindings = named
                            .named
                            .iter()
                            .flat_map(|i| i.ident.as_ref())
                            .map(|i| format_ident!("__self_{ident}"))
                            .collect::<Vec<_>>();

                        let pats = fields
                            .iter()
                            .flat_map(|f| f.ident.as_ref())
                            .zip(bindings.iter())
                            .map(|(field_id, binding)| {
                                quote! { #field_id: #binding }
                            });

                        let len = named.named.len();

                        quote! {
                            #ident::#id { #(#pats),* } => {
                                TokenStream::from_iter(
                                    <[TokenStream<'p, 'p>; #len]>::into_iter([#(
                                       #bindings.to_tokens()
                                    ),*]
                                    )
                                    .flatten()
                                )
                            }
                        }
                    }
                    Fields::Unnamed(unnamed) => {
                        let bindings = (0..unnamed.unnamed.len())
                            .into_iter()
                            .map(|i| format_ident!("__self_{ident}"))
                            .collect::<Vec<_>>();
                        let len = bindings.len();
                        quote! {
                            #ident::#id(#(#bindings),*) => {
                                TokenStream::from_iter(
                                    <[TokenStream<'p, 'p>; #len]>::into_iter([#(
                                       #bindings.to_tokens()
                                    ),*]
                                    )
                                    .flatten()
                                )
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            #ident::#id => { TokenStream::new() }
                        }
                    }
                };

                matches.push(match_case);
            }

            quote! (
                #[automatically_derived]
                impl <'p #(, #generics)*> aroma_ast::token::ToTokens<'p> for #ident <'p, #(, #generic_uses)*> {
                    fn to_tokens(&self) -> TokenStream<'p, 'p> {
                        match self {
                            #(#matches)*
                        }
                    }
                }
            ).into()
        }
        Data::Union(_) => syn::Error::new(derive_input.span(), "unions not supported")
            .into_compile_error()
            .into(),
    }
}
