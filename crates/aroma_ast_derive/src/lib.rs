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
            let fields = s
                .fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    field
                        .ident
                        .as_ref()
                        .map(|id| quote!( #id ))
                        .unwrap_or(quote! { #index })
                })
                .collect::<Vec<_>>();

            quote! (
                #[automatically_derived]
                impl <'p #(, #generics)*> aroma_ast::token::ToTokens<'p> for #ident <'p, #(, #generic_uses)*> {
                    fn to_tokens(&self) -> aroma_ast::token::TokenStream<'p, 'p> {
                        aroma_ast::token::TokenStream::from_iter(
                            [#(
                                self.#fields.to_tokens()
                            ),*]
                            .into_iter()
                            .flatten()
                        )
                    }

                    fn to_token_tree(&self) -> aroma_ast::token::TokenTree<'p> {
                        aroma_ast::token::TokenTree::Node(Vec::from_iter(
                            [
                                #(self.#fields.to_token_tree()),*
                            ]
                        ))
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
                            .map(|i| format_ident!("__self_{i}"))
                            .collect::<Vec<_>>();

                        let pats = fields
                            .iter()
                            .flat_map(|f| f.ident.as_ref())
                            .zip(bindings.iter())
                            .map(|(field_id, binding)| {
                                quote! { #field_id: #binding }
                            })
                            .collect::<Vec<_>>();

                        let len = named.named.len();

                        (
                            quote! {
                                #ident::#id { #(#pats),* } => {
                                    aroma_ast::token::TokenStream::from_iter(
                                        <[aroma_ast::token::TokenStream<'p, 'p>; #len]>::into_iter([#(
                                           #bindings.to_tokens()
                                        ),*]
                                        )
                                        .flatten()
                                    )
                                }
                            },
                            quote! {
                                #ident::#id { #(#pats),* } => {
                                    aroma_ast::token::TokenTree::Node(
                                        Vec::from([
                                                #(#bindings.to_token_tree()),*
                                            ])
                                    )
                                }
                            },
                        )
                    }
                    Fields::Unnamed(unnamed) => {
                        let bindings = (0..unnamed.unnamed.len())
                            .into_iter()
                            .map(|i| format_ident!("__self_{i}"))
                            .collect::<Vec<_>>();
                        let len = bindings.len();
                        (
                            quote! {
                                #ident::#id(#(#bindings),*) => {
                                    aroma_ast::token::TokenStream::from_iter(
                                        <[aroma_ast::token::TokenStream<'p, 'p>; #len]>::into_iter([#(
                                           #bindings.to_tokens()
                                        ),*]
                                        )
                                        .flatten()
                                    )
                                }
                            },
                            quote! {
                                #ident::#id(#(#bindings),*) => {
                                    aroma_ast::token::TokenTree::Node(
                                        Vec::from([
                                                #(#bindings.to_token_tree()),*
                                            ])
                                    )
                                }
                            },
                        )
                    }
                    Fields::Unit => (
                        quote! {
                            #ident::#id => { aroma_ast::token::TokenStream::new() }
                        },
                        quote! { #ident::#id => { aroma_ast::token::TokenTree::Leaf(vec![]) }},
                    ),
                };

                matches.push(match_case);
            }

            let (to_token_matches, to_token_tree_matches) =
                matches.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

            quote! (
                #[automatically_derived]
                impl <'p #(, #generics)*> aroma_ast::token::ToTokens<'p> for #ident <'p, #(, #generic_uses)*> {
                    fn to_tokens(&self) -> aroma_ast::token::TokenStream<'p, 'p> {
                        match self {
                            #(#to_token_matches)*
                        }
                    }

                    fn to_token_tree(&self) -> aroma_ast::token::TokenTree<'p> {
                        match self {
                            #(#to_token_tree_matches)*
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
