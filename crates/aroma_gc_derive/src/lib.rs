use quote::{format_ident, quote};
use syn::{Data, DataEnum, DataStruct, DeriveInput, Fields, Ident, parse_macro_input};

#[doc(hidden)]
#[proc_macro]
pub fn derive_tuples(_tok: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut impls = vec![];

    for i in 1..=12 {
        let generics = (0..i)
            .into_iter()
            .map(|i| format_ident!("T{i}"))
            .collect::<Vec<_>>();
        let idents = (0..i)
            .into_iter()
            .map(|i| format_ident!("__self_{i}"))
            .collect::<Vec<_>>();

        let quoted = quote! {
            unsafe impl<#(#generics),*> crate::Trace for (#(#generics),*,)
                where
                    #(#generics : crate::Trace),*
            {
                fn trace(&self) {
                    let (#(#idents),*,) = self;
                    #(crate::Trace::trace(#idents);)*
                }

                fn finalize(&mut self) {
                    let (#(#idents),*,) = self;
                    #(crate::Trace::finalize(#idents);)*
                }
            }
        };
        eprintln!("created: {quoted}");
        impls.push(quoted);
    }

    quote! (
        #(#impls)*
    )
    .into()
}

#[proc_macro_derive(Trace, attributes(req_static))]
pub fn derive_trace(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let expanded = match &input.data {
        Data::Struct(s) => derive_trace_struct(&input, s),
        Data::Enum(e) => derive_trace_enum(&input, e),
        Data::Union(_) => {
            unimplemented!("")
        }
    };

    expanded.into()
}

fn derive_trace_struct(
    derive_input: &DeriveInput,
    d_struct: &DataStruct,
) -> proc_macro2::TokenStream {
    let traces = call_method_on_fields(&d_struct.fields, "trace", false);
    let finalizes = call_method_on_fields(&d_struct.fields, "finalize", true);

    eprintln!("traces: {}", quote! { traces });
    eprintln!("finalizes: {}", quote! { finalizes });

    let DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        ..
    } = derive_input;
    quote! {
        #[automatically_derived]
        unsafe impl #generics aroma_gc::Trace for #ident #generics {
            fn trace(&self) {
                #( #traces  )*
            }

            fn finalize(&mut self) {
                #( #finalizes  )*
            }
        }
    }
}

fn call_method_on_fields(
    fields: &Fields,
    method: &str,
    is_mut: bool,
) -> Vec<proc_macro2::TokenStream> {
    let method = format_ident!("{method}");
    fields
        .iter()
        .enumerate()
        .filter(|(_idx, field)| field.attrs.iter().all(|a| !a.path().is_ident("req_static")))
        .map(|(idx, field)| {
            if is_mut {
                match &field.ident {
                    None => {
                        let idx = format_ident!("{idx}");
                        quote! { aroma_gc::Trace::#method(&mut self.#idx); }
                    }
                    Some(field) => {
                        quote! {  aroma_gc::Trace::#method(&mut self.#field); }
                    }
                }
            } else {
                match &field.ident {
                    None => {
                        let idx = format_ident!("{idx}");
                        quote! { aroma_gc::Trace::#method(&self.#idx); }
                    }
                    Some(field) => {
                        quote! {  aroma_gc::Trace::#method(&self.#field); }
                    }
                }
            }
        })
        .inspect(|created| {
            eprintln!("created: {created}");
        })
        .collect::<Vec<_>>()
}

fn call_method_on_idents_refs<I: IntoIterator<Item = Ident>>(
    idents: I,
    method: &str,
    is_mut: bool,
) -> Vec<proc_macro2::TokenStream> {
    let method = format_ident!("{method}");
    idents
        .into_iter()
        .map(|ident| {
            if is_mut {
                quote! { aroma_gc::Trace::#method(&mut #ident); }
            } else {
                quote! { aroma_gc::Trace::#method(& #ident); }
            }
        })
        .collect::<Vec<_>>()
}

fn call_method_on_idents<I: IntoIterator<Item = Ident>>(
    idents: I,
    method: &str,
) -> Vec<proc_macro2::TokenStream> {
    let method = format_ident!("{method}");
    idents
        .into_iter()
        .map(|ident| {
            quote! { aroma_gc::Trace::#method(#ident); }
        })
        .collect::<Vec<_>>()
}

fn derive_trace_enum(derive_input: &DeriveInput, d_enum: &DataEnum) -> proc_macro2::TokenStream {
    let d_enum_ident = &derive_input.ident;
    let (traces, finalizers): (Vec<_>, Vec<_>) = d_enum
        .variants
        .iter()
        .map(|variant| {
            // eprintln!("variant = {variant:?}");
            let idents = variant
                .fields
                .iter()
                .enumerate()
                .filter(|(_, field)| {
                    field
                        .attrs
                        .iter()
                        .all(|attr| !attr.path().is_ident("req_static"))
                })
                .map(|(idx, ident)| match &ident.ident {
                    None => {
                        format_ident!("__self_{idx}")
                    }
                    Some(ident) => ident.clone(),
                })
                .collect::<Vec<_>>();
            eprintln!("idents = {idents:?}");
            if idents.is_empty() {
                return (quote! {}, quote! {});
            }
            let destructed = match variant.fields.iter().any(|f| f.ident.is_none()) {
                true => {
                    quote! { ( #(#idents,)* ) }
                }
                false => {
                    quote! { { #(#idents,)* } }
                }
            };
            let traces = call_method_on_idents(idents.clone(), "trace");
            let finalizers = call_method_on_idents(idents, "finalize");

            eprintln!("traces: {}", quote! { #(#traces),* });
            eprintln!("finalizes: {}", quote! { #(#finalizers),* });

            let id = &variant.ident;
            (
                quote! {
                    #d_enum_ident :: #id #destructed => {
                        #(#traces)*
                    }
                },
                quote! {
                    #d_enum_ident :: #id #destructed => {
                        #(#finalizers)*
                    }
                },
            )
        })
        .unzip();

    let DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        ..
    } = derive_input;
    quote! {
        unsafe impl #generics aroma_gc::Trace for #ident #generics {
            fn trace(&self) {
                match self {
                    #( #traces )*
                    _ => {}
                }
            }

            fn finalize(&mut self) {
                match self {
                    #( #finalizers )*
                    _ => {}
                }
            }
        }
    }
}
