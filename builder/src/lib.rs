use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_name = &input.ident;
    let builder_ident = format_ident!("{}Builder", input_name);

    let builder_struct = gen_builder_struct(&input, &builder_ident);
    let builder_impl = gen_builder_impl(&input, &builder_ident);
    let input_impl = gen_input_impl(&input, &builder_ident);

    let macro_output = quote! {
        #builder_struct
        #builder_impl
        #input_impl
    };

    macro_output.into()
}

fn get_fields(input: &DeriveInput) -> &syn::Fields {
    match input.data {
        syn::Data::Struct(ref ds) => &ds.fields,
        _ => unimplemented!(),
    }
}

fn get_inner_option_type(ty: &syn::Type) -> Option<&syn::Type> {
    let option_ident = proc_macro2::Ident::new("Option", proc_macro2::Span::call_site());
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty
    {
        if let Some(syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        }) = segments.iter().next()
        {
            if ident == &option_ident {
                if let Some(syn::GenericArgument::Type(inner_ty)) = args.iter().next() {
                    return Some(inner_ty);
                }
            }
        }
    }
    None
}

fn gen_builder_struct(input: &DeriveInput, ident: &syn::Ident) -> proc_macro2::TokenStream {
    let fields = get_fields(input).iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        if let Some(inner_ty) = get_inner_option_type(&field_ty) {
            return quote! { #field_ident: std::option::Option<#inner_ty> };
        }
        quote! { #field_ident: std::option::Option<#field_ty> }
    });
    quote! {
        pub struct #ident {
            #( #fields, )*
        }
    }
}

fn gen_build_fn(input: &DeriveInput) -> proc_macro2::TokenStream {
    let input_ident = &input.ident;
    let fields = get_fields(input);
    let field_setters = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        if let Some(_ty) = get_inner_option_type(&field_ty) {
            return quote! { #field_ident: self.#field_ident.clone() };
        }
        let err_msg = format!("{} value not supplied", field_ident);
        let field_setter = quote! {
            self.#field_ident
                .as_ref()
                .ok_or_else(|| #err_msg.to_owned())?
                .to_owned()
        };
        quote! { #field_ident: #field_setter }
    });
    let ret_type = quote! {
        std::result::Result<#input_ident, std::boxed::Box<dyn std::error::Error>>
    };
    quote! {
        fn build(&mut self) -> #ret_type {
            Ok(#input_ident {
                #( #field_setters, )*
            })
        }
    }
}

fn gen_builder_impl(input: &DeriveInput, ident: &syn::Ident) -> proc_macro2::TokenStream {
    let fields = get_fields(input);
    let build_fn_impl = gen_build_fn(input);
    let builder_impl_fns = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        if let Some(inner_ty) = get_inner_option_type(&field_ty) {
            return quote! {
                fn #field_ident(&mut self, #field_ident: #inner_ty) -> &mut Self {
                    self.#field_ident = std::option::Option::Some(#field_ident);
                    self
                }
            };
        }
        quote! {
            fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = std::option::Option::Some(#field_ident);
                self
            }
        }
    });
    quote! {
        impl #ident {
            #build_fn_impl
            #( #builder_impl_fns )*
        }
    }
}

fn gen_input_impl(input: &DeriveInput, builder_ident: &syn::Ident) -> proc_macro2::TokenStream {
    let input_name = &input.ident;
    let fields = get_fields(input);
    let builder_fn_fields = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        quote! { #field_ident: std::option::Option::None }
    });
    quote! {
        impl #input_name {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #( #builder_fn_fields, )*
                }
            }
        }
    }
}
