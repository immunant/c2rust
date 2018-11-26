extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, DeriveInput, Field, Fields, Ident, ItemStruct, Lit, Meta, NestedMeta, Path, PathArguments, PathSegment, Token, TypePath, parse_macro_input};
use syn::punctuated::Punctuated;

#[derive(Debug)]
struct BFFieldAttr {
    field_name: Ident,
    name: String,
    ty: String,
    bits: String,
}

fn parse_bitfield_attr(attr: &Attribute) -> (String, String, String) {
    let mut name = None;
    let mut ty = None;
    let mut bits = None;

    if let Meta::List(meta_list) = attr.interpret_meta().unwrap() {
        for nested_meta in meta_list.nested {
            if let NestedMeta::Meta(Meta::NameValue(meta_name_value)) = nested_meta {
                let rhs_string = match meta_name_value.lit {
                    Lit::Str(lit_str) => lit_str.value(),
                    _ => panic!("Found bitfield attribute with non string assignment"),
                };

                let lhs_string = meta_name_value.ident.to_string();

                match lhs_string.as_str() {
                    "name" => name = Some(rhs_string),
                    "ty" => ty = Some(rhs_string),
                    "bits" => bits = Some(rhs_string),
                    _ => {},
                }
            }
        }
    }

    if name.is_none() || ty.is_none() || bits.is_none() {
        panic!("Missing name, ty, or bits param on bitfield attribute");
    }

    (name.unwrap(), ty.unwrap(), bits.unwrap())
}

fn filter_and_parse_fields(field: &Field) -> Vec<BFFieldAttr> {
    let attrs: Vec<_> = field.attrs.iter().filter(|attr|
        attr.path
            .segments
            .last()
            .unwrap()
            .value()
            .ident == "bitfield"
    ).collect();

    if attrs.len() == 0 {
        return Vec::new();
    }

    attrs.iter().map(|attr| {
        let (name, ty, bits) = parse_bitfield_attr(attr);

        BFFieldAttr {
            field_name: field.ident.clone().unwrap(),
            name,
            ty,
            bits,
        }
    }).collect()
}

#[proc_macro_derive(BitfieldStruct, attributes(bitfield))]
pub fn bitfield_struct(input: TokenStream) -> TokenStream {
    println!("{}", input);

    let struct_item = parse_macro_input!(input as ItemStruct);
    let struct_ident = struct_item.ident;
    let fields = match struct_item.fields {
        Fields::Named(named_fields) => named_fields.named,
        _ => panic!("Unnamed bitfield fields are not currently supported"),
    };
    let bitfields: Vec<BFFieldAttr> = fields.iter().flat_map(filter_and_parse_fields).collect();
    let field_types: Vec<_> = bitfields.iter().map(|field| {
        let leading_colon = if field.ty.starts_with("::") {
            Some(Token![::]([Span::call_site().into(), Span::call_site().into()]))
        } else {
            None
        };

        let mut segments = Punctuated::new();
        let mut segement_strings = field.ty.split("::").peekable();

        while let Some(segment_string) = segement_strings.next() {
            segments.push_value(PathSegment {
                ident: Ident::new(segment_string, Span::call_site().into()),
                arguments: PathArguments::None,
            });

            if segement_strings.peek().is_some() {
                segments.push_punct(Token![::]([Span::call_site().into(), Span::call_site().into()]));
            }
        }

        Path {
            leading_colon,
            segments,
        }
    }).collect();
    let field_types2 = field_types.clone();
    let field_names: Vec<_> = bitfields.iter().map(|field| Ident::new(&field.name, Span::call_site().into())).collect();
    let field_name_setters: Vec<_> = field_names.iter().map(|field_ident| {
        let span = Span::call_site().into();
        let setter_name = &format!("set_{}", field_ident);

        Ident::new(setter_name, span)
    }).collect();

    let q = quote! {
        impl #struct_ident {
            #(
                pub fn #field_name_setters(&self, setter: #field_types) {
                }

                pub fn #field_names(&self) -> #field_types2 {
                    42
                }
            )*
        }
    };

    println!("{}", q);

    q.into()
}
