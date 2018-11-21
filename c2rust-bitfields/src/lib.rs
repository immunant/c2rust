extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, DeriveInput, Fields, Ident, ItemStruct, Lit, Meta, NestedMeta, Type, parse_macro_input};

#[derive(Debug)]
struct BFFieldAttr {
    field_name: Ident,
    name: String,
    ty: String,
    bits: String,
}

fn parse_bitfield_attr(attr: &Attribute) -> (String, String, String) {
    println!("attrs: {}", attr.tts);
    let mut name = None as Option<String>;
    let mut ty = None as Option<String>;
    let mut bits = None as Option<String>;

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

#[proc_macro_derive(BitfieldStruct, attributes(bitfield))]
pub fn bitfield_struct(input: TokenStream) -> TokenStream {
    println!("{}", input);

    let struct_item = parse_macro_input!(input as ItemStruct);
    let struct_ident = struct_item.ident;
    let fields = match struct_item.fields {
        Fields::Named(named_fields) => named_fields.named,
        _ => panic!("Unnamed bitfield fields are not currently supported"),
    };
    let bitfields: Vec<BFFieldAttr> = fields.iter().flat_map(|field| {
        let attrs: Vec<_> = field.attrs.iter().filter(|attr|
            attr.path.segments.last().unwrap().value().ident == "bitfield"
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
    }).collect();

    println!("{:?}", bitfields);

    let field_names: Vec<_> = bitfields.iter().map(|field| Ident::new(&field.name, Span::call_site().into())).collect();
    let field_name_setters: Vec<_> = field_names.iter().map(|field_ident| {
        let span = Span::call_site().into();
        let setter_name = &format!("set_{}", field_ident);

        Ident::new(setter_name, span)
    }).collect();

    let q = quote! {
        impl #struct_ident {
            #(
                pub fn #field_name_setters(&self) {
                }

                pub fn #field_names(&self) -> u64 {
                    42
                }
            )*
        }
    };

    println!("{}", q);

    q.into()
}
