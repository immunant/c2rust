#![recursion_limit="128"]

extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, Field, Fields, Ident, ItemStruct, Lit, Meta, NestedMeta, Path, PathArguments, PathSegment, Token, parse_macro_input};
use syn::punctuated::Punctuated;

#[cfg(target_endian = "big")]
compile_error!("Big endian architectures are not currently supported");

/// This struct keeps track of a single bitfield attr's params
/// as well as the bitfield's field name.
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

fn parse_bitfield_ty_path(field: &BFFieldAttr) -> Path {
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
    let field_types: Vec<_> = bitfields.iter().map(parse_bitfield_ty_path).collect();
    let field_type_setters = field_types.clone();
    let method_names: Vec<_> = bitfields.iter().map(|field| Ident::new(&field.name, Span::call_site().into())).collect();
    let field_name: Vec<_> = bitfields.iter().map(|field| &field.field_name).collect();
    let method_name_setters: Vec<_> = method_names.iter().map(|field_ident| {
        let span = Span::call_site().into();
        let setter_name = &format!("set_{}", field_ident);

        Ident::new(setter_name, span)
    }).collect();
    let field_bit_info: Vec<_> = bitfields.iter().map(|field| {
        let bit_string = &field.bits;
        let mut split = bit_string.split("..=");
        let lhs: usize = split.next().unwrap().parse().unwrap();
        let rhs: usize = split.next().unwrap().parse().unwrap();

        quote! { (#lhs, #rhs) }
    }).collect();

    // TODO: Field visibility determined by struct field visibility?
    // TODO: Add generic doc strings
    let q = quote! {
        impl #struct_ident {
            #(
                pub fn #method_name_setters(&mut self, val: #field_type_setters) {
                    let field = self.#field_name;
                    let (lhs_bit, rhs_bit) = #field_bit_info;
                    let bit_width = rhs_bit - lhs_bit;
                    // let byte_index = field.len() - bit_width / 8;
                    let start_byte_index = lhs_bit / 8;
                    let end_byte_index = rhs_bit / 8;

                    // Fits in a single byte
                    if start_byte_index == end_byte_index {
                        let mut byte = field[start_byte_index];

                        for bit_pos in 7..=0 {
                            let bit = val;

                            // byte;

                            // bit_width -= 1;
                        }

                        return
                    }

                    // TODO: Multi byte
                }

                pub fn #method_names(&self) -> #field_types {
                    42
                }
            )*
        }
    };

    println!("{}", q);

    q.into()
}
