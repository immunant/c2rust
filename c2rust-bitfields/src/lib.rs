#![recursion_limit="512"]

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
    let struct_item = parse_macro_input!(input as ItemStruct);
    let struct_ident = struct_item.ident;
    let fields = match struct_item.fields {
        Fields::Named(named_fields) => named_fields.named,
        _ => panic!("Unnamed bitfield fields are not currently supported"),
    };
    let bitfields: Vec<BFFieldAttr> = fields.iter().flat_map(filter_and_parse_fields).collect();
    let field_types: Vec<_> = bitfields.iter().map(parse_bitfield_ty_path).collect();
    let field_types2 = field_types.clone();
    let field_type_setters = field_types.clone();
    let field_type_setters2 = field_types.clone();
    let method_names: Vec<_> = bitfields.iter().map(|field| Ident::new(&field.name, Span::call_site().into())).collect();
    let field_names: Vec<_> = bitfields.iter().map(|field| &field.field_name).collect();
    let field_names2 = field_names.clone();
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
    let field_bit_info2 = field_bit_info.clone();

    // TODO: Field visibility determined by struct field visibility?
    // TODO: Signed ints?
    let q = quote! {
        impl #struct_ident {
            #(
                /// This method allows you to write to a bitfield with a value
                pub fn #method_name_setters(&mut self, int: #field_type_setters) {
                    fn zero_bit(byte: &mut u8, n_bit: u64) {
                        let bit = 1 << n_bit;

                        *byte &= !bit as u8;
                    }

                    fn one_bit(byte: &mut u8, n_bit: u64) {
                        let bit = 1 << n_bit;

                        *byte |= bit as u8;
                    }

                    let mut field = &mut self.#field_names;
                    let (lhs_bit, rhs_bit) = #field_bit_info;

                    // Check for overflow, which C defines to 0 the bitfield
                    let min_overflow_val = 1 << (rhs_bit - lhs_bit + 1);
                    let zeroing = int >= min_overflow_val as #field_type_setters2;

                    for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
                        let byte_index = bit_index / 8;
                        let mut byte = &mut field[byte_index];
                        let bit = 1 << i;
                        let read_bit = int & bit;

                        if zeroing || read_bit == 0 {
                            zero_bit(byte, (bit_index % 8) as u64);
                        } else {
                            one_bit(byte, (bit_index % 8) as u64);
                        }
                    }
                }

                /// This method allows you to read from a bitfield to a value
                pub fn #method_names(&self) -> #field_types {
                    let field = self.#field_names2;
                    let (lhs_bit, rhs_bit) = #field_bit_info2;
                    let mut val = 0;

                    for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
                        let byte_index = bit_index / 8;
                        let byte = field[byte_index];
                        let bit = 1 << (bit_index % 8);
                        let read_bit = byte & bit;

                        if read_bit != 0 {
                            let actual_bit = 1 << i;

                            val |= actual_bit as #field_types2;
                        }
                    }

                    val
                }
            )*
        }
    };

    q.into()
}
