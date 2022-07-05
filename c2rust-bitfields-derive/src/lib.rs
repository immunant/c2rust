#![recursion_limit = "512"]

use proc_macro::TokenStream;

use itertools::Itertools;
use quote::{format_ident, quote};
use syn::parse::Error;
use syn::spanned::Spanned;
use syn::{Attribute, Field, Fields, Ident, ItemStruct, Lit, Meta, NestedMeta, Type};

#[cfg(target_endian = "big")]
compile_error!("Big endian architectures are not currently supported");

/// This struct keeps track of a single bitfield attr's params
/// as well as the bitfield's field name.
#[derive(Debug)]
struct BFFieldAttr {
    field_name: Ident,
    name: String,
    ty: String,
    bits: (String, proc_macro2::Span),
}

fn parse_bitfield_attr(
    attr: &Attribute,
    field_ident: &Ident,
) -> Result<Option<BFFieldAttr>, Error> {
    let mut name = None;
    let mut ty = None;
    let mut bits = None;

    if let Meta::List(meta_list) = attr.parse_meta()? {
        for nested_meta in meta_list.nested {
            if let NestedMeta::Meta(Meta::NameValue(meta_name_value)) = nested_meta {
                let rhs_string = match meta_name_value.lit {
                    Lit::Str(lit_str) => lit_str.value(),
                    _ => {
                        let err_str = "Found bitfield attribute with non str literal assignment";
                        let span = meta_name_value.path.span();

                        return Err(Error::new(span, err_str));
                    }
                };

                if let Some(lhs_ident) = meta_name_value.path.get_ident() {
                    match lhs_ident.to_string().as_str() {
                        "name" => name = Some(rhs_string),
                        "ty" => ty = Some(rhs_string),
                        "bits" => {
                            bits = Some((rhs_string, meta_name_value.path.span()));
                        }
                        // This one shouldn't ever occur here,
                        // but we're handling it just to be safe
                        "padding" => {
                            return Ok(None);
                        }
                        _ => {}
                    }
                }
            } else if let NestedMeta::Meta(Meta::Path(ref path)) = nested_meta {
                if let Some(ident) = path.get_ident() {
                    if ident == "padding" {
                        return Ok(None);
                    }
                }
            }
        }
    }

    let mut missing_fields = Vec::new();

    if name.is_none() {
        missing_fields.push("name");
    }

    if ty.is_none() {
        missing_fields.push("ty");
    }

    if bits.is_none() {
        missing_fields.push("bits");
    }

    if let (Some(name), Some(ty), Some(bits)) = (name, ty, bits) {
        Ok(Some(BFFieldAttr {
            field_name: field_ident.clone(),
            name,
            ty,
            bits,
        }))
    } else {
        let err_str = format!("Missing bitfield params: {:?}", missing_fields);
        let span = attr.path.segments.span();

        Err(Error::new(span, err_str))
    }
}

fn filter_and_parse_fields(field: &Field) -> Vec<Result<BFFieldAttr, Error>> {
    field
        .attrs
        .iter()
        .filter_map(|attr| {
            (attr.path.segments.last().unwrap().ident == "bitfield")
                .then(|| parse_bitfield_attr(attr, field.ident.as_ref().unwrap()).transpose())
                .flatten()
        })
        .collect()
}

fn parse_bit_params(field: &BFFieldAttr) -> syn::Result<(usize, usize)> {
    let (bit_string, span) = &field.bits;
    bit_string
        .split_once("..=")
        .and_then(|(lo, hi)| Some((lo.parse().ok()?, hi.parse().ok()?)))
        .ok_or_else(|| Error::new(*span, "bits param must be in the format \"lo..=hi\""))
}

fn bitfield_struct_impl(struct_item: ItemStruct) -> Result<TokenStream, Error> {
    // REVIEW: Should we throw a compile error if bit ranges on a single field overlap?
    let struct_ident = struct_item.ident;
    let fields = match struct_item.fields {
        Fields::Named(named_fields) => named_fields.named,
        Fields::Unnamed(_) => {
            let err_str =
                "Unnamed struct fields are not currently supported but may be in the future.";
            let span = struct_ident.span();

            return Err(Error::new(span, err_str));
        }
        Fields::Unit => {
            let err_str = "Cannot create bitfield struct out of struct with no fields";
            let span = struct_ident.span();

            return Err(Error::new(span, err_str));
        }
    };

    let bitfields: Vec<_> = fields
        .iter()
        .flat_map(filter_and_parse_fields)
        .try_collect()?;

    let field_types: Vec<Type> = bitfields
        .iter()
        .map(|bf| syn::parse_str(&bf.ty))
        .try_collect()?;

    let method_names: Vec<_> = bitfields
        .iter()
        .map(|field| format_ident!("{}", field.name))
        .collect();

    let field_names: Vec<_> = bitfields.iter().map(|field| &field.field_name).collect();

    let method_name_setters: Vec<_> = method_names
        .iter()
        .map(|field_ident| format_ident!("set_{}", field_ident))
        .collect();

    let field_bit_info: Vec<_> = bitfields
        .iter()
        .map(|field| {
            let (lo, hi) = parse_bit_params(field)?;
            Ok::<_, Error>(quote! { (#lo, #hi) })
        })
        .try_collect()?;

    // TODO: Method visibility determined by struct field visibility?
    let q = quote! {
        #[automatically_derived]
        impl #struct_ident {
            #(
                /// This method allows you to write to a bitfield with a value
                pub fn #method_name_setters(&mut self, int: #field_types) {
                    use c2rust_bitfields::FieldType;

                    let field = &mut self.#field_names;
                    let (lhs_bit, rhs_bit) = #field_bit_info;
                    int.set_field(field, (lhs_bit, rhs_bit));
                }

                /// This method allows you to read from a bitfield to a value
                pub fn #method_names(&self) -> #field_types {
                    use c2rust_bitfields::FieldType;

                    type IntType = #field_types;

                    let field = &self.#field_names;
                    let (lhs_bit, rhs_bit) = #field_bit_info;
                    <IntType as FieldType>::get_field(field, (lhs_bit, rhs_bit))
                }
            )*
        }
    };

    Ok(q.into())
}

#[proc_macro_derive(BitfieldStruct, attributes(bitfield))]
pub fn bitfield_struct(input: TokenStream) -> TokenStream {
    let struct_item = syn::parse_macro_input!(input as ItemStruct);

    match bitfield_struct_impl(struct_item) {
        Ok(ts) => ts,
        Err(error) => error.to_compile_error().into(),
    }
}
