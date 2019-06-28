#![recursion_limit = "512"]

extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::{Span, TokenStream};
use quote::__rt;
use quote::quote;
use syn::parse::Error;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Field, Fields, Ident, ItemStruct, Lit, Meta, NestedMeta, Path,
    PathArguments, PathSegment, Token,
};

#[cfg(target_endian = "big")]
compile_error!("Big endian architectures are not currently supported");

/// This struct keeps track of a single bitfield attr's params
/// as well as the bitfield's field name.
#[derive(Debug)]
struct BFFieldAttr {
    field_name: Ident,
    name: String,
    ty: String,
    bits: (String, __rt::Span),
}

fn parse_bitfield_attr(
    attr: &Attribute,
    field_ident: &Ident,
) -> Result<Option<BFFieldAttr>, Error> {
    let mut name = None;
    let mut ty = None;
    let mut bits = None;
    let mut bits_span = None;

    if let Meta::List(meta_list) = attr.parse_meta()? {
        for nested_meta in meta_list.nested {
            if let NestedMeta::Meta(Meta::NameValue(meta_name_value)) = nested_meta {
                let rhs_string = match meta_name_value.lit {
                    Lit::Str(lit_str) => lit_str.value(),
                    _ => {
                        let err_str = "Found bitfield attribute with non str literal assignment";
                        let span = meta_name_value.ident.span();

                        return Err(Error::new(span, err_str));
                    }
                };

                let lhs_string = meta_name_value.ident.to_string();

                match lhs_string.as_str() {
                    "name" => name = Some(rhs_string),
                    "ty" => ty = Some(rhs_string),
                    "bits" => {
                        bits = Some(rhs_string);
                        bits_span = Some(meta_name_value.ident.span());
                    }
                    // This one shouldn't ever occur here,
                    // but we're handling it just to be safe
                    "padding" => {
                        return Ok(None);
                    }
                    _ => {}
                }
            } else if let NestedMeta::Meta(Meta::Word(ref ident)) = nested_meta {
                if ident == "padding" {
                    return Ok(None);
                }
            }
        }
    }

    if name.is_none() || ty.is_none() || bits.is_none() {
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

        let err_str = format!("Missing bitfield params: {:?}", missing_fields);
        let span = attr.path.segments.span();

        return Err(Error::new(span, err_str));
    }

    Ok(Some(BFFieldAttr {
        field_name: field_ident.clone(),
        name: name.unwrap(),
        ty: ty.unwrap(),
        bits: (bits.unwrap(), bits_span.unwrap()),
    }))
}

fn filter_and_parse_fields(field: &Field) -> Vec<Result<BFFieldAttr, Error>> {
    let attrs: Vec<_> = field
        .attrs
        .iter()
        .filter(|attr| attr.path.segments.last().unwrap().value().ident == "bitfield")
        .collect();

    if attrs.len() == 0 {
        return Vec::new();
    }

    attrs
        .into_iter()
        .map(|attr| parse_bitfield_attr(attr, &field.ident.as_ref().unwrap()))
        .flat_map(Result::transpose) // Remove the Ok(None) values
        .collect()
}

fn parse_bitfield_ty_path(field: &BFFieldAttr) -> Path {
    let leading_colon = if field.ty.starts_with("::") {
        Some(Token![::]([
            Span::call_site().into(),
            Span::call_site().into(),
        ]))
    } else {
        None
    };

    let mut segments = Punctuated::new();
    let mut segment_strings = field.ty.split("::").peekable();

    while let Some(segment_string) = segment_strings.next() {
        segments.push_value(PathSegment {
            ident: Ident::new(segment_string, Span::call_site().into()),
            arguments: PathArguments::None,
        });

        if segment_strings.peek().is_some() {
            segments.push_punct(Token![::]([
                Span::call_site().into(),
                Span::call_site().into(),
            ]));
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

    match bitfield_struct_impl(struct_item) {
        Ok(ts) => ts,
        Err(error) => error.to_compile_error().into(),
    }
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
    let bitfields: Result<Vec<BFFieldAttr>, Error> =
        fields.iter().flat_map(filter_and_parse_fields).collect();
    let bitfields = bitfields?;
    let field_types: Vec<_> = bitfields.iter().map(parse_bitfield_ty_path).collect();
    let field_types_return = &field_types;
    let field_types_typedef = &field_types;
    let field_types_setter_arg = &field_types;
    let method_names: Vec<_> = bitfields
        .iter()
        .map(|field| Ident::new(&field.name, Span::call_site().into()))
        .collect();
    let field_names: Vec<_> = bitfields.iter().map(|field| &field.field_name).collect();
    let field_names_setters = &field_names;
    let field_names_getters = &field_names;
    let method_name_setters: Vec<_> = method_names
        .iter()
        .map(|field_ident| {
            let span = Span::call_site().into();
            let setter_name = &format!("set_{}", field_ident);

            Ident::new(setter_name, span)
        })
        .collect();
    let field_bit_info: Result<Vec<_>, Error> = bitfields
        .iter()
        .map(|field| {
            let bit_string = &field.bits.0;
            let nums: Vec<_> = bit_string.split("..=").collect();
            let err_str = "bits param must be in the format \"1..=4\"";

            if nums.len() != 2 {
                return Err(Error::new(field.bits.1, err_str));
            }

            let lhs = nums[0].parse::<usize>();
            let rhs = nums[1].parse::<usize>();

            let (lhs, rhs) = match (lhs, rhs) {
                (Err(_), _) | (_, Err(_)) => return Err(Error::new(field.bits.1, err_str)),
                (Ok(lhs), Ok(rhs)) => (lhs, rhs),
            };

            Ok(quote! { (#lhs, #rhs) })
        })
        .collect();
    let field_bit_info = field_bit_info?;
    let field_bit_info_setters = &field_bit_info;
    let field_bit_info_getters = &field_bit_info;

    // TODO: Method visibility determined by struct field visibility?
    let q = quote! {
        #[automatically_derived]
        impl #struct_ident {
            #(
                /// This method allows you to write to a bitfield with a value
                pub fn #method_name_setters(&mut self, int: #field_types_setter_arg) {
                    use c2rust_bitfields::FieldType;

                    let field = &mut self.#field_names_setters;
                    let (lhs_bit, rhs_bit) = #field_bit_info_setters;
                    int.set_field(field, (lhs_bit, rhs_bit));
                }

                /// This method allows you to read from a bitfield to a value
                pub fn #method_names(&self) -> #field_types_return {
                    use c2rust_bitfields::FieldType;

                    type IntType = #field_types_typedef;

                    let field = &self.#field_names_getters;
                    let (lhs_bit, rhs_bit) = #field_bit_info_getters;
                    <IntType as FieldType>::get_field(field, (lhs_bit, rhs_bit))
                }
            )*
        }
    };

    Ok(q.into())
}
