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

fn parse_bitfield_attr(attr: &Attribute, field_ident: &Ident) -> Result<BFFieldAttr, Error> {
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
                    _ => {}
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

    Ok(BFFieldAttr {
        field_name: field_ident.clone(),
        name: name.unwrap(),
        ty: ty.unwrap(),
        bits: (bits.unwrap(), bits_span.unwrap()),
    })
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
    let field_types2 = field_types.clone();
    let field_type_setters = field_types.clone();
    let method_names: Vec<_> = bitfields
        .iter()
        .map(|field| Ident::new(&field.name, Span::call_site().into()))
        .collect();
    let field_names: Vec<_> = bitfields.iter().map(|field| &field.field_name).collect();
    let field_names2 = field_names.clone();
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
    let field_bit_info2 = field_bit_info.clone();
    let calc_total_bit_size_fn = generate_calc_total_bit_size_fn(field_bit_info.len());

    // TODO: Method visibility determined by struct field visibility?
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

                    for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
                        let byte_index = bit_index / 8;
                        let mut byte = &mut field[byte_index];
                        let bit = 1 << i;
                        let read_bit = (int as usize) & bit;

                        if read_bit == 0 {
                            zero_bit(byte, (bit_index % 8) as u64);
                        } else {
                            one_bit(byte, (bit_index % 8) as u64);
                        }
                    }
                }

                /// This method allows you to read from a bitfield to a value
                pub fn #method_names(&self) -> #field_types {
                    type IntType = #field_types2;

                    trait BoolOrInt {
                        fn is_signed() -> bool;
                    }

                    /// This wrapper allows us to convert an int value
                    /// to the return type which may be an int or a bool.
                    /// We must use a wrapper because rust doesn't let
                    /// you impl std traits on std types.
                    struct Wrapper<T>(T);

                    macro_rules! impl_int {
                        ($($typ: ident),+) => {
                            $(
                                impl BoolOrInt for $typ {
                                    fn is_signed() -> bool {
                                        $typ::min_value() != 0
                                    }
                                }

                                impl Into<$typ> for Wrapper<$typ> {
                                    fn into(self) -> $typ {
                                        self.0
                                    }
                                }

                                impl Into<bool> for Wrapper<$typ> {
                                    fn into(self) -> bool {
                                        if self.0 != 0 {
                                            true
                                        } else {
                                            false
                                        }
                                    }
                                }
                            )+
                        };
                    }

                    impl_int!{u8, u16, u32, u64, u128, i8, i16, i32, i64, i128}

                    impl BoolOrInt for bool {
                        fn is_signed() -> bool {
                            false
                        }
                    }

                    let is_signed = <IntType as BoolOrInt>::is_signed();

                    let field = self.#field_names2;
                    let (lhs_bit, rhs_bit) = #field_bit_info2;
                    let mut val = 0;

                    for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
                        let byte_index = bit_index / 8;
                        let byte = field[byte_index];
                        let bit = 1 << (bit_index % 8);
                        let read_bit = byte & bit;

                        if read_bit != 0 {
                            let write_bit = 1 << i;

                            val |= write_bit;
                        }
                    }

                    // If the int type is signed, attempt to sign extend unconditionally
                    if is_signed {
                        #calc_total_bit_size_fn

                        let total_bit_size = calc_total_bit_size();
                        let bit_width = rhs_bit - lhs_bit + 1;
                        let unused_bits = total_bit_size - bit_width;

                        val <<= unused_bits;
                        val >>= unused_bits;
                    }

                    Wrapper(val).into()
                }
            )*
        }
    };

    Ok(q.into())
}

fn generate_calc_total_bit_size_fn(count: usize) -> Vec<__rt::TokenStream> {
    #[cfg(not(feature = "no_std"))]
    let quoted = quote! {
        fn calc_total_bit_size() -> usize {
            ::std::mem::size_of::<IntType>() * 8
        }
    };

    #[cfg(feature = "no_std")]
    let quoted = quote! {
        fn calc_total_bit_size() -> usize {
            ::core::mem::size_of::<IntType>() * 8
        }
    };

    vec![quoted; count]
}
