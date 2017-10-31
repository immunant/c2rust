extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use std::collections::{HashMap};

use quote::ToTokens;

enum ArgValue<'a> {
    Nothing,
    Str(String),
    List(ArgList<'a>),
}

impl<'a> ArgValue<'a> {
    fn get_str(&self) -> &String {
        match *self {
            ArgValue::Str(ref s) => s,
            _ => panic!("argument expects string value")
        }
    }

    fn get_list(&self) -> &ArgList<'a> {
        match *self {
            ArgValue::List(ref l) => l,
            _ => panic!("argument expects list value")
        }
    }
}

type ArgList<'a> = HashMap<&'a str, ArgValue<'a>>;

fn get_item_args(mi: &syn::MetaItem) -> ArgList {
    if let syn::MetaItem::List(_, ref items) = *mi {
        items.iter().map(|item| {
            match *item {
                syn::NestedMetaItem::MetaItem(ref mi) => {
                    match *mi {
                        syn::MetaItem::Word(ref kw) => (kw.as_ref(), ArgValue::Nothing),

                        syn::MetaItem::NameValue(ref kw, ref val) => {
                            match *val {
                                syn::Lit::Str(ref s, syn::StrStyle::Cooked) =>
                                    (kw.as_ref(), ArgValue::Str(s.clone())),

                                _ => panic!("invalid tag value for by_value: {:?}", *val)
                            }
                        },

                        syn::MetaItem::List(ref kw, _) => {
                            (kw.as_ref(), ArgValue::List(get_item_args(mi)))
                        }
                    }
                },
                _ => panic!("unknown item passed to by_value: {:?}", *item)
            }
        }).collect()
    } else {
        Default::default()
    }
}

// Extract the optional tag from a #[cross_check(by_value(...))] attribute
fn get_direct_item_config(args: &ArgList, default_filter_tokens: quote::Tokens)
        -> (syn::Ident, quote::Tokens) {
    // Process "tag = ..." argument
    let tag_ident = match args.get("tag") {
        Some(ref tag) => syn::Ident::from(tag.get_str().as_str()),
        None => syn::Ident::from("UNKNOWN_TAG")
    };
    // Process "filter = ..." argument
    let filter_tokens = match args.get("filter") {
        Some(ref filter) => {
            let mut new_tokens = quote::Tokens::new();
            syn::Ident::from(filter.get_str().as_str()).to_tokens(&mut new_tokens);
            new_tokens
        },
        None => default_filter_tokens,
    };
    (tag_ident, filter_tokens)
}

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|f| {
        let xcheck_attr = f.ast().attrs.iter().find(
            |f| f.name() == "cross_check");
        if let Some(ref attr) = xcheck_attr {
            let args = get_item_args(&attr.value);
            if args.contains_key("no") ||
               args.contains_key("never") ||
               args.contains_key("disable") {
                // Cross-checking is disabled
                return quote::Tokens::new();
            }
            if let Some(ref sub_args) = args.get("check_value") {
                // Cross-check field directly by value
                // This has an optional tag parameter (tag="NNN_TAG")
                let (tag, filter) = get_direct_item_config(sub_args.get_list(),
                                                           quote::Tokens::new());
                return quote! { cross_check_value!(#tag, (#filter(#f)), __XCHA, __XCHS) };
            }
            if let Some(ref sub_args) = args.get("check_raw") {
                let (tag, filter) = get_direct_item_config(sub_args.get_list(),
                                                           quote! { * });
                return quote! { cross_check_raw!(#tag, (#filter(#f)) as u64) }
            }
            if let Some(ref sub_arg) = args.get("custom_hash") {
                let id = syn::Ident::from(sub_arg.get_str().as_str());
                return quote! { #id(&mut h, #f) };
            }
        }
        // Default implementation
        quote! {
            use cross_check_runtime::hash::CrossCheckHash;
            h.write_u64(CrossCheckHash::cross_check_hash_depth::<__XCHA, __XCHS>(#f, _depth - 1));
        }
    });
    s.bound_impl("::cross_check_runtime::hash::CrossCheckHash", quote! {
        fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                where __XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
                      __XCHS: ::cross_check_runtime::hash::CrossCheckHasher {
            let mut h = __XCHA::default();
            match *self { #hash_fields }
            h.finish()
        }
    })
}
decl_derive!([CrossCheckHash] => xcheck_hash_derive);
