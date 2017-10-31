extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use std::collections::{HashSet, HashMap};

use quote::ToTokens;

fn get_item_args(mi: &syn::MetaItem, args: HashSet<&'static str>)
        -> HashMap<&'static str, Option<String>> {
    if let syn::MetaItem::List(_, ref items) = *mi {
        items.iter().map(|item| {
            match *item {
                syn::NestedMetaItem::MetaItem(ref mi) => {
                    match *mi {
                        syn::MetaItem::Word(ref kw) => (kw.as_ref(), None),

                        syn::MetaItem::NameValue(ref kw, ref val) => {
                            match *val {
                                syn::Lit::Str(ref s, syn::StrStyle::Cooked) =>
                                    (kw.as_ref(), Some(s.clone())),

                                _ => panic!("invalid tag value for by_value: {:?}", *val)
                            }
                        },

                        _ => panic!("unknown item passed to by_value: {:?}", *mi)
                    }
                },
                _ => panic!("unknown item passed to by_value: {:?}", *item)
            }
        }).map(|(key, val)| {
            let key_ref = args.get(key)
                .expect(&format!("unknown #[cross_check(...)] argument: {}", key));
            (*key_ref, val)
        }).collect()
    } else {
        Default::default()
    }
}

// Extract the optional tag from a #[cross_check(by_value(...))] attribute
fn get_direct_item_config(mi: &syn::MetaItem, default_filter_tokens: quote::Tokens)
        -> (syn::Ident, quote::Tokens) {
    const DIRECT_ARGS: &[&str] = &["tag", "filter"];
    let args = get_item_args(mi, DIRECT_ARGS.iter().cloned().collect());
    // Process "tag = ..." argument
    let tag_ident = match args.get("tag") {
        Some(&Some(ref tag)) => syn::Ident::from(tag.clone()),
        Some(&None) => panic!("tag argument expects value"),
        None => syn::Ident::from("UNKNOWN_TAG")
    };
    // Process "filter = ..." argument
    let filter_tokens = match args.get("filter") {
        Some(&Some(ref filter)) => {
            let mut new_tokens = quote::Tokens::new();
            syn::Ident::from(filter.clone()).to_tokens(&mut new_tokens);
            new_tokens
        },
        Some(&None) => panic!("filter argument expects value"),
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
            if let syn::MetaItem::List(ref ident, ref items) = attr.value {
                assert!(ident == "cross_check");
                for item in items {
                    if let &syn::NestedMetaItem::MetaItem(ref mi) = item {
                        match *mi {
                            syn::MetaItem::Word(ref kw)
                                if kw == "no" ||
                                   kw == "never" ||
                                   kw == "disable" => return quote! {},

                            // Cross-check field directly by value
                            // This has an optional tag parameter (tag="NNN_TAG")
                            syn::MetaItem::Word(ref kw) |
                            syn::MetaItem::List(ref kw, _)
                                if kw == "check_value" => {
                                    let (tag, filter) = get_direct_item_config(mi, quote::Tokens::new());
                                    return quote! { cross_check_value!(#tag, (#filter(#f)), __XCHA, __XCHS) }
                                },

                            syn::MetaItem::Word(ref kw) |
                            syn::MetaItem::List(ref kw, _)
                                if kw == "check_raw" => {
                                    let (tag, filter) = get_direct_item_config(mi, quote! { * });
                                    return quote! { cross_check_raw!(#tag, (#filter(#f)) as u64) }
                                },

                            syn::MetaItem::NameValue(ref kw, ref val)
                                if kw == "custom_hash" => match *val {
                                    syn::Lit::Str(ref s, syn::StrStyle::Cooked) => {
                                        let id = syn::Ident::from(s.clone());
                                        return quote! { #id(&mut h, #f) };
                                    }
                                    _ => panic!("invalid identifier passed to #[cross_check(custom = ...)]: {:?}", *val)
                            }

                            _ => panic!("unknown parameter for #[cross_check]: {:?}", *mi)
                        }
                    }
                }
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
