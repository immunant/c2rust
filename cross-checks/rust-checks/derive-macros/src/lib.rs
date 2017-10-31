extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use quote::ToTokens;

// Extract the optional tag from a #[cross_check(by_value(...))] attribute
fn get_direct_item_config(mi: &syn::MetaItem) -> (syn::Ident, quote::Tokens) {
    let mut tag_ident = syn::Ident::from("UNKNOWN_TAG");
    let mut filter_tokens = quote! { };
    if let syn::MetaItem::List(_, ref items) = *mi {
        items.iter().for_each(|item| {
            match *item {
                syn::NestedMetaItem::MetaItem(ref mi) => {
                    match *mi {
                        syn::MetaItem::NameValue(ref kw, ref val)
                            if kw == "tag" => match *val {
                                syn::Lit::Str(ref s, syn::StrStyle::Cooked) => {
                                    tag_ident = syn::Ident::from(s.clone());
                                },
                                _ => panic!("invalid tag value for by_value: {:?}", *val)
                            },

                        syn::MetaItem::NameValue(ref kw, ref val)
                            if kw == "filter" => match *val {
                                syn::Lit::Str(ref s, syn::StrStyle::Cooked) => {
                                    syn::Ident::from(s.clone()).to_tokens(&mut filter_tokens);
                                },
                                _ => panic!("invalid tag value for by_value: {:?}", *val)
                            },

                        _ => panic!("unknown item passed to by_value: {:?}", *mi)
                    }
                },
                _ => panic!("unknown item passed to by_value: {:?}", *item)
            }
        })
    }
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
                                    let (tag, filter) = get_direct_item_config(mi);
                                    return quote! { cross_check_value!(#tag, #filter(#f), __XCHA, __XCHS) }
                                },

                            syn::MetaItem::Word(ref kw) |
                            syn::MetaItem::List(ref kw, _)
                                if kw == "check_raw" => {
                                    let (tag, filter) = get_direct_item_config(mi);
                                    return quote! { cross_check_raw!(#tag, #filter(#f) as u64) }
                                },

                            syn::MetaItem::NameValue(ref kw, ref val)
                                if kw == "custom" => match *val {
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
