extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

use std::collections::{HashMap};

use quote::ToTokens;

#[derive(Debug)]
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

    fn get_str_ident(&self) -> syn::Ident {
        syn::Ident::from(self.get_str().as_str())
    }

    #[allow(dead_code)] // Not used right now, but we might need it
    fn get_str_tokens(&self) -> quote::Tokens {
        let mut tokens = quote::Tokens::new();
        self.get_str_ident().to_tokens(&mut tokens);
        tokens
    }

    #[allow(dead_code)] // Not used right now, but we might need it
    fn get_list(&self) -> &ArgList<'a> {
        match *self {
            ArgValue::List(ref l) => l,
            _ => panic!("argument expects list value")
        }
    }
}

#[derive(Debug, Default)]
struct ArgList<'a>(HashMap<&'a str, ArgValue<'a>>);

impl<'a> ArgList<'a> {
    fn from_map(m: HashMap<&'a str, ArgValue<'a>>) -> ArgList<'a> {
        ArgList(m)
    }

    fn get_ident_arg<D>(&self, arg: &str, default: D) -> syn::Ident
            where syn::Ident: std::convert::From<D> {
        self.0.get(arg).map_or_else(|| syn::Ident::from(default),
                                    ArgValue::get_str_ident)
    }

    #[allow(dead_code)] // Not used right now, but we might need it
    fn get_token_arg<D>(&self, arg: &str, default: D) -> quote::Tokens
            where quote::Tokens: std::convert::From<D> {
        self.0.get(arg).map_or_else(|| quote::Tokens::from(default),
                                    ArgValue::get_str_tokens)
    }
}

fn get_item_args(mi: &syn::MetaItem) -> ArgList {
    if let syn::MetaItem::List(_, ref items) = *mi {
        ArgList::from_map(items.iter().map(|item| {
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
        }).collect())
    } else {
        Default::default()
    }
}

fn get_cross_check_args(attrs: &[syn::Attribute]) -> Option<ArgList> {
    attrs.iter()
         .find(|f| f.name() == "cross_check_hash")
         .map(|attr| get_item_args(&attr.value))
}

fn xcheck_hash_derive(s: synstructure::Structure) -> quote::Tokens {
    let top_args = get_cross_check_args(&s.ast().attrs[..]).unwrap_or_default();

    // Allow users to override __XCHA and __XCHS
    let ahasher = top_args.get_ident_arg("ahasher", "__XCHA");
    let shasher = top_args.get_ident_arg("shasher", "__XCHS");

    // Iterate through all fields, inserting the hash computation for each field
    let hash_fields = s.each(|f| {
        get_cross_check_args(&f.ast().attrs[..]).and_then(|args| {
            // FIXME: figure out the argument priorities here
            if args.0.contains_key("no") ||
               args.0.contains_key("never") ||
               args.0.contains_key("disable") {
                // Cross-checking is disabled
                Some(quote::Tokens::new())
            } else if let Some(ref sub_arg) = args.0.get("fixed_hash") {
                // FIXME: should try parsing this as an integer
                let id = sub_arg.get_str_ident();
                Some(quote! { h.write_u64(#id) })
            } else if let Some(ref sub_arg) = args.0.get("custom_hash") {
                let id = sub_arg.get_str_ident();
                Some(quote! { #id::<#ahasher, #shasher, Self>(&mut h, self, #f, _depth) })
            } else {
                None
            }
        }).unwrap_or_else(|| {
            // Default implementation
            quote! {
                use cross_check_runtime::hash::CrossCheckHash;
                h.write_u64(CrossCheckHash::cross_check_hash_depth::<#ahasher, #shasher>(#f, _depth - 1));
            }
        })
    });

    let hash_code = top_args.0.get("custom_hash").map(|sub_arg| {
        // Hash this value by calling the specified function
        let id = sub_arg.get_str_ident();
        quote! { #id::<#ahasher, #shasher>(&self, _depth) }
    }).unwrap_or_else(|| {
        // Hash this value using the default algorithm
        let hasher = top_args.get_ident_arg("field_hasher", ahasher);
        quote! {
            let mut h = #hasher::default();
            match *self { #hash_fields }
            h.finish()
        }
    });
    s.bound_impl("::cross_check_runtime::hash::CrossCheckHash", quote! {
        fn cross_check_hash_depth<__XCHA, __XCHS>(&self, _depth: usize) -> u64
                where __XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
                      __XCHS: ::cross_check_runtime::hash::CrossCheckHasher {
            #[allow(unused_imports)] use std::hash::Hasher;
            #hash_code
        }
    })
}
decl_derive!([CrossCheckHash, attributes(cross_check_hash)] => xcheck_hash_derive);
