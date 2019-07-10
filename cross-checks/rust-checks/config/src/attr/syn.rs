extern crate syn;

use super::{ArgList, ArgValue};

use std::cmp::Eq;
use std::hash::Hash;

impl<K: Hash + Eq> ArgValue<K> {
    pub fn get_str_ident(&self) -> syn::Ident {
        syn::Ident::from(self.as_str())
    }
}

impl<K: Hash + Eq> ArgList<K> {
    pub fn get_ident_arg<D, Q>(&self, arg: &Q, default: D) -> syn::Ident
    where
        Q: ?Sized + Hash + indexmap::Equivalent<K>,
        syn::Ident: ::std::convert::From<D>,
    {
        self.0
            .get(arg)
            .map_or_else(|| syn::Ident::from(default), ArgValue::get_str_ident)
    }
}

pub fn get_item_args(mi: &syn::MetaItem) -> ArgList<String> {
    if let syn::MetaItem::List(_, ref items) = *mi {
        ArgList::from_map(
            items
                .iter()
                .map(|item| match *item {
                    syn::NestedMetaItem::MetaItem(ref mi) => match *mi {
                        syn::MetaItem::Word(ref kw) => (kw.to_string(), ArgValue::Nothing),

                        syn::MetaItem::NameValue(ref kw, ref val) => match *val {
                            syn::Lit::Str(ref s, syn::StrStyle::Cooked) => {
                                (kw.to_string(), ArgValue::Str(s.clone()))
                            }

                            syn::Lit::Int(i, _) => (kw.to_string(), ArgValue::Int(i.into())),

                            _ => panic!("invalid tag value for by_value: {:?}", *val),
                        },

                        syn::MetaItem::List(ref kw, _) => {
                            (kw.to_string(), ArgValue::List(get_item_args(mi)))
                        }
                    },
                    _ => panic!("unknown item passed to by_value: {:?}", *item),
                })
                .collect(),
        )
    } else {
        ArgList::new()
    }
}
