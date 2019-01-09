
extern crate syn;

use super::{ArgValue, ArgList};

impl<'a> ArgValue<'a> {
    pub fn get_str_ident(&self) -> syn::Ident {
        syn::Ident::from(self.as_str())
    }
}

impl<'a> ArgList<'a> {
    pub fn get_ident_arg<D>(&self, arg: &str, default: D) -> syn::Ident
        where syn::Ident: ::std::convert::From<D>
    {
        self.0.get(arg).map_or_else(|| syn::Ident::from(default),
                                    ArgValue::get_str_ident)
    }

}

pub fn get_item_args(mi: &syn::MetaItem) -> ArgList {
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

                                syn::Lit::Int(i, _) => (kw.as_ref(), ArgValue::Int(i.into())),

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
