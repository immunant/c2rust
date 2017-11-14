
#[cfg(feature="parse-syn")]
extern crate syn;

#[cfg(feature="parse-syntax")]
extern crate syntax;

use std::collections::HashMap;
use std::ops::Deref;

#[cfg(feature="parse-syntax")]
use self::syntax::ast;

#[cfg(feature="with-quote")]
use quote::ToTokens;

#[derive(Debug)]
pub enum ArgValue<'a> {
    Nothing,
    Str(String),
    List(ArgList<'a>),
}

impl<'a> ArgValue<'a> {
    pub fn get_str(&self) -> Option<&String> {
        match *self {
            ArgValue::Str(ref s) => Some(s),
            _ => None
        }
    }

    pub fn as_str(&self) -> &String {
        self.get_str().expect("argument expects string value")
    }

    pub fn get_list(&self) -> Option<&ArgList<'a>> {
        match *self {
            ArgValue::List(ref l) => Some(l),
            _ => None
        }
    }

    pub fn as_list(&self) -> &ArgList<'a> {
        self.get_list().expect("argument expects list value")
    }

    #[cfg(feature="parse-syn")]
    pub fn get_str_ident(&self) -> syn::Ident {
        syn::Ident::from(self.as_str().as_str())
    }

    #[cfg(feature="with-quote")]
    pub fn get_str_tokens(&self) -> quote::Tokens {
        let mut tokens = quote::Tokens::new();
        self.get_str_ident().to_tokens(&mut tokens);
        tokens
    }
}

#[derive(Debug, Default)]
pub struct ArgList<'a>(HashMap<&'a str, ArgValue<'a>>);

impl<'a> Deref for ArgList<'a> {
    type Target = HashMap<&'a str, ArgValue<'a>>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl<'a> ArgList<'a> {
    #[allow(dead_code)]
    fn from_map(m: HashMap<&'a str, ArgValue<'a>>) -> ArgList<'a> {
        ArgList(m)
    }

    #[cfg(feature="parse-syn")]
    pub fn get_ident_arg<D>(&self, arg: &str, default: D) -> syn::Ident
            where syn::Ident: ::std::convert::From<D> {
        self.0.get(arg).map_or_else(|| syn::Ident::from(default),
                                    ArgValue::get_str_ident)
    }

    #[cfg(feature="with-quote")]
    pub fn get_token_arg<D>(&self, arg: &str, default: D) -> quote::Tokens
            where quote::Tokens: ::std::convert::From<D> {
        self.0.get(arg).map_or_else(|| quote::Tokens::from(default),
                                    ArgValue::get_str_tokens)
    }
}

#[cfg(feature="parse-syn")]
pub fn get_syn_item_args(mi: &syn::MetaItem) -> ArgList {
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
                            (kw.as_ref(), ArgValue::List(get_syn_item_args(mi)))
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

#[cfg(feature="parse-syntax")]
pub fn get_syntax_item_args(mi: &ast::MetaItem) -> ArgList<'static> {
    if let Some(ref items) = mi.meta_item_list() {
        ArgList::from_map(items.iter().map(|item| {
            match item.node {
                ast::NestedMetaItemKind::MetaItem(ref mi) => {
                    let kw = unsafe {
                        let kw_str = mi.name.interned().as_str();
                        // FIXME: this looks unsafe, but mi.name.as_str()
                        // returns an InternedString whose sole member is
                        // a &'static str (which we're forcing the conversion to)
                        // Ideally, InternedString's as_ref() or deref() would
                        // correctly return a &'static str reference
                        ::std::mem::transmute::<&str, &'static str>(kw_str.as_ref())
                    };
                    match mi.node {
                        ast::MetaItemKind::Word => (kw, ArgValue::Nothing),

                        ast::MetaItemKind::NameValue(ref val) => {
                            match val.node {
                                ast::LitKind::Str(ref s, ast::StrStyle::Cooked) =>
                                    (kw, ArgValue::Str(String::from(&*s.as_str()))),

                                _ => panic!("invalid tag value for by_value: {:?}", *val)
                            }
                        },

                        ast::MetaItemKind::List(_) => {
                            (kw, ArgValue::List(get_syntax_item_args(mi)))
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
