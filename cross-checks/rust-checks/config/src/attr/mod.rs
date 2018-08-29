
#[cfg(feature="with-quote")]
extern crate quote;

#[cfg(feature="parse-syn")]
pub mod syn;

#[cfg(feature="parse-syntax")]
pub mod syntax;

use std::collections::HashMap;
use std::ops::Deref;

#[cfg(feature="with-quote")]
use self::quote::ToTokens;

#[derive(Debug)]
pub enum ArgValue<'a> {
    Nothing,
    Str(String),
    Int(u128),
    List(ArgList<'a>),
}

impl<'a> ArgValue<'a> {
    pub fn get_str(&self) -> Option<&str> {
        match *self {
            ArgValue::Str(ref s) => Some(&s[..]),
            _ => None
        }
    }

    pub fn as_str(&self) -> &str {
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

    #[cfg(feature="with-quote")]
    pub fn get_str_tokens(&self) -> self::quote::Tokens {
        let mut tokens = self::quote::Tokens::new();
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

    #[cfg(feature="with-quote")]
    pub fn get_token_arg<D>(&self, arg: &str, default: D) -> self::quote::Tokens
            where self::quote::Tokens: ::std::convert::From<D> {
        self.0.get(arg).map_or_else(|| self::quote::Tokens::from(default),
                                    ArgValue::get_str_tokens)
    }
}
