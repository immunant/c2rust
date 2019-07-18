#[cfg(feature = "with-quote")]
extern crate quote;

#[cfg(feature = "parse-syn")]
pub mod syn;

#[cfg(feature = "parse-syntax")]
pub mod syntax;

use indexmap::IndexMap;

use std::cmp::Eq;
use std::hash::Hash;
use std::iter::FromIterator;
use std::ops::Deref;

#[cfg(feature = "with-quote")]
use self::quote::ToTokens;

#[derive(Debug)]
pub enum ArgValue<K: Hash + Eq> {
    Nothing,
    Str(String),
    Int(u128),
    List(ArgList<K>),
}

impl<K: Hash + Eq> ArgValue<K> {
    pub fn get_str(&self) -> Option<&str> {
        match *self {
            ArgValue::Str(ref s) => Some(&s[..]),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.get_str().expect("argument expects string value")
    }

    pub fn get_list(&self) -> Option<&ArgList<K>> {
        match *self {
            ArgValue::List(ref l) => Some(l),
            _ => None,
        }
    }

    pub fn as_list(&self) -> &ArgList<K> {
        self.get_list().expect("argument expects list value")
    }

    #[cfg(feature = "with-quote")]
    pub fn get_str_tokens(&self) -> self::quote::Tokens {
        let mut tokens = self::quote::Tokens::new();
        self.get_str_ident().to_tokens(&mut tokens);
        tokens
    }
}

type ArgListInnerMap<K> = IndexMap<K, ArgValue<K>>;

#[derive(Debug)]
pub struct ArgList<K: Hash + Eq>(ArgListInnerMap<K>);

impl<K: Hash + Eq> Deref for ArgList<K> {
    type Target = ArgListInnerMap<K>;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: Hash + Eq> IntoIterator for ArgList<K> {
    type Item = (K, ArgValue<K>);
    type IntoIter = indexmap::map::IntoIter<K, ArgValue<K>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K: Hash + Eq> FromIterator<(K, ArgValue<K>)> for ArgList<K> {
    #[inline]
    fn from_iter<T>(iter: T) -> ArgList<K>
    where
        T: IntoIterator<Item = (K, ArgValue<K>)>,
    {
        ArgList(iter.into_iter().collect())
    }
}

impl<K: Hash + Eq> ArgList<K> {
    #[inline]
    pub fn new() -> ArgList<K> {
        ArgList(ArgListInnerMap::new())
    }

    #[allow(dead_code)]
    fn from_map(m: ArgListInnerMap<K>) -> ArgList<K> {
        ArgList(m)
    }

    #[cfg(feature = "with-quote")]
    pub fn get_token_arg<D>(&self, arg: &K, default: D) -> self::quote::Tokens
    where
        self::quote::Tokens: ::std::convert::From<D>,
    {
        self.0.get(arg).map_or_else(
            || self::quote::Tokens::from(default),
            ArgValue::get_str_tokens,
        )
    }
}
