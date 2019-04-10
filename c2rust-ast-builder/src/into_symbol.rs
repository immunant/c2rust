use syntax::symbol::keywords::Keyword;
use syntax::symbol::{InternedString, Symbol};

/// Conversion of string-like values into interned `Symbol`s.
pub trait IntoSymbol {
    fn into_symbol(self) -> Symbol;
}

impl IntoSymbol for Symbol {
    fn into_symbol(self) -> Symbol {
        self
    }
}

impl<'a> IntoSymbol for &'a str {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(self)
    }
}

impl IntoSymbol for String {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(&self)
    }
}

impl<'a> IntoSymbol for &'a String {
    fn into_symbol(self) -> Symbol {
        <&str as IntoSymbol>::into_symbol(self)
    }
}

impl IntoSymbol for InternedString {
    fn into_symbol(self) -> Symbol {
        self.as_symbol()
    }
}

impl IntoSymbol for Keyword {
    fn into_symbol(self) -> Symbol {
        self.name()
    }
}
