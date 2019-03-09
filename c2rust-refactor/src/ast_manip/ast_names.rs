use syntax::ast::*;
use syntax::parse::token::Nonterminal;
use syntax::ptr::P;
use syntax::source_map::Spanned;
use syntax::tokenstream::{DelimSpan, Delimited, TokenTree};

pub trait AstName {
    fn ast_name(&self) -> String;
}

include!(concat!(env!("OUT_DIR"), "/ast_names_gen.inc.rs"));

impl<T: AstName> AstName for P<T> {
    fn ast_name(&self) -> String { <T as AstName>::ast_name(self) }
}

impl<T: AstName> AstName for Spanned<T> {
    fn ast_name(&self) -> String { self.node.ast_name() }
}
