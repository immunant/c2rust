use rustc_ast::ptr::P;
use rustc_ast::token::{
    BinOpToken, CommentKind, Delimiter, IdentIsRaw, InvisibleOrigin, Lit as TokenLit,
    LitKind as TokenLitKind, MetaVarKind, Nonterminal, NtExprKind, NtPatKind, Token, TokenKind,
};
use rustc_ast::tokenstream::{DelimSpacing, DelimSpan, Spacing, TokenTree};
use rustc_ast::*;
use rustc_span::source_map::Spanned;
use rustc_span::Ident;

pub trait AstName {
    fn ast_name(&self) -> String;
}

include!(concat!(env!("OUT_DIR"), "/ast_names_gen.inc.rs"));

impl<T: AstName + ?Sized> AstName for P<T> {
    fn ast_name(&self) -> String {
        <T as AstName>::ast_name(self)
    }
}

impl<T: AstName> AstName for Spanned<T> {
    fn ast_name(&self) -> String {
        self.node.ast_name()
    }
}
