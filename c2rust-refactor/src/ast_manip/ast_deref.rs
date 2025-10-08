use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{Span, Spanned};
use rustc_span::symbol::{Ident, Symbol};
use rustc_target::spec::abi::Abi;

pub trait AstDeref {
    type Target: ?Sized;
    fn ast_deref(&self) -> &Self::Target;
}

include!(concat!(env!("OUT_DIR"), "/ast_deref_gen.inc.rs"));

impl<T: AstDeref + ?Sized> AstDeref for P<T> {
    type Target = <T as AstDeref>::Target;
    fn ast_deref(&self) -> &Self::Target {
        <T as AstDeref>::ast_deref(self)
    }
}

impl<T: AstDeref> AstDeref for Spanned<T> {
    type Target = <T as AstDeref>::Target;
    fn ast_deref(&self) -> &Self::Target {
        self.node.ast_deref()
    }
}

impl<T> AstDeref for Vec<T> {
    type Target = [T];
    fn ast_deref(&self) -> &Self::Target {
        &**self
    }
}

impl<T> AstDeref for ThinVec<T> {
    type Target = [T];
    fn ast_deref(&self) -> &Self::Target {
        &**self
    }
}
