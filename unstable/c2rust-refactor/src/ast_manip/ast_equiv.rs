//! `AstEquiv` trait for checking equivalence of two ASTs.
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
use std::rc::Rc;

/// Trait for checking equivalence of AST nodes.  This is similar to `PartialEq`, but less strict,
/// as it ignores some fields that have no bearing on the semantics of the AST (particularly
/// `Span`s and `NodeId`s).
pub trait AstEquiv {
    fn ast_equiv(&self, other: &Self) -> bool;

    /// Checks for structural and name equivalence of AST nodes, while ignoring
    /// any difference in the names of C2RustUnnamed* types
    fn unnamed_equiv(&self, other: &Self) -> bool;
}

impl<'a, T: AstEquiv> AstEquiv for &'a T {
    fn ast_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::ast_equiv(*self, *other)
    }
    fn unnamed_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::unnamed_equiv(*self, *other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for P<T> {
    fn ast_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for Box<T> {
    fn ast_equiv(&self, other: &Box<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Box<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv + ?Sized> AstEquiv for Rc<T> {
    fn ast_equiv(&self, other: &Rc<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Rc<T>) -> bool {
        <T as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Spanned<T> {
    fn ast_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.ast_equiv(&other.node)
    }
    fn unnamed_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.unnamed_equiv(&other.node)
    }
}

impl<T: AstEquiv> AstEquiv for [T] {
    fn ast_equiv(&self, other: &[T]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.ast_equiv(r) {
                return false;
            }
        }
        true
    }
    fn unnamed_equiv(&self, other: &[T]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.unnamed_equiv(r) {
                return false;
            }
        }
        true
    }
}

impl<T: AstEquiv> AstEquiv for Vec<T> {
    fn ast_equiv(&self, other: &Vec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &Vec<T>) -> bool {
        <[T] as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for ThinVec<T> {
    fn ast_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
    }
    fn unnamed_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::unnamed_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Option<T> {
    fn ast_equiv(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref x), &Some(ref y)) => x.ast_equiv(y),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
    fn unnamed_equiv(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref x), &Some(ref y)) => x.unnamed_equiv(y),
            (&None, &None) => true,
            (_, _) => false,
        }
    }
}

impl<A: AstEquiv, B: AstEquiv> AstEquiv for (A, B) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) && self.1.ast_equiv(&other.1)
    }
    fn unnamed_equiv(&self, other: &Self) -> bool {
        self.0.unnamed_equiv(&other.0) && self.1.unnamed_equiv(&other.1)
    }
}

impl<A: AstEquiv, B: AstEquiv, C: AstEquiv> AstEquiv for (A, B, C) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) && self.1.ast_equiv(&other.1) && self.2.ast_equiv(&other.2)
    }
    fn unnamed_equiv(&self, other: &Self) -> bool {
        self.0.unnamed_equiv(&other.0)
            && self.1.unnamed_equiv(&other.1)
            && self.2.unnamed_equiv(&other.2)
    }
}

// Implementations for specific AST types are auto-generated.
include!(concat!(env!("OUT_DIR"), "/ast_equiv_gen.inc.rs"));

impl AstEquiv for Ident {
    fn ast_equiv(&self, other: &Self) -> bool {
        // Exhaustiveness check
        match self {
            &Ident {
                name: ref _name,
                span: ref _span,
            } => {}
        }

        // Comparison
        match (self, other) {
            (
                &Ident {
                    name: ref name1,
                    span: ref span1,
                },
                &Ident {
                    name: ref name2,
                    span: ref span2,
                },
            ) => AstEquiv::ast_equiv(name1, name2) && AstEquiv::ast_equiv(span1, span2) && true,
        }
    }

    fn unnamed_equiv(&self, other: &Self) -> bool {
        (self.as_str().contains("C2RustUnnamed") && other.as_str().contains("C2RustUnnamed"))
            || self.ast_equiv(other)
    }
}
