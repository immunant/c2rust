//! `AstEquiv` trait for checking equivalence of two ASTs.
use std::rc::Rc;
use rustc_target::spec::abi::Abi;
use syntax::ThinVec;
use syntax::ast::*;
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::ptr::P;
use syntax::source_map::{Span, Spanned};
use syntax::tokenstream::{TokenTree, Delimited, DelimSpan, TokenStream};
use syntax_pos::hygiene::SyntaxContext;

/// Trait for checking equivalence of AST nodes.  This is similar to `PartialEq`, but less strict,
/// as it ignores some fields that have no bearing on the semantics of the AST (particularly
/// `Span`s and `NodeId`s).
pub trait AstEquiv {
    fn ast_equiv(&self, other: &Self) -> bool;
}


impl<'a, T: AstEquiv> AstEquiv for &'a T {
    fn ast_equiv(&self, other: &&'a T) -> bool {
        <T as AstEquiv>::ast_equiv(*self, *other)
    }
}

impl<T: AstEquiv> AstEquiv for P<T> {
    fn ast_equiv(&self, other: &P<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Rc<T> {
    fn ast_equiv(&self, other: &Rc<T>) -> bool {
        <T as AstEquiv>::ast_equiv(self, other)
    }
}

impl<T: AstEquiv> AstEquiv for Spanned<T> {
    fn ast_equiv(&self, other: &Spanned<T>) -> bool {
        self.node.ast_equiv(&other.node)
    }
}

impl<T: AstEquiv> AstEquiv for [T] {
    fn ast_equiv(&self, other: &[T]) -> bool {
        for (l, r) in self.iter().zip(other.iter()) {
            if !l.ast_equiv(r) {
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
}

impl<T: AstEquiv> AstEquiv for ThinVec<T> {
    fn ast_equiv(&self, other: &ThinVec<T>) -> bool {
        <[T] as AstEquiv>::ast_equiv(self, other)
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
}

impl<A: AstEquiv, B: AstEquiv> AstEquiv for (A, B) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) &&
        self.1.ast_equiv(&other.1)
    }
}

impl<A: AstEquiv, B: AstEquiv, C: AstEquiv> AstEquiv for (A, B, C) {
    fn ast_equiv(&self, other: &Self) -> bool {
        self.0.ast_equiv(&other.0) &&
        self.1.ast_equiv(&other.1) &&
        self.2.ast_equiv(&other.2)
    }
}


// Implementations for specific AST types are auto-generated.
include!(concat!(env!("OUT_DIR"), "/ast_equiv_gen.inc.rs"));
