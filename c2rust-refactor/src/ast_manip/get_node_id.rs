//! `GetNodeId` trait for obtaining the `NodeId` of a generic AST node.
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

/// Trait for obtaining the `NodeId` of a generic AST node.
pub trait GetNodeId {
    fn get_node_id(&self) -> NodeId;
}

impl<T: GetNodeId + ?Sized> GetNodeId for P<T> {
    fn get_node_id(&self) -> NodeId {
        <T as GetNodeId>::get_node_id(self)
    }
}

pub trait MaybeGetNodeId {
    fn supported() -> bool {
        false
    }
    fn get_node_id(&self) -> NodeId {
        DUMMY_NODE_ID
    }
}

impl<T: MaybeGetNodeId + ?Sized> MaybeGetNodeId for P<T> {
    fn supported() -> bool {
        <T as MaybeGetNodeId>::supported()
    }
    fn get_node_id(&self) -> NodeId {
        <T as MaybeGetNodeId>::get_node_id(self)
    }
}

impl<T: MaybeGetNodeId + ?Sized> MaybeGetNodeId for Rc<T> {
    fn supported() -> bool {
        <T as MaybeGetNodeId>::supported()
    }
    fn get_node_id(&self) -> NodeId {
        <T as MaybeGetNodeId>::get_node_id(self)
    }
}

impl<T> MaybeGetNodeId for Spanned<T> {}
impl<T> MaybeGetNodeId for Option<T> {}
impl<A, B> MaybeGetNodeId for (A, B) {}
impl<A, B, C> MaybeGetNodeId for (A, B, C) {}

impl<T> MaybeGetNodeId for [T] {}
impl<T> MaybeGetNodeId for Vec<T> {}
impl<T> MaybeGetNodeId for ThinVec<T> {}

include!(concat!(env!("OUT_DIR"), "/get_node_id_gen.inc.rs"));
