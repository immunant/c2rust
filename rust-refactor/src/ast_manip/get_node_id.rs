//! `GetNodeId` trait for obtaining the `NodeId` of a generic AST node.
use std::rc::Rc;
use rustc_target::spec::abi::Abi;
use syntax::ast::*;
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::ptr::P;
use syntax::source_map::{Span, Spanned};
use syntax::tokenstream::{TokenTree, Delimited, TokenStream, ThinTokenStream};
use syntax_pos::hygiene::SyntaxContext;


/// Trait for obtaining the `NodeId` of a generic AST node.
pub trait GetNodeId {
    fn get_node_id(&self) -> NodeId;
}

impl<T: GetNodeId> GetNodeId for P<T> {
    fn get_node_id(&self) -> NodeId {
        <T as GetNodeId>::get_node_id(self)
    }
}

pub trait MaybeGetNodeId {
    fn supported() -> bool { false }
    fn get_node_id(&self) -> NodeId { DUMMY_NODE_ID }
}

impl<T: MaybeGetNodeId> MaybeGetNodeId for P<T> {
    fn supported() -> bool { <T as MaybeGetNodeId>::supported() }
    fn get_node_id(&self) -> NodeId { <T as MaybeGetNodeId>::get_node_id(self) }
}

impl<T: MaybeGetNodeId> MaybeGetNodeId for Rc<T> {
    fn supported() -> bool { <T as MaybeGetNodeId>::supported() }
    fn get_node_id(&self) -> NodeId { <T as MaybeGetNodeId>::get_node_id(self) }
}

impl<T> MaybeGetNodeId for Spanned<T> {}
impl<T> MaybeGetNodeId for Option<T> {}
impl<A, B> MaybeGetNodeId for (A, B) {}
impl<A, B, C> MaybeGetNodeId for (A, B, C) {}

impl<T> MaybeGetNodeId for [T] {}
impl<T> MaybeGetNodeId for Vec<T> {}
impl<T> MaybeGetNodeId for ThinVec<T> {}

include!(concat!(env!("OUT_DIR"), "/get_node_id_gen.inc.rs"));
