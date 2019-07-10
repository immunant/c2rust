use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use std::rc::Rc;
use syntax::ast::*;
use syntax::parse::token::{DelimToken, Nonterminal, Token};
use syntax::ptr::P;
use syntax::source_map::{Span, Spanned};
use syntax::tokenstream::{DelimSpan, TokenStream, TokenTree};
use syntax::ThinVec;
use syntax_pos::hygiene::SyntaxContext;

pub trait ListNodeIds {
    fn list_node_ids(&self) -> Vec<NodeId> {
        let mut ids = Vec::new();
        self.add_node_ids(&mut ids);
        ids
    }

    fn add_node_ids(&self, ids: &mut Vec<NodeId>);
}

impl ListNodeIds for NodeId {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        ids.push(*self);
    }
}

impl<T: ListNodeIds> ListNodeIds for P<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for Rc<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for Spanned<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(&self.node, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for [T] {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        for x in self.iter() {
            <T as ListNodeIds>::add_node_ids(x, ids)
        }
    }
}

impl<T: ListNodeIds> ListNodeIds for Vec<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <[T] as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for SmallVec<[T; 1]> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <[T] as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for ThinVec<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <[T] as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds> ListNodeIds for Option<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        if let Some(ref x) = *self {
            <T as ListNodeIds>::add_node_ids(x, ids);
        }
    }
}

impl<A: ListNodeIds, B: ListNodeIds> ListNodeIds for (A, B) {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        self.0.add_node_ids(ids);
        self.1.add_node_ids(ids);
    }
}

impl<A: ListNodeIds, B: ListNodeIds, C: ListNodeIds> ListNodeIds for (A, B, C) {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        self.0.add_node_ids(ids);
        self.1.add_node_ids(ids);
        self.2.add_node_ids(ids);
    }
}

include!(concat!(env!("OUT_DIR"), "/list_node_ids_gen.inc.rs"));
