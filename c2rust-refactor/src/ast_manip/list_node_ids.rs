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
use smallvec::SmallVec;
use std::rc::Rc;

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

impl<T: ListNodeIds + ?Sized> ListNodeIds for P<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds + ?Sized> ListNodeIds for Box<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(self, ids)
    }
}

impl<T: ListNodeIds + ?Sized> ListNodeIds for Rc<T> {
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
