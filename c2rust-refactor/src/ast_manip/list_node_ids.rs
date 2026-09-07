use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{IdentIsRaw, InvisibleOrigin, MetaVarKind, NtExprKind, NtPatKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::DelimSpacing;
use rustc_ast::tokenstream::{DelimSpan, LazyAttrTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::packed::Pu128;
use rustc_errors::ErrorGuaranteed;
use rustc_span::source_map::Spanned;
use rustc_span::Span;
use rustc_span::SyntaxContext;
use rustc_span::{Ident, Symbol};
use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use std::rc::Rc;
use std::sync::Arc;
use thin_vec::ThinVec;

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

impl ListNodeIds for FormatArgs {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        for argument in self.arguments.all_args() {
            argument.expr.add_node_ids(ids);
        }
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

impl ListNodeIds for std::borrow::Cow<'_, str> {
    fn add_node_ids(&self, _node_id_list: &mut Vec<NodeId>) {}
}

impl ListNodeIds for Result<(), ErrorGuaranteed> {
    fn add_node_ids(&self, _node_id_list: &mut Vec<NodeId>) {}
}

include!(concat!(env!("OUT_DIR"), "/list_node_ids_gen.inc.rs"));

impl<T: ListNodeIds + ?Sized> ListNodeIds for Arc<T> {
    fn add_node_ids(&self, ids: &mut Vec<NodeId>) {
        <T as ListNodeIds>::add_node_ids(self, ids)
    }
}
