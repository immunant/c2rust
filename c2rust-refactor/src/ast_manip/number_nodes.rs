use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::token::{Nonterminal, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_ast::{MacCall, NodeId, DUMMY_NODE_ID};
use rustc_data_structures::sync::Lrc;
use std::cell::Cell;

use crate::ast_manip::MutVisit;

#[derive(Clone)]
pub struct NodeIdCounter(Cell<u32>);

impl NodeIdCounter {
    pub fn new(start: u32) -> NodeIdCounter {
        NodeIdCounter(Cell::new(start))
    }

    pub fn next(&self) -> NodeId {
        let id = NodeId::from_u32(self.0.get());
        self.0.set(self.0.get() + 1);
        id
    }
}

struct NumberNodes<'a> {
    counter: &'a NodeIdCounter,
}

impl<'a> MutVisitor for NumberNodes<'a> {
    fn visit_id(&mut self, i: &mut NodeId) {
        *i = self.counter.next()
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self);
    }
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes<T: MutVisit>(x: &mut T) {
    // 0 is a valid node id.  DUMMY_NODE_ID is -1.
    number_nodes_with(x, &NodeIdCounter::new(0))
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes_with<T: MutVisit>(x: &mut T, counter: &NodeIdCounter) {
    x.visit(&mut NumberNodes { counter })
}

struct ResetNodeIds;

fn reset_nonterminal_node_ids(nt: &mut Nonterminal, visitor: &mut ResetNodeIds) {
    match nt {
        Nonterminal::NtItem(item) => item.visit(visitor),
        Nonterminal::NtBlock(block) => block.visit(visitor),
        Nonterminal::NtStmt(stmt) => stmt.visit(visitor),
        Nonterminal::NtPat(pat) => pat.visit(visitor),
        Nonterminal::NtExpr(expr) | Nonterminal::NtLiteral(expr) => expr.visit(visitor),
        Nonterminal::NtPath(path) => path.visit(visitor),
        Nonterminal::NtTy(ty) => ty.visit(visitor),
        Nonterminal::NtVis(vis) => vis.visit(visitor),
        Nonterminal::NtIdent(..) | Nonterminal::NtLifetime(..) | Nonterminal::NtMeta(..) => {}
    }
}

fn reset_interpolated_node_ids(tokens: TokenStream, visitor: &mut ResetNodeIds) -> TokenStream {
    tokens
        .into_trees()
        .map(|tree| match tree {
            TokenTree::Token(mut token, spacing) => {
                if let TokenKind::Interpolated(nt) = &token.kind {
                    let mut nt = (**nt).clone();
                    reset_nonterminal_node_ids(&mut nt, visitor);
                    token.kind = TokenKind::Interpolated(Lrc::new(nt));
                }
                TokenTree::Token(token, spacing)
            }
            TokenTree::Delimited(span, delimiter, tokens) => TokenTree::Delimited(
                span,
                delimiter,
                reset_interpolated_node_ids(tokens, visitor),
            ),
        })
        .collect()
}

impl MutVisitor for ResetNodeIds {
    fn visit_id(&mut self, i: &mut NodeId) {
        *i = DUMMY_NODE_ID;
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self);
        // Macro collapsing preserves transformed arguments as interpolated
        // nonterminals. They are not visited by rustc's token-stream walker,
        // but their IDs must be reset along with the surrounding AST before
        // the next compiler session assigns a fresh ID space.
        mac.args.tokens = reset_interpolated_node_ids(mac.args.tokens.clone(), self);
    }
}

pub fn reset_node_ids<T: MutVisit>(x: &mut T) {
    x.visit(&mut ResetNodeIds)
}
