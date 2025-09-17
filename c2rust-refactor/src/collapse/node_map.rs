//! `NodeMap` support for macro expansion/collapsing.
use rustc_ast::token::{Nonterminal, Token, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use rustc_data_structures::sync::Lrc;
use rustc_span::source_map::Span;
use std::collections::HashMap;

use crate::ast_manip::{AstEquiv, ListNodeIds, Visit};
use crate::node_map::NodeMap;

use super::mac_table::{InvocKind, MacTable};

/// Match up IDs of pre-expansion `Nonterminal` tokens with post-expansion AST nodes.  Matching is
/// performed by first checking for equal spans and then by comparing with `ast_equiv`.  This can
/// match multiple new IDs to a single old ID.
///
/// We need this to match up nodes across multiple rounds of macro expansion and collapsing.  The
/// first macro expansion step turns some sequences of tokens into actual nodes, and later
/// collapse/expand steps preserve those nodes by storing them as nonterminals.  Nonterminal ID
/// matching lets us track those nodes throughout the later rounds of this process.
pub fn match_nonterminal_ids(node_map: &mut NodeMap, mac_table: &MacTable) {
    for info in mac_table.invocations() {
        let mac = match info.invoc {
            InvocKind::Mac(mac) => mac,
            _ => continue,
        };

        // Find all nonterminals in the macro's input tokens.
        let mut span_map = HashMap::new();
        collect_nonterminals(mac.args.inner_tokens(), &mut span_map);

        // Match IDs of nonterminal nodes with IDs of their uses in the expanded AST.
        let mut v = NtUseVisitor {
            nts: &span_map,
            matched_ids: Vec::new(),
        };
        info.expanded.visit(&mut v);

        // Add the results to `node_map.pending_edges`.
        node_map.add_edges(&v.matched_ids);
    }
}

/// Get the span of the inner node of a nonterminal token.  Note we only need to handle nonterminal
/// kinds that have both spans and NodeIds.
fn nt_span(nt: &Nonterminal) -> Option<Span> {
    use rustc_ast::token::Nonterminal::*;
    Some(match nt {
        NtItem(ref i) => i.span,
        NtBlock(ref b) => b.span,
        NtStmt(ref s) => s.span,
        NtPat(ref p) => p.span,
        NtExpr(ref e) => e.span,
        NtTy(ref t) => t.span,
        _ => return None,
    })
}

fn collect_nonterminals(ts: TokenStream, span_map: &mut HashMap<Span, Lrc<Nonterminal>>) {
    for tt in ts.into_trees() {
        match tt {
            TokenTree::Token(
                Token {
                    kind: TokenKind::Interpolated(nt),
                    ..
                },
                _,
            ) => {
                if let Some(span) = nt_span(&nt) {
                    span_map.insert(span, nt.clone());
                }
            }
            TokenTree::Token(..) => {}
            TokenTree::Delimited(_, _, tts) => {
                collect_nonterminals(tts, span_map);
            }
        }
    }
}

struct NtUseVisitor<'a> {
    nts: &'a HashMap<Span, Lrc<Nonterminal>>,
    matched_ids: Vec<(NodeId, NodeId)>,
}

macro_rules! define_nt_use_visitor {
    ($( $visit_thing:ident, $walk_thing:ident, $NtThing:ident, $Thing:ty; )*) => {
        impl<'a, 'ast> Visitor<'ast> for NtUseVisitor<'a> {
            $( fn $visit_thing(&mut self, x: &'ast $Thing) {
                if let Some(nt) = self.nts.get(&x.span) {
                    match **nt {
                        Nonterminal::$NtThing(ref y) => {
                            if AstEquiv::ast_equiv(x, y) {
                                self.matched_ids.extend(
                                    x.list_node_ids().into_iter().zip(
                                        y.list_node_ids().into_iter()));
                                // No need to continue looking for IDs inside this node.
                                return;
                            }
                        },
                        _ => {},
                    }
                }
                visit::$walk_thing(self, x);
            } )*
        }
    };
}

define_nt_use_visitor! {
    visit_item, walk_item, NtItem, Item;
    visit_block, walk_block, NtBlock, Block;
    visit_stmt, walk_stmt, NtStmt, Stmt;
    visit_pat, walk_pat, NtPat, Pat;
    visit_expr, walk_expr, NtExpr, Expr;
    visit_ty, walk_ty, NtTy, Ty;
}
