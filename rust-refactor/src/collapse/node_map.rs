//! `NodeMap` support for macro expansion/collapsing.
use std::collections::HashMap;
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::parse::token::{Token, Nonterminal};
use syntax::tokenstream::{TokenStream, TokenTree};
use syntax::visit::{self, Visitor};

use ast_manip::{AstEquiv, Visit, ListNodeIds};
use node_map::NodeMap;

use super::mac_table::{MacTable, InvocKind};


/// Match up IDs of pre-expansion `Nonterminal` tokens with post-expansion AST nodes.  Matching is
/// performed by first checking for equal spans and then by comparing with `ast_equiv`.  This can
/// match multiple new IDs to a single old ID.
pub fn match_nonterminal_ids(node_map: &mut NodeMap, mac_table: &MacTable) {
    for info in mac_table.invocations() {
        let mac = match info.invoc {
            InvocKind::Mac(mac) => mac,
            _ => continue,
        };

        // Find all nonterminals in the macro's input tokens.
        let mut span_map = HashMap::new();
        collect_nonterminals(mac.node.tts.clone().into(), &mut span_map);

        // Match IDs of nonterminal nodes with IDs of their uses in the expanded AST.
        let mut v = NtUseVisitor { nts: &span_map, matched_ids: Vec::new() };
        info.expanded.visit(&mut v);

        // Add the results to `node_map.pending_edges`.
        node_map.add_edges(&v.matched_ids);
    }
}


/// Get the span of the inner node of a nonterminal token.  Note we only need to handle nonterminal
/// kinds that have both spans and NodeIds.
fn nt_span(nt: &Nonterminal) -> Option<Span> {
    use syntax::parse::token::Nonterminal::*;
    Some(match nt {
        NtItem(ref i) => i.span,
        NtBlock(ref b) => b.span,
        NtStmt(ref s) => s.span,
        NtPat(ref p) => p.span,
        NtExpr(ref e) => e.span,
        NtTy(ref t) => t.span,
        NtImplItem(ref ii) => ii.span,
        NtTraitItem(ref ti) => ti.span,
        NtForeignItem(ref fi) => fi.span,
        _ => return None,
    })
}

/// Get the NodeId of the inner node of a nonterminal token.  Note we only need to handle
/// nonterminal kinds that have both spans and NodeIds.
pub fn nt_id(nt: &Nonterminal) -> Option<NodeId> {
    use syntax::parse::token::Nonterminal::*;
    Some(match nt {
        NtItem(ref i) => i.id,
        NtBlock(ref b) => b.id,
        NtStmt(ref s) => s.id,
        NtPat(ref p) => p.id,
        NtExpr(ref e) => e.id,
        NtTy(ref t) => t.id,
        NtImplItem(ref ii) => ii.id,
        NtTraitItem(ref ti) => ti.id,
        NtForeignItem(ref fi) => fi.id,
        _ => return None,
    })
}

fn collect_nonterminals(ts: TokenStream, span_map: &mut HashMap<Span, Nonterminal>) {
    for tt in ts.into_trees() {
        match tt {
            TokenTree::Token(_, Token::Interpolated(nt_tts)) => {
                let nt = &nt_tts.0;
                if let (Some(span), Some(id)) = (nt_span(nt), nt_id(nt)) {
                    span_map.insert(span, nt.clone());
                }
            },
            TokenTree::Token(..) => {},
            TokenTree::Delimited(_, d) => {
                collect_nonterminals(d.tts.into(), span_map);
            },
        }
    }
}

struct NtUseVisitor<'a> {
    nts: &'a HashMap<Span, Nonterminal>,
    matched_ids: Vec<(NodeId, NodeId)>,
}

macro_rules! define_nt_use_visitor {
    ($( $visit_thing:ident, $walk_thing:ident, $NtThing:ident, $Thing:ty; )*) => {
        impl<'a, 'ast> Visitor<'ast> for NtUseVisitor<'a> {
            $( fn $visit_thing(&mut self, x: &'ast $Thing) {
                if let Some(nt) = self.nts.get(&x.span) {
                    match nt {
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
    visit_impl_item, walk_impl_item, NtImplItem, ImplItem;
    visit_trait_item, walk_trait_item, NtTraitItem, TraitItem;
    visit_foreign_item, walk_foreign_item, NtForeignItem, ForeignItem;
}
