use std::collections::HashSet;
use syntax::ast::*;
use syntax::ptr::P;

use super::mac_table::{MacTable, InvocId};


enum AnyMacNode {
    Expr(P<Expr>),
    Pat(P<Pat>),
    Ty(P<Ty>),

    Item(P<Item>),
    ImplItem(ImplItem),
    TraitItem(TraitItem),
    ForeignItem(ForeignItem),

    Stmt(Stmt),
}


struct CollapseMacros<'a> {
    mac_table: &'a MacTable<'a>,

    /// Used to keep track of which invocations we've seen, inside contexts where a macro might
    /// expand to multiple nodes.  The first node for each invocation gets replaced with the macro;
    /// the rest get discarded.
    seen_invocs: HashSet<InvocId>,

    token_rewrites: Vec<(InvocId, AnyMacNode)>,
}

impl<'a> Folder for CollapseMacros<'a> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(mac) = self.mac_table.get(e.id) {
        }
    }
}
