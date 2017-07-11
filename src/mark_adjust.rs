//! This module implements commands for manipulating the current set of marked nodes.
use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use rustc::hir;
use rustc::hir::def::Def;
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::codemap::{Span, BytePos};
use syntax::ext::hygiene::SyntaxContext;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor, FnKind};

use bindings::IntoSymbol;
use driver;
use util::HirDefExt;
use visit::Visit;


/// Find all nodes that refer to marked nodes.
struct MarkUseVisitor<'a, 'hir: 'a, 'gcx: 'a + 'tcx, 'tcx: 'a> {
    cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
    uses: HashSet<NodeId>,
    label: Symbol,
}

impl<'a, 'hir, 'gcx, 'tcx> MarkUseVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn handle_qpath(&mut self, use_id: NodeId, hp: &hir::QPath) {
        match hp {
            &hir::QPath::Resolved(_, ref path) => {
                println!("found qpath::resolved {:?}", path);
                if let Some(def_id) = path.def.opt_def_id() {
                    println!("  def_id {:?}", def_id);
                    if let Some(id) = self.cx.hir_map().as_local_node_id(def_id) {
                        println!("    node id {:?}", id);
                        if self.cx.marked(id, self.label) {
                            println!("      insert!!");
                            self.uses.insert(use_id);
                        }
                    }
                }
            },
            &hir::QPath::TypeRelative(..) => {},
        }
    }
}

impl<'a, 'hir, 'gcx, 'tcx, 's> Visitor<'s> for MarkUseVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn visit_expr(&mut self, x: &'s Expr) {
        match x.node {
            ExprKind::Path(_, _) => {
                match self.cx.hir_map().expect_expr(x.id).node {
                    hir::ExprPath(ref hp) => self.handle_qpath(x.id, hp),
                    ref n => panic!("unexpected hir node for ExprKind::Path: {:?}", n),
                }
            },
            ExprKind::Struct(_, _, _) => {
                match self.cx.hir_map().expect_expr(x.id).node {
                    hir::ExprStruct(ref hp, _, _) => self.handle_qpath(x.id, hp),
                    ref n => panic!("unexpected hir node for ExprKind::Struct: {:?}", n),
                }
            },
            _ => {},
        }

        visit::walk_expr(self, x);
    }
}

pub fn find_mark_uses<T: Visit>(target: &T, cx: &driver::Ctxt, label: &str) -> HashSet<NodeId> {
    let mut v = MarkUseVisitor {
        cx: cx,
        uses: HashSet::new(),
        label: label.into_symbol(),
    };
    target.visit(&mut v);
    v.uses
}

pub fn mark_uses_command(krate: &Crate,
                         cx: &driver::Ctxt,
                         label: &str) -> HashSet<(NodeId, Symbol)> {
    let uses = find_mark_uses(krate, cx, label);
    let mut new_marks = cx.marks().clone().into_iter()
                          .filter(|&(_, v)| v.as_str() != label).collect::<HashSet<_>>();
    for id in uses {
        new_marks.insert((id, label.into_symbol()));
    }
    new_marks
}
