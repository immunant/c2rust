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

use command::CommandState;
use driver;
use util::HirDefExt;
use util::IntoSymbol;
use visit::Visit;


/// Find all nodes that refer to marked nodes.
struct MarkUseVisitor<'a, 'hir: 'a, 'gcx: 'a + 'tcx, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
    label: Symbol,
}

impl<'a, 'hir, 'gcx, 'tcx> MarkUseVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn handle_qpath(&mut self, use_id: NodeId, hp: &hir::QPath) {
        match hp {
            &hir::QPath::Resolved(_, ref path) => {
                if let Some(def_id) = path.def.opt_def_id() {
                    if let Some(id) = self.cx.hir_map().as_local_node_id(def_id) {
                        if self.st.marked(id, self.label) {
                            self.st.add_mark(use_id, self.label);
                        }

                        // For struct and node constructors, also check the parent item
                        if matches!([path.def] Def::StructCtor(..)) ||
                           matches!([path.def] Def::VariantCtor(..)) {
                            let parent_id = self.cx.hir_map().get_parent(id);
                            if self.st.marked(parent_id, self.label) {
                                self.st.add_mark(use_id, self.label);
                            }
                        }
                    }
                }
            },
            &hir::QPath::TypeRelative(..) => {},
        }
    }
}

impl<'a, 'hir, 'gcx, 'tcx, 's> Visitor<'s> for MarkUseVisitor<'a, 'hir, 'gcx, 'tcx> {
    // We currently handle exprs, pats, and tys.  There are more cases (see comment in
    // path_edit.rs), but this should be sufficient for now.
    fn visit_expr(&mut self, x: &'s Expr) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::map::NodeExpr(y) => y)
        } else {
            visit::walk_expr(self, x);
            return;
        };

        match x.node {
            ExprKind::Path(_, _) => {
                expect!([hir.node] hir::ExprPath(ref hp) => {
                    info!("looking at ExprPath {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            ExprKind::Struct(_, _, _) => {
                expect!([hir.node] hir::ExprStruct(ref hp, _, _) => {
                    info!("looking at ExprStruct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            _ => {},
        }

        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'s Pat) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::map::NodePat(y) => y,
                           hir::map::NodeLocal(y) => y)
        } else {
            visit::walk_pat(self, x);
            return;
        };

        match x.node {
            PatKind::Struct(_, _, _) => {
                expect!([hir.node] hir::PatKind::Struct(ref hp, _, _) => {
                    info!("looking at PatStruct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            PatKind::TupleStruct(_, _, _) => {
                expect!([hir.node] hir::PatKind::TupleStruct(ref hp, _, _) => {
                    info!("looking at PatTupleStruct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            PatKind::Path(_, _) => {
                expect!([hir.node] hir::PatKind::Path(ref hp) => {
                    info!("looking at PatPath {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            _ => {},
        }

        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'s Ty) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::map::NodeTy(y) => y)
        } else {
            visit::walk_ty(self, x);
            return;
        };

        match x.node {
            TyKind::Path(_, _) => {
                expect!([hir.node] hir::TyPath(ref hp) => {
                    info!("looking at TyPath {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            },

            _ => {},
        }

        visit::walk_ty(self, x);
    }
}

pub fn find_mark_uses<T: Visit>(target: &T,
                                st: &CommandState,
                                cx: &driver::Ctxt,
                                label: &str) {
    let old_ids = st.marks().iter().filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id).collect::<Vec<_>>();

    let mut v = MarkUseVisitor {
        st: st,
        cx: cx,
        label: label.into_symbol(),
    };
    target.visit(&mut v);

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

pub fn find_mark_uses_command(st: &CommandState, cx: &driver::Ctxt, label: &str) {
    find_mark_uses(&*st.krate(), st, cx, label);
}
