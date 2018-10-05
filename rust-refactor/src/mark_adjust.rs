//! This module implements commands for manipulating the current set of marked nodes.
use std::str::FromStr;
use rustc::hir;
use rustc::hir::def::Def;
use rustc::ty::TypeVariants;
use syntax::ast::*;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor};

use api::DriverCtxtExt;
use ast_manip::{Visit, visit_nodes};
use command::CommandState;
use command::{Registry, DriverCommand};
use driver::{self, Phase};
use util::HirDefExt;
use util::IntoSymbol;


/// Find all nodes that refer to marked nodes.
struct MarkUseVisitor<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a driver::Ctxt<'a, 'tcx>,
    label: Symbol,
}

impl<'a, 'tcx> MarkUseVisitor<'a, 'tcx> {
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

impl<'a, 'tcx, 's> Visitor<'s> for MarkUseVisitor<'a, 'tcx> {
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
                           hir::map::NodeBinding(y) => y)
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


pub fn find_field_uses<T: Visit>(target: &T,
                                 st: &CommandState,
                                 cx: &driver::Ctxt,
                                 field: &str,
                                 label: &str) {
    let field = field.into_symbol();
    let label = label.into_symbol();

    let old_ids = st.marks().iter().filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id).collect::<Vec<_>>();

    // Fields can only appear in exprs, so we don't need a whole Visitor impl.
    visit_nodes(target, |e: &Expr| {
        match e.node {
            ExprKind::Field(ref obj, ref ident) => {
                if ident.name != field {
                    return;
                }

                // Use the adjusted type to catch field accesses through autoderef.
                let ty = cx.adjusted_node_type(obj.id);
                let def = match_or!([ty.sty] TypeVariants::TyAdt(def, _) => def; return);
                if let Some(id) = cx.hir_map().as_local_node_id(def.did) {
                    if st.marked(id, label) {
                        st.add_mark(e.id, label);
                    }
                }
            },

            // TODO: Also handle uses in ExprKind::Struct.  (This case is more complicated since we
            // need to resolve the `Struct` node's `Path`.  Also, the `Field` node type
            // (representing field uses) does not have a NodeId of its own, so it's unclear where
            // we should put the resulting mark.)

            _ => {},
        }
    });

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

pub fn find_field_uses_command(st: &CommandState, cx: &driver::Ctxt, field: &str, label: &str) {
    find_field_uses(&*st.krate(), st, cx, field, label);
}


pub fn find_arg_uses<T: Visit>(target: &T,
                               st: &CommandState,
                               cx: &driver::Ctxt,
                               arg_idx: usize,
                               label: &str) {
    let label = label.into_symbol();

    let old_ids = st.marks().iter().filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id).collect::<Vec<_>>();

    visit_nodes(target, |e: &Expr| {
        if let Some(def_id) = cx.opt_callee(e) {
            if let Some(node_id) = cx.hir_map().as_local_node_id(def_id) {
                if st.marked(node_id, label) {
                    let args = match e.node {
                        ExprKind::Call(_, ref args) => args,
                        ExprKind::MethodCall(_, ref args) => args,
                        _ => panic!("expected Call or MethodCall"),
                    };
                    st.add_mark(args[arg_idx].id, label);
                }
            }
        }
    });

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

pub fn find_arg_uses_command(st: &CommandState, cx: &driver::Ctxt, arg_idx: usize, label: &str) {
    find_arg_uses(&*st.krate(), st, cx, arg_idx, label);
}


pub fn find_callers<T: Visit>(target: &T,
                              st: &CommandState,
                              cx: &driver::Ctxt,
                              label: &str) {
    let label = label.into_symbol();

    let old_ids = st.marks().iter().filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id).collect::<Vec<_>>();

    visit_nodes(target, |e: &Expr| {
        if let Some(def_id) = cx.opt_callee(e) {
            if let Some(node_id) = cx.hir_map().as_local_node_id(def_id) {
                if st.marked(node_id, label) {
                    st.add_mark(e.id, label);
                }
            }
        }
    });

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

pub fn find_callers_command(st: &CommandState, cx: &driver::Ctxt, label: &str) {
    find_callers(&*st.krate(), st, cx, label);
}


pub fn rename_marks(st: &CommandState, old: Symbol, new: Symbol) {
    let mut marks = st.marks_mut();
    let nodes = marks.iter().filter(|&&(_, label)| label == old)
        .map(|&(id, _)| id).collect::<Vec<_>>();
    for id in nodes {
        marks.remove(&(id, old));
        marks.insert((id, new));
    }
}


pub fn mark_pub_in_mod(st: &CommandState, label: &str) {
    let label = label.into_symbol();

    // Use a preorder traversal.  This results in recursively marking public descendants of any
    // marked module or crate.
    if st.marked(CRATE_NODE_ID, label) {
        for i in &st.krate().module.items {
            if i.vis.node == VisibilityKind::Public {
                st.add_mark(i.id, label);
            }
        }
    }

    visit_nodes(&*st.krate(), |i: &Item| {
        if st.marked(i.id, label) {
            if let ItemKind::Mod(ref m) = i.node {
                for i in &m.items {
                    if i.vis.node == VisibilityKind::Public {
                        st.add_mark(i.id, label);
                    }
                }
            }
        }
    });
}


pub fn register_commands(reg: &mut Registry) {
    reg.register("print_marks", |_| {
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            let mut marks = st.marks().iter().map(|&x| x).collect::<Vec<_>>();
            marks.sort();

            for (id, label) in marks {
                info!("{}:{}", id.as_usize(), label.as_str());
            }
        }))
    });

    reg.register("mark_uses", |args| {
        let arg = args[0].clone();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, cx| {
            find_mark_uses_command(st, cx, &arg);
        }))
    });

    reg.register("mark_field_uses", |args| {
        let field = args[0].clone();
        let label = args[1].clone();
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            find_field_uses_command(st, cx, &field, &label);
        }))
    });

    reg.register("mark_arg_uses", |args| {
        let arg_idx = usize::from_str(&args[0]).unwrap();
        let label = args[1].clone();
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            find_arg_uses_command(st, cx, arg_idx, &label);
        }))
    });

    reg.register("mark_callers", |args| {
        let label = args[0].clone();
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            find_callers_command(st, cx, &label);
        }))
    });

    reg.register("rename_marks", |args| {
        let old = (&args[0]).into_symbol();
        let new = (&args[1]).into_symbol();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            rename_marks(st, old, new);
        }))
    });

    reg.register("mark_pub_in_mod", |args| {
        let label = args[0].clone();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            mark_pub_in_mod(st, &label);
        }))
    });
}
