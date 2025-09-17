//! This module implements commands for manipulating the current set of marked nodes.
use log::info;
use rustc_ast::ast;
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use rustc_hir as hir;
use rustc_hir::def::{DefKind, Res};
use rustc_span::symbol::Symbol;
use rustc_type_ir::sty::TyKind;
use std::str::FromStr;

use crate::ast_builder::IntoSymbol;
use crate::ast_manip::{visit_nodes, Visit};
use crate::command::CommandState;
use crate::command::{DriverCommand, FuncCommand, RefactorState, Registry};
use crate::driver::Phase;
use crate::RefactorCtxt;
use crate::{expect, match_or};

/// Find all nodes that refer to marked nodes.
struct MarkUseVisitor<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    label: Symbol,
}

impl<'a, 'tcx> MarkUseVisitor<'a, 'tcx> {
    fn handle_qpath(&mut self, use_id: NodeId, hp: &hir::QPath) {
        match hp {
            &hir::QPath::Resolved(_, ref path) => {
                if let Some(def_id) = path.res.opt_def_id() {
                    if let Some(id) = self.cx.hir_map().as_local_node_id(def_id) {
                        if self.st.marked(id, self.label) {
                            self.st.add_mark(use_id, self.label);
                        }

                        // For struct and node constructors, also check the parent item
                        if crate::matches!([path.res] Res::Def(DefKind::Ctor(..), _)) {
                            let hir_id = self.cx.hir_map().node_to_hir_id(id);
                            let parent_id = self.cx.hir_map().get_parent_item(hir_id);
                            let parent_id = self.cx.hir_map().local_def_id_to_node_id(parent_id);
                            if self.st.marked(parent_id, self.label) {
                                self.st.add_mark(use_id, self.label);
                            }
                        }
                    }
                }
            }
            &hir::QPath::TypeRelative(..) => {}
            &hir::QPath::LangItem(..) => unimplemented!(),
        }
    }
}

impl<'a, 'tcx, 's> Visitor<'s> for MarkUseVisitor<'a, 'tcx> {
    // We currently handle exprs, pats, and tys.  There are more cases (see comment in
    // path_edit.rs), but this should be sufficient for now.
    fn visit_expr(&mut self, x: &'s Expr) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::Node::Expr(y) => y)
        } else {
            visit::walk_expr(self, x);
            return;
        };

        match x.kind {
            ExprKind::Path(_, _) => {
                expect!([hir.kind] hir::ExprKind::Path(ref hp) => {
                    info!("looking at ExprKind::Path {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            ExprKind::Struct(_) => {
                expect!([hir.kind] hir::ExprKind::Struct(ref hp, _, _) => {
                    info!("looking at ExprKind::Struct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            _ => {}
        }

        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'s Pat) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::Node::Pat(y) => y)
        } else {
            visit::walk_pat(self, x);
            return;
        };

        match x.kind {
            PatKind::Struct(_, _, _, _) => {
                expect!([hir.kind] hir::PatKind::Struct(ref hp, _, _) => {
                    info!("looking at PatStruct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            PatKind::TupleStruct(_, _, _) => {
                expect!([hir.kind] hir::PatKind::TupleStruct(ref hp, _, _) => {
                    info!("looking at PatTupleStruct {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            PatKind::Path(_, _) => {
                expect!([hir.kind] hir::PatKind::Path(ref hp) => {
                    info!("looking at PatPath {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            _ => {}
        }

        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'s Ty) {
        let hir = if let Some(node) = self.cx.hir_map().find(x.id) {
            expect!([node] hir::Node::Ty(y) => y)
        } else {
            visit::walk_ty(self, x);
            return;
        };

        match x.kind {
            ast::TyKind::Path(_, _) => {
                expect!([hir.kind] hir::TyKind::Path(ref hp) => {
                    info!("looking at TyPath {:?}", x);
                    self.handle_qpath(x.id, hp);
                });
            }

            _ => {}
        }

        visit::walk_ty(self, x);
    }
}

pub fn find_mark_uses<T: Visit>(target: &T, st: &CommandState, cx: &RefactorCtxt, label: &str) {
    let old_ids = st
        .marks()
        .iter()
        .filter(|&&(_, l)| l.as_str() == label)
        .map(|&(id, _)| id)
        .collect::<Vec<_>>();

    let mut v = MarkUseVisitor {
        st,
        cx,
        label: label.into_symbol(),
    };
    target.visit(&mut v);

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

/// # `mark_uses` Command
///
/// Usage: `mark_uses MARK`
///
/// Marks: reads `MARK`; sets/clears `MARK`
///
/// For every top-level definition bearing `MARK`, apply `MARK` to uses of that
/// definition.  Removes `MARK` from the original definitions.
pub fn find_mark_uses_command(st: &CommandState, cx: &RefactorCtxt, label: &str) {
    find_mark_uses(&*st.krate_mut(), st, cx, label);
}

pub fn find_field_uses<T: Visit>(
    target: &T,
    st: &CommandState,
    cx: &RefactorCtxt,
    field: &str,
    label: &str,
) {
    let field = field.into_symbol();
    let label = label.into_symbol();

    let old_ids = st
        .marks()
        .iter()
        .filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id)
        .collect::<Vec<_>>();

    // Fields can only appear in exprs, so we don't need a whole Visitor impl.
    visit_nodes(target, |e: &Expr| {
        match e.kind {
            ExprKind::Field(ref obj, ref ident) => {
                if ident.name != field {
                    return;
                }

                // Use the adjusted type to catch field accesses through autoderef.
                let ty = cx.adjusted_node_type(obj.id);
                let def = match_or!([ty.kind()] TyKind::Adt(def, _) => def; return);
                if let Some(id) = cx.hir_map().as_local_node_id(def.did()) {
                    if st.marked(id, label) {
                        st.add_mark(e.id, label);
                    }
                }
            }

            // TODO: Also handle uses in ExprKind::Struct.  (This case is more complicated since we
            // need to resolve the `Struct` node's `Path`.  Also, the `Field` node type
            // (representing field uses) does not have a NodeId of its own, so it's unclear where
            // we should put the resulting mark.)
            _ => {}
        }
    });

    for id in old_ids {
        st.remove_mark(id, label);
    }
}

/// # `mark_field_uses` Command
///
/// Obsolete - use `select` with `match_expr!(typed!(::TheStruct).field)` instead
///
/// Usage: `mark_field_uses FIELD MARK`
///
/// Marks: reads `MARK`; sets/clears `MARK`
///
/// For every struct definition bearing `MARK`, apply `MARK` to expressions
/// that use `FIELD` of that struct.  Removes `MARK` from the original struct.
pub fn find_field_uses_command(st: &CommandState, cx: &RefactorCtxt, field: &str, label: &str) {
    find_field_uses(&*st.krate(), st, cx, field, label);
}

pub fn find_arg_uses<T: Visit>(
    target: &T,
    st: &CommandState,
    cx: &RefactorCtxt,
    arg_idx: usize,
    label: &str,
) {
    let label = label.into_symbol();

    let old_ids = st
        .marks()
        .iter()
        .filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id)
        .collect::<Vec<_>>();

    visit_nodes(target, |e: &Expr| {
        if let Some(def_id) = cx.opt_callee(e) {
            if let Some(node_id) = cx.hir_map().as_local_node_id(def_id) {
                if st.marked(node_id, label) {
                    let args = match e.kind {
                        ExprKind::Call(_, ref args) => args,
                        ExprKind::MethodCall(_, ref args, _) => args,
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

/// # `mark_arg_uses` Command
///
/// Usage: `mark_arg_uses ARG_IDX MARK`
///
/// Marks: reads `MARK`; sets/clears `MARK`
///
/// For every `fn` definition bearing `MARK`, apply `MARK` to expressions
/// passed in as argument `ARG_IDX` in calls to that function.
/// Removes `MARK` from the original function.
pub fn find_arg_uses_command(st: &CommandState, cx: &RefactorCtxt, arg_idx: usize, label: &str) {
    find_arg_uses(&*st.krate(), st, cx, arg_idx, label);
}

pub fn find_callers<T: Visit>(target: &T, st: &CommandState, cx: &RefactorCtxt, label: &str) {
    let label = label.into_symbol();

    let old_ids = st
        .marks()
        .iter()
        .filter(|&&(_, l)| l == label)
        .map(|&(id, _)| id)
        .collect::<Vec<_>>();

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

/// # `mark_callers` Command
///
/// Usage: `mark_callers MARK`
///
/// Marks: reads `MARK`; sets/clears `MARK`
///
/// For every `fn` definition bearing `MARK`, apply `MARK` to call
/// expressions that call that function.
/// Removes `MARK` from the original function.
pub fn find_callers_command(st: &CommandState, cx: &RefactorCtxt, label: &str) {
    find_callers(&*st.krate(), st, cx, label);
}

/// # `copy_marks` Command
///
/// Usage: `copy_marks OLD_MARK NEW_MARK`
///
/// Marks: reads `OLD_MARK`; sets `NEW_MARK`
///
/// For every node bearing `OLD_MARK`, also apply `NEW_MARK`.
pub fn copy_marks(st: &CommandState, old: Symbol, new: Symbol) {
    let mut marks = st.marks_mut();
    let nodes = marks
        .iter()
        .filter(|&&(_, label)| label == old)
        .map(|&(id, _)| id)
        .collect::<Vec<_>>();
    for id in nodes {
        marks.insert((id, new));
    }
}

/// # `delete_marks` Command
///
/// Usage: `delete_marks MARK`
///
/// Marks: clears `MARK`
///
/// Remove `MARK` from every node where it appears.
pub fn delete_marks(st: &CommandState, old: Symbol) {
    let mut marks = st.marks_mut();
    marks.retain(|&(_, label)| label != old);
}

/// # `rename_marks` Command
///
/// Usage: `rename_marks OLD_MARK NEW_MARK`
///
/// Marks: reads/clears `OLD_MARK`; sets `NEW_MARK`
///
/// For every node bearing `OLD_MARK`, remove `OLD_MARK` and apply `NEW_MARK`.
pub fn rename_marks(st: &CommandState, old: Symbol, new: Symbol) {
    copy_marks(st, old, new);
    delete_marks(st, old);
}

/// # `mark_pub_in_mod` Command
///
/// Obsolete - use `select` instead.
///
/// Usage: `mark_pub_in_mod MARK`
///
/// Marks: reads `MARK`; sets `MARK`
///
/// In each `mod` bearing `MARK`, apply `MARK` to every public item in the module.
pub fn mark_pub_in_mod(st: &CommandState, label: &str) {
    let label = label.into_symbol();

    // Use a preorder traversal.  This results in recursively marking public descendants of any
    // marked module or crate.
    if st.marked(CRATE_NODE_ID, label) {
        for i in &st.krate().items {
            if let VisibilityKind::Public = i.vis.kind {
                st.add_mark(i.id, label);
            }
        }
    }

    visit_nodes(&*st.krate(), |i: &Item| {
        if st.marked(i.id, label) {
            if let ItemKind::Mod(_, ModKind::Loaded(ref m_items, _, _)) = i.kind {
                for i in &m_items[..] {
                    if let VisibilityKind::Public = i.vis.kind {
                        st.add_mark(i.id, label);
                    }
                }
            }
        }
    });
}

/// # `print_marks` Command
///
/// Test command - not intended for general use.
///
/// Usage: `print_marks`
///
/// Marks: reads all
///
/// Logs the ID and label of every mark, at level `info`.
fn print_marks(st: &CommandState) {
    let mut marks = st.marks().iter().copied().collect::<Vec<_>>();
    marks.sort();

    for (id, label) in marks {
        info!("{}:{}", id.as_usize(), label.as_str());
    }
}

/// # `clear_marks` Command
///
/// Usage: `clear_marks`
///
/// Marks: clears all marks
///
/// Remove all marks from all nodes.
fn register_clear_marks(reg: &mut Registry) {
    reg.register("clear_marks", |_args| {
        Box::new(FuncCommand(|rs: &mut RefactorState| {
            rs.clear_marks();
        }))
    });
}

pub fn register_commands(reg: &mut Registry) {
    reg.register("print_marks", |_| {
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            print_marks(st);
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

    reg.register("copy_marks", |args| {
        let old = (&args[0]).into_symbol();
        let new = (&args[1]).into_symbol();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            copy_marks(st, old, new);
        }))
    });

    reg.register("delete_marks", |args| {
        let old = (&args[0]).into_symbol();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, _cx| {
            delete_marks(st, old);
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

    register_clear_marks(reg);
}
