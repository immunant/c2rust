//! Visitors for implementing `ChildMatch`, `DescMatch`, and `Filter`, which need to walk the AST
//! and inspect the currently selected nodes.

use std::collections::HashSet;
use syntax::ast::*;
use syntax::source_map::Span;
use syntax::visit::{self, FnKind, Visitor};

use crate::command::CommandState;
use crate::select::filter::{self, AnyNode};
use crate::select::Filter;
use crate::RefactorCtxt;

struct ChildMatchVisitor<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    old: HashSet<NodeId>,
    new: HashSet<NodeId>,
    /// Are we at a child of a node that was selected in the `old` set?
    in_old: bool,
    filt: &'a Filter,
}

impl<'ast, 'a, 'tcx> ChildMatchVisitor<'a, 'tcx> {
    fn matches(&self, node: AnyNode) -> bool {
        filter::matches_filter(self.st, self.cx, node, self.filt)
    }

    fn maybe_enter_old<F: FnOnce(&mut Self)>(&mut self, id: NodeId, func: F) {
        let was_in_old = self.in_old;
        self.in_old = self.old.contains(&id);
        func(self);
        self.in_old = was_in_old;
    }

    fn walk_args(&mut self, x: &'ast [Arg]) {
        for arg in x {
            if self.in_old && self.matches(AnyNode::Arg(arg)) {
                self.new.insert(arg.id);
            }

            // No point in visiting if the arg is not in `old` - just let `walk` handle it.
            if self.old.contains(&arg.id) {
                self.maybe_enter_old(arg.id, |v| {
                    v.visit_pat(&arg.pat);
                    v.visit_ty(&arg.ty);
                });
            }
        }
    }
}

impl<'ast, 'a, 'tcx> Visitor<'ast> for ChildMatchVisitor<'a, 'tcx> {
    fn visit_item(&mut self, x: &'ast Item) {
        if self.in_old && self.matches(AnyNode::Item(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let ItemKind::Fn(ref decl, ..) = x.node {
                v.walk_args(&decl.inputs);
            }
            visit::walk_item(v, x)
        });
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        if self.in_old && self.matches(AnyNode::TraitItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let TraitItemKind::Method(ref sig, ..) = x.node {
                v.walk_args(&sig.decl.inputs);
            }
            visit::walk_trait_item(v, x)
        });
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        if self.in_old && self.matches(AnyNode::ImplItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let ImplItemKind::Method(ref sig, ..) = x.node {
                v.walk_args(&sig.decl.inputs);
            }
            visit::walk_impl_item(v, x)
        });
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        if self.in_old && self.matches(AnyNode::ForeignItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            // walk_foreign_item doesn't call visit_fn
            if let ForeignItemKind::Fn(ref decl, _) = x.node {
                v.walk_args(&decl.inputs);
            }
            visit::walk_foreign_item(v, x)
        });
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        if self.in_old && self.matches(AnyNode::Stmt(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_stmt(v, x));
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        if self.in_old && self.matches(AnyNode::Expr(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_expr(v, x));
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        if self.in_old && self.matches(AnyNode::Pat(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_pat(v, x));
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        if self.in_old && self.matches(AnyNode::Ty(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_ty(v, x));
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        if self.in_old && self.matches(AnyNode::Field(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_struct_field(v, x));
    }
}

pub fn matching_children(
    st: &CommandState,
    cx: &RefactorCtxt,
    krate: &Crate,
    sel: HashSet<NodeId>,
    filt: &Filter,
) -> HashSet<NodeId> {
    let in_old = sel.contains(&CRATE_NODE_ID);
    let mut v = ChildMatchVisitor {
        st: st,
        cx: cx,
        old: sel,
        new: HashSet::new(),
        in_old: in_old,
        filt: filt,
    };
    visit::walk_crate(&mut v, krate);
    v.new
}

struct DescMatchVisitor<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    old: HashSet<NodeId>,
    new: HashSet<NodeId>,
    /// Are we at a descendant of a node that was selected in the `old` set?
    in_old: bool,
    filt: &'a Filter,
}

impl<'ast, 'a, 'tcx> DescMatchVisitor<'a, 'tcx> {
    fn matches(&self, node: AnyNode) -> bool {
        filter::matches_filter(self.st, self.cx, node, self.filt)
    }

    fn maybe_enter_old<F: FnOnce(&mut Self)>(&mut self, id: NodeId, func: F) {
        let enter = self.old.contains(&id);
        if enter {
            let was_in_old = self.in_old;
            self.in_old = true;
            func(self);
            self.in_old = was_in_old;
        } else {
            func(self);
        }
    }

    fn walk_args(&mut self, x: &'ast [Arg]) {
        for arg in x {
            if self.in_old && self.matches(AnyNode::Arg(arg)) {
                self.new.insert(arg.id);
            }

            // No point in visiting if the arg is not in `old` - just let `walk` handle it.
            if self.old.contains(&arg.id) {
                self.maybe_enter_old(arg.id, |v| {
                    v.visit_pat(&arg.pat);
                    v.visit_ty(&arg.ty);
                });
            }
        }
    }
}

impl<'ast, 'a, 'tcx> Visitor<'ast> for DescMatchVisitor<'a, 'tcx> {
    fn visit_item(&mut self, x: &'ast Item) {
        if self.in_old && self.matches(AnyNode::Item(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let ItemKind::Fn(ref decl, ..) = x.node {
                v.walk_args(&decl.inputs);
            }
            visit::walk_item(v, x)
        });
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        if self.in_old && self.matches(AnyNode::TraitItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let TraitItemKind::Method(ref sig, ..) = x.node {
                v.walk_args(&sig.decl.inputs);
            }
            visit::walk_trait_item(v, x)
        });
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        if self.in_old && self.matches(AnyNode::ImplItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let ImplItemKind::Method(ref sig, ..) = x.node {
                v.walk_args(&sig.decl.inputs);
            }
            visit::walk_impl_item(v, x)
        });
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        if self.in_old && self.matches(AnyNode::ForeignItem(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| {
            if let ForeignItemKind::Fn(ref decl, ..) = x.node {
                v.walk_args(&decl.inputs);
            }
            visit::walk_foreign_item(v, x)
        });
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        if self.in_old && self.matches(AnyNode::Stmt(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_stmt(v, x));
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        if self.in_old && self.matches(AnyNode::Expr(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_expr(v, x));
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        if self.in_old && self.matches(AnyNode::Pat(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_pat(v, x));
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        if self.in_old && self.matches(AnyNode::Ty(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_ty(v, x));
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        if self.in_old && self.matches(AnyNode::Field(x)) {
            self.new.insert(x.id);
        }
        self.maybe_enter_old(x.id, |v| visit::walk_struct_field(v, x));
    }
}

pub fn matching_descendants(
    st: &CommandState,
    cx: &RefactorCtxt,
    krate: &Crate,
    sel: HashSet<NodeId>,
    filt: &Filter,
) -> HashSet<NodeId> {
    let in_old = sel.contains(&CRATE_NODE_ID);
    let mut v = DescMatchVisitor {
        st: st,
        cx: cx,
        old: sel,
        new: HashSet::new(),
        in_old: in_old,
        filt: filt,
    };
    visit::walk_crate(&mut v, krate);
    v.new
}

struct FilterVisitor<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    old: HashSet<NodeId>,
    new: HashSet<NodeId>,
    filt: &'a Filter,
}

impl<'ast, 'a, 'tcx> FilterVisitor<'a, 'tcx> {
    fn matches(&self, node: AnyNode) -> bool {
        filter::matches_filter(self.st, self.cx, node, self.filt)
    }

    fn walk_args(&mut self, x: &'ast [Arg]) {
        for arg in x {
            if self.old.contains(&arg.id) && self.matches(AnyNode::Arg(arg)) {
                self.new.insert(arg.id);
            }
        }
    }
}

impl<'ast, 'a, 'tcx> Visitor<'ast> for FilterVisitor<'a, 'tcx> {
    fn visit_item(&mut self, x: &'ast Item) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Item(x)) {
            self.new.insert(x.id);
        }
        if let ItemKind::Fn(ref decl, ..) = x.node {
            self.walk_args(&decl.inputs);
        }
        visit::walk_item(self, x);
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        if self.old.contains(&x.id) && self.matches(AnyNode::TraitItem(x)) {
            self.new.insert(x.id);
        }
        if let TraitItemKind::Method(ref sig, ..) = x.node {
            self.walk_args(&sig.decl.inputs);
        }
        visit::walk_trait_item(self, x);
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        if self.old.contains(&x.id) && self.matches(AnyNode::ImplItem(x)) {
            self.new.insert(x.id);
        }
        if let ImplItemKind::Method(ref sig, ..) = x.node {
            self.walk_args(&sig.decl.inputs);
        }
        visit::walk_impl_item(self, x);
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        if self.old.contains(&x.id) && self.matches(AnyNode::ForeignItem(x)) {
            self.new.insert(x.id);
        }
        if let ForeignItemKind::Fn(ref decl, ..) = x.node {
            self.walk_args(&decl.inputs);
        }
        visit::walk_foreign_item(self, x);
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Stmt(x)) {
            self.new.insert(x.id);
        }
        visit::walk_stmt(self, x);
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Expr(x)) {
            self.new.insert(x.id);
        }
        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Pat(x)) {
            self.new.insert(x.id);
        }
        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Ty(x)) {
            self.new.insert(x.id);
        }
        visit::walk_ty(self, x);
    }

    fn visit_fn(&mut self, kind: FnKind<'ast>, fd: &'ast FnDecl, span: Span, _id: NodeId) {
        for arg in &fd.inputs {
            if self.old.contains(&arg.id) && self.matches(AnyNode::Arg(arg)) {
                self.new.insert(arg.id);
            }
        }

        visit::walk_fn(self, kind, fd, span);
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        if self.old.contains(&x.id) && self.matches(AnyNode::Field(x)) {
            self.new.insert(x.id);
        }
        visit::walk_struct_field(self, x);
    }
}

pub fn filter(
    st: &CommandState,
    cx: &RefactorCtxt,
    krate: &Crate,
    sel: HashSet<NodeId>,
    filt: &Filter,
) -> HashSet<NodeId> {
    let mut v = FilterVisitor {
        st: st,
        cx: cx,
        old: sel,
        new: HashSet::new(),
        filt: filt,
    };
    visit::walk_crate(&mut v, krate);
    v.new
}
