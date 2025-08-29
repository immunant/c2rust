use rustc_ast::visit::{self, AssocCtxt, Visitor};
use rustc_ast::*;
use rustc_data_structures::fx::FxHashMap;
use rustc_span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpanNodeKind {
    Crate,
    Block,
    Pat,
    PatField,
    Stmt,
    Local,
    Arm,
    ExprField,
    Expr,
    AssocConstraint,
    Ty,
    Param,
    Variant,
    FieldDef,
    Item,
    ForeignItem,
    AssocItem,
    // We need a separate kind for ExprKind::Path because
    // the rustc macro expander emits `*foo` expressions where
    // both the outer and inner expressions have the same span
    // We disambiguate by assigning (span, Expr) to the outer one,
    // and (span, PathExpr) to the inner expression.
    PathExpr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeSpan {
    pub span: Span,
    pub kind: SpanNodeKind,
}

impl NodeSpan {
    pub fn new(span: Span, kind: SpanNodeKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Default, Clone)]
pub struct AstSpanMaps {
    pub node_id_to_span_map: FxHashMap<NodeId, NodeSpan>,
    pub span_to_node_id_map: FxHashMap<NodeSpan, NodeId>,
}

impl AstSpanMaps {
    pub fn new(krate: &Crate) -> Self {
        let mut mapper = AstSpanMapper::default();
        mapper.visit_crate(krate);
        mapper.0
    }
}

#[derive(Default, Clone)]
struct AstSpanMapper(AstSpanMaps);

impl AstSpanMapper {
    fn insert_mapping(&mut self, id: NodeId, span: Span, kind: SpanNodeKind) {
        if id == DUMMY_NODE_ID || span.is_dummy() {
            return;
        }

        let ns = NodeSpan { span, kind };
        let old_ns = self.0.node_id_to_span_map.insert(id, ns);
        let _old_id = self.0.span_to_node_id_map.insert(ns, id);

        assert!(
            old_ns.is_none(),
            "id {id:?} already has span {old_ns:?} != {ns:?}"
        );
        // Some spans can show up in multiple nodes
        //assert!(old_id.is_none(), "span {ns:?} already has id {old_id:?} != {id:?}");
    }
}

impl Visitor<'_> for AstSpanMapper {
    fn visit_crate(&mut self, krate: &Crate) {
        self.insert_mapping(krate.id, krate.spans.inner_span, SpanNodeKind::Crate);
        visit::walk_crate(self, krate);
    }

    fn visit_block(&mut self, block: &Block) {
        self.insert_mapping(block.id, block.span, SpanNodeKind::Block);
        visit::walk_block(self, block);
    }

    fn visit_pat(&mut self, pat: &Pat) {
        self.insert_mapping(pat.id, pat.span, SpanNodeKind::Pat);
        visit::walk_pat(self, pat);
    }

    fn visit_pat_field(&mut self, pf: &PatField) {
        self.insert_mapping(pf.id, pf.span, SpanNodeKind::PatField);
        visit::walk_pat_field(self, pf);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        self.insert_mapping(stmt.id, stmt.span, SpanNodeKind::Stmt);
        visit::walk_stmt(self, stmt);
    }

    fn visit_local(&mut self, local: &Local) {
        self.insert_mapping(local.id, local.span, SpanNodeKind::Local);
        visit::walk_local(self, local);
    }

    fn visit_arm(&mut self, arm: &Arm) {
        self.insert_mapping(arm.id, arm.span, SpanNodeKind::Arm);
        visit::walk_arm(self, arm);
    }

    fn visit_expr_field(&mut self, ef: &ExprField) {
        self.insert_mapping(ef.id, ef.span, SpanNodeKind::ExprField);
        visit::walk_expr_field(self, ef);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if matches!(expr.kind, ExprKind::Path(..)) {
            self.insert_mapping(expr.id, expr.span, SpanNodeKind::PathExpr);
        } else {
            self.insert_mapping(expr.id, expr.span, SpanNodeKind::Expr);
        }
        visit::walk_expr(self, expr);
    }

    fn visit_assoc_constraint(&mut self, ac: &AssocConstraint) {
        self.insert_mapping(ac.id, ac.span, SpanNodeKind::AssocConstraint);
        visit::walk_assoc_constraint(self, ac);
    }

    fn visit_ty(&mut self, ty: &Ty) {
        self.insert_mapping(ty.id, ty.span, SpanNodeKind::Ty);
        visit::walk_ty(self, ty);
    }

    fn visit_param(&mut self, param: &Param) {
        self.insert_mapping(param.id, param.span, SpanNodeKind::Param);
        visit::walk_param(self, param);
    }

    fn visit_variant(&mut self, var: &Variant) {
        self.insert_mapping(var.id, var.span, SpanNodeKind::Variant);
        visit::walk_variant(self, var);
    }

    fn visit_field_def(&mut self, fd: &FieldDef) {
        self.insert_mapping(fd.id, fd.span, SpanNodeKind::FieldDef);
        visit::walk_field_def(self, fd);
    }

    fn visit_item(&mut self, i: &Item) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::Item);
        visit::walk_item(self, i);
    }

    fn visit_foreign_item(&mut self, i: &ForeignItem) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::ForeignItem);
        visit::walk_foreign_item(self, i);
    }

    fn visit_assoc_item(&mut self, i: &AssocItem, ctxt: AssocCtxt) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::AssocItem);
        visit::walk_assoc_item(self, i, ctxt);
    }
}
