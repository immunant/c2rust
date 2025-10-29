use rustc_ast::visit::{self, AssocCtxt, Visitor};
use rustc_ast::*;
use rustc_data_structures::fx::FxHashMap;
use rustc_span::source_map::symbol::Symbol;
use rustc_span::Span;
use smallvec::SmallVec;

/// Shared enumeration of child positions within a parent node.
/// The AST span mapper records a path of these slot values while walking each expression, and
/// the HIR visitor in `context.rs` reconstructs the same path.  Later, lookups compare both the
/// span and the path so that nodes which reuse the same span (for example, the derive-generated
/// BitfieldStruct helpers that originally crashed with a NodeId -> HirId panic) still resolve to
/// distinct HirIds.  Keeping the numbering identical on both sides is therefore required.
pub mod child_slot {
    // Expression slots - Binary operators
    pub const BINARY_LHS: u16 = 0;
    pub const BINARY_RHS: u16 = 1;

    // Expression slots - Unary operators
    pub const UNARY_OPERAND: u16 = 0;

    // Expression slots - Function calls (base values for dynamic computation)
    pub const CALL_CALLEE: u16 = 0;
    pub const CALL_ARG_BASE: u16 = 1;
    // CallArg0 = 1, CallArg1 = 2, ... (computed dynamically)

    // Expression slots - Method calls
    pub const METHOD_RECEIVER: u16 = 0;
    pub const METHOD_ARG_BASE: u16 = 1;
    // MethodArg0 = 1, MethodArg1 = 2, ... (computed dynamically)

    // Expression slots - Tuples
    pub const TUPLE_ELEM_BASE: u16 = 0;
    // TupleElem0 = 0, TupleElem1 = 1, ... (computed dynamically)

    // Expression slots - Arrays
    pub const ARRAY_ELEM_BASE: u16 = 0;
    // ArrayElem0 = 0, ArrayElem1 = 1, ... (computed dynamically)

    // Expression slots - Struct construction
    pub const STRUCT_FIELD_BASE: u16 = 0;
    // StructField0 = 0, StructField1 = 1, ... (computed dynamically)

    // Expression slots - Control flow
    pub const IF_CONDITION: u16 = 0;
    pub const IF_THEN_BRANCH: u16 = 1;
    pub const IF_ELSE_BRANCH: u16 = 2;

    pub const MATCH_SCRUTINEE: u16 = 0;
    pub const MATCH_ARM_BASE: u16 = 1;
    // MatchArm0 = 1, MatchArm1 = 2, ... (computed dynamically)

    // Expression slots - Field access and indexing
    pub const FIELD_BASE: u16 = 0;
    pub const INDEX_BASE: u16 = 0;
    pub const INDEX_INDEX: u16 = 1;

    // Block slots (statements use stmt_index; tail expression gets explicit slot)
    pub const BLOCK_TAIL: u16 = 0x0100;

    // Pattern slots (base values for dynamic computation)
    pub const PAT_TUPLE_ELEM_BASE: u16 = 0;
    pub const PAT_STRUCT_FIELD_BASE: u16 = 0;
    pub const PAT_SLICE_ELEM_BASE: u16 = 0;
    pub const PAT_OR_ALTERNATIVE_BASE: u16 = 0;

    // Closure slots
    pub const CLOSURE_PARAM_BASE: u16 = 0;
    pub const CLOSURE_BODY: u16 = 0x0080; // Reserve 0-127 for params

    // Special markers
    pub const SYNTHETIC: u16 = 0xFFFE; // Node exists only on one side
    pub const SKIP: u16 = 0xFFFF; // Explicit skip for alignment

    /// Dynamic slot for variable-length collections (args, tuple elements, etc.)
    /// Base is the starting slot number, index is the position within collection
    pub fn dynamic(base: u16, index: usize) -> u16 {
        let max_index = (u16::MAX - base) as usize;
        assert!(
            index <= max_index,
            "child slot overflow: base {} + index {} exceeds u16::MAX",
            base,
            index
        );
        base + (index as u16)
    }

    /// Create slot for Nth function argument (0-indexed)
    pub fn call_arg(n: usize) -> u16 {
        dynamic(CALL_ARG_BASE, n)
    }

    /// Create slot for Nth method argument (0-indexed, after receiver)
    pub fn method_arg(n: usize) -> u16 {
        dynamic(METHOD_ARG_BASE, n)
    }

    /// Create slot for Nth tuple element (0-indexed)
    pub fn tuple_elem(n: usize) -> u16 {
        dynamic(TUPLE_ELEM_BASE, n)
    }

    /// Create slot for Nth array element (0-indexed)
    pub fn array_elem(n: usize) -> u16 {
        dynamic(ARRAY_ELEM_BASE, n)
    }

    /// Create slot for Nth struct field (0-indexed)
    pub fn struct_field(n: usize) -> u16 {
        dynamic(STRUCT_FIELD_BASE, n)
    }

    /// Create slot for Nth match arm (0-indexed, after scrutinee)
    pub fn match_arm(n: usize) -> u16 {
        dynamic(MATCH_ARM_BASE, n)
    }

    /// Create slot for Nth pattern tuple element (0-indexed)
    pub fn pat_tuple_elem(n: usize) -> u16 {
        dynamic(PAT_TUPLE_ELEM_BASE, n)
    }

    /// Create slot for Nth pattern struct field (0-indexed)
    pub fn pat_struct_field(n: usize) -> u16 {
        dynamic(PAT_STRUCT_FIELD_BASE, n)
    }

    /// Create slot for Nth pattern slice element (0-indexed)
    pub fn pat_slice_elem(n: usize) -> u16 {
        dynamic(PAT_SLICE_ELEM_BASE, n)
    }

    /// Create slot for Nth pattern Or alternative (0-indexed)
    pub fn pat_or_alternative(n: usize) -> u16 {
        dynamic(PAT_OR_ALTERNATIVE_BASE, n)
    }

    /// Create slot for Nth closure parameter (0-indexed)
    pub fn closure_param(n: usize) -> u16 {
        assert!(
            n < 128,
            "closure parameter index overflow: {} exceeds maximum 127",
            n
        );
        dynamic(CLOSURE_PARAM_BASE, n)
    }
}

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

/// Structural fingerprint used to disambiguate nodes that reuse the same `Span`.
/// Derive-generated helpers and macro expansions routinely stamp identical spans on
/// multiple siblings; without recording where a node sits (statement index, child-slot
/// path, owning item) we cannot reconstruct a stable NodeId -> HirId mapping.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeContextKey {
    /// Statement index within parent block (if applicable)
    /// This is computed via explicit loop counter in visit_block
    pub stmt_index: Option<u32>,

    /// Hierarchical child position path: [parent_slot, child_slot, grandchild_slot, ...]
    /// Example: [BinaryLhs=0, CallArg1=2] means "2nd arg of call in LHS of binary op"
    pub child_path: SmallVec<[u16; 6]>,

    /// Optional identifier symbol for additional disambiguation
    pub symbol: Option<Symbol>,

    /// Enclosing owner (function/item) NodeId used to disambiguate identical spans
    /// emitted into multiple helper bodies (e.g. derives).
    pub owner: Option<NodeId>,
}

impl NodeContextKey {
    pub fn new() -> Self {
        Self {
            stmt_index: None,
            child_path: SmallVec::new(),
            symbol: None,
            owner: None,
        }
    }

    /// Create child context with explicit slot appended to path
    pub fn with_child_slot(&self, slot: u16) -> Self {
        let mut path = self.child_path.clone();
        path.push(slot);
        Self {
            stmt_index: self.stmt_index,
            child_path: path,
            symbol: None, // Reset symbol for child (parent's symbol doesn't apply)
            owner: self.owner,
        }
    }

    /// Set statement index (for nodes within blocks)
    pub fn with_stmt_index(mut self, idx: u32) -> Self {
        self.stmt_index = Some(idx);
        self
    }

    /// Set identifier symbol (for named nodes like bindings or path expressions)
    pub fn with_symbol(mut self, sym: Option<Symbol>) -> Self {
        self.symbol = sym;
        self
    }

    /// Attach owner information
    pub fn with_owner(mut self, owner: Option<NodeId>) -> Self {
        self.owner = owner;
        self
    }
}

impl Default for NodeContextKey {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Default, Clone)]
pub struct AstSpanMaps {
    pub node_id_to_span_map: FxHashMap<NodeId, NodeSpan>,
    pub span_to_node_id_map: FxHashMap<NodeSpan, NodeId>,
    /// Extended mapping: (NodeSpan, NodeContextKey) -> NodeId
    /// This provides structural disambiguation for macro-generated nodes
    pub context_to_node_id_map: FxHashMap<(NodeSpan, NodeContextKey), NodeId>,
    /// Store context for each node for reverse lookup
    pub node_id_to_context_map: FxHashMap<NodeId, NodeContextKey>,
}

impl AstSpanMaps {
    pub fn new(krate: &Crate) -> Self {
        let mut mapper = AstSpanMapper::default();
        mapper.visit_crate(krate);
        mapper.maps
    }
}

/// Walks the AST to populate `AstSpanMaps`, maintaining extra stacks so we can reconstruct the
/// structural context for every node we see.
#[derive(Clone)]
pub struct StructuralContext<OwnerId> {
    block_stack: Vec<(OwnerId, u32)>,
    context_stack: Vec<NodeContextKey>,
    owner_stack: Vec<OwnerId>,
}

impl<OwnerId> Default for StructuralContext<OwnerId> {
    fn default() -> Self {
        Self {
            block_stack: Vec::new(),
            context_stack: Vec::new(),
            owner_stack: Vec::new(),
        }
    }
}

impl<OwnerId: Copy> StructuralContext<OwnerId> {
    pub fn current_stmt_index(&self) -> Option<u32> {
        self.block_stack.last().map(|(_, idx)| *idx)
    }

    pub fn push_block(&mut self, block_id: OwnerId) {
        self.block_stack.push((block_id, 0));
    }

    pub fn pop_block(&mut self) {
        self.block_stack.pop();
    }

    pub fn next_stmt(&mut self) {
        if let Some((_, ref mut idx)) = self.block_stack.last_mut() {
            *idx += 1;
        }
    }

    pub fn current_owner(&self) -> Option<OwnerId> {
        self.owner_stack.last().copied()
    }

    pub fn push_owner(&mut self, owner: OwnerId) {
        self.owner_stack.push(owner);
    }

    pub fn pop_owner(&mut self) {
        self.owner_stack.pop();
    }

    pub fn current_context(&self) -> NodeContextKey {
        self.context_stack.last().cloned().unwrap_or_default()
    }

    pub fn push_child(&mut self, slot: u16) {
        let parent_ctx = self.current_context();
        let child_ctx = parent_ctx.with_child_slot(slot);
        self.context_stack.push(child_ctx);
    }

    pub fn pop_child(&mut self) {
        self.context_stack.pop();
    }
}

#[derive(Default, Clone)]
struct AstSpanMapper {
    maps: AstSpanMaps,
    /// Tracks block/owner/child-slot stacks while walking the AST
    ctx: StructuralContext<NodeId>,
    /// Depth of attribute visitation (>0 means we're inside an attribute)
    attribute_depth: usize,
    /// Are we currently inside a FieldDef? Struct field patterns don't map to HIR
    in_field_def: bool,
}

impl AstSpanMapper {
    /// Visit a child node with automatic context management
    fn visit_child<F>(&mut self, slot: u16, visit_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.ctx.push_child(slot);
        visit_fn(self);
        self.ctx.pop_child();
    }

    fn insert_mapping(&mut self, id: NodeId, span: Span, kind: SpanNodeKind) {
        self.insert_mapping_with_context(id, span, kind, None);
    }

    fn insert_mapping_with_context(
        &mut self,
        id: NodeId,
        span: Span,
        kind: SpanNodeKind,
        symbol: Option<Symbol>,
    ) {
        // Skip nodes that live inside an attribute. Attributes themselves survive on the HIR,
        // but they are never lowered into standalone HIR nodes, so their contents cannot be
        // mapped one-for-one.
        if self.attribute_depth > 0 {
            return;
        }

        // Skip patterns inside field definitions - HIR doesn't create pattern nodes for field names
        if self.in_field_def && matches!(kind, SpanNodeKind::Pat) {
            return;
        }

        if id == DUMMY_NODE_ID || span.is_dummy() {
            return;
        }

        let ns = NodeSpan { span, kind };
        let old_ns = self.maps.node_id_to_span_map.insert(id, ns);
        let _old_id = self.maps.span_to_node_id_map.insert(ns, id);

        assert!(
            old_ns.is_none(),
            "id {id:?} already has span {old_ns:?} != {ns:?}"
        );

        // Build a structural fingerprint using the stacks accumulated so far.  This mirrors the
        // information the HIR visitor records and together they let us distinguish same-span
        // nodes emitted by derives and macros.
        let mut context = self.ctx.current_context();
        if let Some(stmt_idx) = self.ctx.current_stmt_index() {
            context = context.with_stmt_index(stmt_idx);
        }
        if context.owner.is_none() {
            if let Some(owner) = self.ctx.current_owner() {
                context = context.with_owner(Some(owner));
            }
        }
        if let Some(sym) = symbol {
            context = context.with_symbol(Some(sym));
        }

        // Store context mappings
        self.maps.node_id_to_context_map.insert(id, context.clone());
        let _old_context_id = self.maps.context_to_node_id_map.insert((ns, context), id);
    }
}

impl Visitor<'_> for AstSpanMapper {
    fn visit_crate(&mut self, krate: &Crate) {
        self.insert_mapping(krate.id, krate.spans.inner_span, SpanNodeKind::Crate);
        self.ctx.push_owner(krate.id);
        visit::walk_crate(self, krate);
        self.ctx.pop_owner();
    }

    fn visit_block(&mut self, block: &Block) {
        self.insert_mapping(block.id, block.span, SpanNodeKind::Block);

        // Track the active block so statement indices become part of the context key
        self.ctx.push_block(block.id);

        // Use default walker to ensure we visit all block children
        visit::walk_block(self, block);

        self.ctx.pop_block();
    }

    fn visit_pat(&mut self, pat: &Pat) {
        // Extract identifier symbol if it's a binding pattern
        let symbol = match &pat.kind {
            PatKind::Ident(_, ident, _) => Some(ident.name),
            _ => None,
        };
        self.insert_mapping_with_context(pat.id, pat.span, SpanNodeKind::Pat, symbol);
        visit::walk_pat(self, pat);
    }

    fn visit_pat_field(&mut self, pf: &PatField) {
        self.insert_mapping(pf.id, pf.span, SpanNodeKind::PatField);
        visit::walk_pat_field(self, pf);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        self.insert_mapping(stmt.id, stmt.span, SpanNodeKind::Stmt);
        visit::walk_stmt(self, stmt);
        // Increment statement index after visiting this statement
        self.ctx.next_stmt();
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
        // Extract identifier symbol if it's a path expression
        let symbol = match &expr.kind {
            ExprKind::Path(_, path) => path.segments.last().map(|seg| seg.ident.name),
            _ => None,
        };

        if matches!(expr.kind, ExprKind::Path(..)) {
            self.insert_mapping_with_context(expr.id, expr.span, SpanNodeKind::PathExpr, symbol);
        } else {
            self.insert_mapping_with_context(expr.id, expr.span, SpanNodeKind::Expr, symbol);
        }

        // Visit attributes first (required for attribute_depth tracking)
        for attr in &expr.attrs {
            self.visit_attribute(attr);
        }

        // Only enumerate expression forms whose AST and HIR shapes match 1:1.  These are the
        // cases where a child-slot path actually helps us distinguish cloned spans.
        match &expr.kind {
            ExprKind::Struct(..) => {
                visit::walk_expr(self, expr);
            }
            ExprKind::Tup(exprs) => {
                for (i, elem) in exprs.iter().enumerate() {
                    self.visit_child(child_slot::tuple_elem(i), |this| {
                        this.visit_expr(elem);
                    });
                }
            }
            ExprKind::Array(exprs) => {
                for (i, elem) in exprs.iter().enumerate() {
                    self.visit_child(child_slot::array_elem(i), |this| {
                        this.visit_expr(elem);
                    });
                }
            }
            ExprKind::Binary(_, lhs, rhs) => {
                self.visit_child(child_slot::BINARY_LHS, |this| {
                    this.visit_expr(lhs);
                });
                self.visit_child(child_slot::BINARY_RHS, |this| {
                    this.visit_expr(rhs);
                });
            }
            ExprKind::Unary(_, operand) => {
                self.visit_child(child_slot::UNARY_OPERAND, |this| {
                    this.visit_expr(operand);
                });
            }
            ExprKind::Call(callee, args) => {
                self.visit_child(child_slot::CALL_CALLEE, |this| {
                    this.visit_expr(callee);
                });
                for (i, arg) in args.iter().enumerate() {
                    self.visit_child(child_slot::call_arg(i), |this| {
                        this.visit_expr(arg);
                    });
                }
            }
            ExprKind::MethodCall(segment, args, _span) => {
                // Visit the method name/generics (PathSegment)
                self.visit_path_segment(expr.span, segment);
                // Visit receiver and arguments
                for (i, arg) in args.iter().enumerate() {
                    let slot = if i == 0 {
                        child_slot::METHOD_RECEIVER
                    } else {
                        child_slot::method_arg(i - 1)
                    };
                    self.visit_child(slot, |this| {
                        this.visit_expr(arg);
                    });
                }
            }
            _ => {
                // Defer to the default walker for every other expression kind.  Lowering often
                // rewrites those shapes (e.g. `if` blocks become expressions, matches grow guard
                // nodes), so any slot numbering we invent here would diverge from the HIR side
                // and reintroduce the very mismatches we are trying to eliminate.
                visit::walk_expr(self, expr);
            }
        }
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
        // Field name patterns don't exist in HIR
        self.in_field_def = true;
        visit::walk_field_def(self, fd);
        self.in_field_def = false;
    }

    fn visit_item(&mut self, i: &Item) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::Item);
        self.ctx.push_owner(i.id);
        visit::walk_item(self, i);
        self.ctx.pop_owner();
    }

    fn visit_foreign_item(&mut self, i: &ForeignItem) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::ForeignItem);
        self.ctx.push_owner(i.id);
        visit::walk_foreign_item(self, i);
        self.ctx.pop_owner();
    }

    fn visit_assoc_item(&mut self, i: &AssocItem, ctxt: AssocCtxt) {
        self.insert_mapping(i.id, i.span, SpanNodeKind::AssocItem);
        self.ctx.push_owner(i.id);
        visit::walk_assoc_item(self, i, ctxt);
        self.ctx.pop_owner();
    }

    fn visit_attribute(&mut self, attr: &Attribute) {
        // Skip mapping the attribute's contents. HIR keeps attributes attached to items/statements,
        // but it does not expose them as addressable nodes, so visiting inside would create entries
        // we could never match on the HIR side.
        self.attribute_depth += 1;
        visit::walk_attribute(self, attr);
        self.attribute_depth -= 1;
    }
}
