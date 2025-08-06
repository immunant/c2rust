use crate::c_ast::*;
use crate::diagnostics::diag;
use c2rust_ast_exporter::clang_ast::*;
use failure::err_msg;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

use super::Located;
use crate::diagnostics::{Diagnostic, TranslationError, TranslationErrorKind};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClangAstParseErrorKind {
    MissingChild,
    MissingType,
    MissingNode,
}

/// Possible node types
pub type NodeType = u16;

mod node_types {
    pub const FUNC_TYPE: super::NodeType = 0b0000000000000001;
    pub const OTHER_TYPE: super::NodeType = 0b0000000000000010;
    pub const TYPE: super::NodeType = FUNC_TYPE | OTHER_TYPE;

    pub const EXPR: super::NodeType = 0b0000000000000100;

    pub const FIELD_DECL: super::NodeType = 0b0000000000001000;
    pub const VAR_DECL: super::NodeType = 0b0000000000010000;
    pub const RECORD_DECL: super::NodeType = 0b0000000000100000;
    pub const TYPDEF_DECL: super::NodeType = 0b0000000001000000;
    pub const ENUM_DECL: super::NodeType = 0b0000000010000000;
    pub const ENUM_CON: super::NodeType = 0b0000000100000000;
    pub const MACRO_DECL: super::NodeType = 0b0000001000000000;
    pub const OTHER_DECL: super::NodeType = 0b0000010000000000;
    pub const DECL: super::NodeType = FIELD_DECL
        | VAR_DECL
        | RECORD_DECL
        | TYPDEF_DECL
        | ENUM_DECL
        | ENUM_CON
        | MACRO_DECL
        | OTHER_DECL;

    pub const LABEL_STMT: super::NodeType = 0b0000100000000000;
    pub const OTHER_STMT: super::NodeType = 0b0001000000000000;
    pub const STMT: super::NodeType = LABEL_STMT | OTHER_STMT;
}

type ClangId = u64;
type ImporterId = u64;

/// Correspondence between old/new IDs.
///
/// We need to re-ID nodes since the mapping from Clang's AST to ours is not one-to-one. Sometimes
/// we need to add nodes (such as 'Semi' nodes to make the lifting of expressions into statements
/// explicit), sometimes we need to collapse (such as inlining 'FieldDecl' into the 'StructDecl').
#[derive(Debug, Default)]
pub struct IdMapper {
    new_id_source: ImporterId,
    old_to_new: HashMap<ClangId, ImporterId>,
    new_to_old: HashMap<ImporterId, ClangId>,
}

impl IdMapper {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a fresh NEW_ID not corresponding to a CLANG_ID
    fn fresh_id(&mut self) -> ImporterId {
        self.new_id_source += 1;
        self.new_id_source
    }

    /// Lookup the NEW_ID corresponding to a CLANG_ID
    pub fn get_new(&mut self, old_id: ClangId) -> Option<ImporterId> {
        self.old_to_new.get(&old_id).copied()
    }

    /// Lookup (or create if not a found) a NEW_ID corresponding to a CLANG_ID
    pub fn get_or_create_new(&mut self, old_id: ClangId) -> ImporterId {
        match self.get_new(old_id) {
            Some(new_id) => new_id,
            None => {
                let new_id = self.fresh_id();
                let inserted = self.old_to_new.insert(old_id, new_id).is_some();
                assert!(
                    !inserted,
                    "get_or_create_new: overwrote an old id at {}",
                    old_id
                );
                new_id
            }
        }
    }

    /// Lookup the CLANG_ID corresponding to a NEW_ID
    pub fn get_old(&mut self, new_id: ImporterId) -> Option<ClangId> {
        self.new_to_old.get(&new_id).copied()
    }

    /// If the `old_id` is present in the mapper, make `other_old_id` map to the same value. Note
    /// that `other_old_id` should not already be in the mapper.
    pub fn merge_old(&mut self, old_id: ClangId, other_old_id: ClangId) -> Option<ImporterId> {
        self.get_new(old_id).map(|new_id| {
            let inserted = self.old_to_new.insert(other_old_id, new_id).is_some();
            assert!(
                !inserted,
                "get_or_create_new: overwrote an old id at {}",
                other_old_id
            );
            new_id
        })
    }
}

/// Transfer location information off of an `AstNode` and onto something that is `Located`
fn located<T>(node: &AstNode, t: T) -> Located<T> {
    Located {
        loc: Some(node.loc),
        kind: t,
    }
}

/// Wrap something into a `Located` node without any location information
fn not_located<T>(t: T) -> Located<T> {
    Located { loc: None, kind: t }
}

fn parse_cast_kind(kind: &str) -> CastKind {
    match kind {
        "BitCast" => CastKind::BitCast,
        "LValueToRValue" => CastKind::LValueToRValue,
        "NoOp" => CastKind::NoOp,
        "ToUnion" => CastKind::ToUnion,
        "ArrayToPointerDecay" => CastKind::ArrayToPointerDecay,
        "FunctionToPointerDecay" => CastKind::FunctionToPointerDecay,
        "NullToPointer" => CastKind::NullToPointer,
        "IntegralToPointer" => CastKind::IntegralToPointer,
        "PointerToIntegral" => CastKind::PointerToIntegral,
        "ToVoid" => CastKind::ToVoid,
        "IntegralCast" => CastKind::IntegralCast,
        "IntegralToBoolean" => CastKind::IntegralToBoolean,
        "IntegralToFloating" => CastKind::IntegralToFloating,
        "FloatingToIntegral" => CastKind::FloatingToIntegral,
        "FloatingToBoolean" => CastKind::FloatingToBoolean,
        "BooleanToSignedIntegral" => CastKind::BooleanToSignedIntegral,
        "PointerToBoolean" => CastKind::PointerToBoolean,
        "FloatingCast" => CastKind::FloatingCast,
        "FloatingRealToComplex" => CastKind::FloatingRealToComplex,
        "FloatingComplexToReal" => CastKind::FloatingComplexToReal,
        "FloatingComplexCast" => CastKind::FloatingComplexCast,
        "FloatingComplexToIntegralComplex" => CastKind::FloatingComplexToIntegralComplex,
        "IntegralRealToComplex" => CastKind::IntegralRealToComplex,
        "IntegralComplexToReal" => CastKind::IntegralComplexToReal,
        "IntegralComplexToBoolean" => CastKind::IntegralComplexToBoolean,
        "IntegralComplexCast" => CastKind::IntegralComplexCast,
        "IntegralComplexToFloatingComplex" => CastKind::IntegralComplexToFloatingComplex,
        "BuiltinFnToFnPtr" => CastKind::BuiltinFnToFnPtr,
        "ConstCast" => CastKind::ConstCast,
        "VectorSplat" => CastKind::VectorSplat,
        "AtomicToNonAtomic" => CastKind::AtomicToNonAtomic,
        "NonAtomicToAtomic" => CastKind::NonAtomicToAtomic,
        k => panic!("Unsupported implicit cast: {}", k),
    }
}

fn parse_attributes(attributes: Vec<Value>) -> IndexSet<Attribute> {
    let mut attrs = IndexSet::new();
    let mut expect_section_value = false;
    let mut expect_alias_value = false;
    let mut expect_visibility_value = false;

    for attr in attributes.into_iter() {
        let attr_str = from_value::<String>(attr).expect("Decl attributes should be strings");

        match attr_str.as_str() {
            "alias" => expect_alias_value = true,
            "always_inline" => {
                attrs.insert(Attribute::AlwaysInline);
            }
            "cold" => {
                attrs.insert(Attribute::Cold);
            }
            "gnu_inline" => {
                attrs.insert(Attribute::GnuInline);
            }
            "noinline" => {
                attrs.insert(Attribute::NoInline);
            }
            "used" => {
                attrs.insert(Attribute::Used);
            }
            "visibility" => expect_visibility_value = true,
            "section" => expect_section_value = true,
            s if expect_section_value => {
                attrs.insert(Attribute::Section(s.into()));

                expect_section_value = false;
            }
            s if expect_alias_value => {
                attrs.insert(Attribute::Alias(s.into()));

                expect_alias_value = false;
            }
            s if expect_visibility_value => {
                attrs.insert(Attribute::Visibility(s.into()));

                expect_visibility_value = false;
            }
            _ => {}
        }
    }

    attrs
}

/// This stores the information needed to convert an `AstContext` into a `TypedAstContext`.
pub struct ConversionContext {
    /// Keeps track of the mapping between IDs used by clang (old) and the AST importer (new)
    pub id_mapper: IdMapper,

    /// Keep track of new nodes already processed and their types
    processed_nodes: HashMap<ImporterId, NodeType>,

    /// Stack of nodes to visit, and the types we expect to see out of them
    visit_as: Vec<(ClangId, NodeType)>,

    /// Typed context we are building up during the conversion
    typed_context: TypedAstContext,

    pub invalid_clang_ast: bool,
}

impl ConversionContext {
    pub fn into_typed_context(self) -> TypedAstContext {
        self.typed_context
    }
}

fn display_loc(ctx: &AstContext, loc: &Option<SrcSpan>) -> Option<DisplaySrcSpan> {
    loc.as_ref().map(|loc| DisplaySrcSpan {
        file: ctx.files[loc.fileid as usize].path.clone(),
        loc: *loc,
    })
}

fn has_packed_attribute(attrs: Vec<Value>) -> bool {
    attrs
        .into_iter()
        .map(|attr| from_value::<String>(attr).expect("Record attributes should be strings"))
        .any(|attr_name| attr_name == "packed")
}

impl ConversionContext {
    /// Create a new 'ConversionContext' seeded with top-level nodes from an 'AstContext'.
    pub fn new(untyped_context: &AstContext) -> ConversionContext {
        let mut invalid_clang_ast = false;

        // This starts out as all of the top-level nodes, which we expect to be 'DECL's
        let mut visit_as: Vec<(ClangId, NodeType)> = Vec::new();
        for top_node in untyped_context.top_nodes.iter().rev() {
            if untyped_context.ast_nodes.contains_key(top_node) {
                visit_as.push((*top_node, node_types::DECL));
            } else {
                diag!(
                    Diagnostic::ClangAst,
                    "{}",
                    TranslationError::new(
                        None,
                        err_msg(format!("Missing top-level node with id: {}", top_node)).context(
                            TranslationErrorKind::InvalidClangAst(
                                ClangAstParseErrorKind::MissingNode,
                            )
                        )
                    ),
                );
                invalid_clang_ast = true;
            }
        }

        for node in untyped_context.ast_nodes.values() {
            for child in node.children.iter().flatten() {
                if !untyped_context.ast_nodes.contains_key(child) {
                    diag!(
                        Diagnostic::ClangAst,
                        "{}",
                        TranslationError::new(
                            display_loc(untyped_context, &Some(node.loc)),
                            err_msg(format!("Missing child {} of node {:?}", child, node,))
                                .context(TranslationErrorKind::InvalidClangAst(
                                    ClangAstParseErrorKind::MissingChild,
                                )),
                        ),
                    );
                    invalid_clang_ast = true;
                }
            }

            if let Some(type_id) = &node.type_id {
                let type_ptr = type_id & TypeNode::ID_MASK;
                if !untyped_context.type_nodes.contains_key(&type_ptr) {
                    diag!(
                        Diagnostic::ClangAst,
                        "{}",
                        TranslationError::new(
                            display_loc(untyped_context, &Some(node.loc)),
                            err_msg(format!("Missing type {} for node: {:?}", type_id, node,))
                                .context(TranslationErrorKind::InvalidClangAst(
                                    ClangAstParseErrorKind::MissingType,
                                )),
                        ),
                    );
                    invalid_clang_ast = true;
                }
            }
        }

        let mut ctx = ConversionContext {
            id_mapper: IdMapper::new(),
            processed_nodes: HashMap::new(),
            visit_as,
            typed_context: TypedAstContext::new(&untyped_context.files),
            invalid_clang_ast,
        };

        ctx.convert(untyped_context);
        ctx
    }

    /// Records the fact that we will need to visit a Clang node and the type we want it to have.
    ///
    /// Returns the new ID that identifies this new node.
    fn visit_node_type(&mut self, node_id: ClangId, node_ty: NodeType) -> ImporterId {
        // Type node IDs have extra information on them
        let node_id = if node_ty & node_types::TYPE != 0 {
            node_id & TypeNode::ID_MASK
        } else {
            node_id
        };

        self.visit_as.push((node_id, node_ty));
        self.id_mapper.get_or_create_new(node_id)
    }

    /// Like `visit_node_type`, but specifically for type nodes
    fn visit_type(&mut self, node_id: ClangId) -> CTypeId {
        CTypeId(self.visit_node_type(node_id, node_types::TYPE))
    }

    /// Like `visit_node_type`, but specifically for qualified type nodes
    ///
    /// Since we store qualifier information on the low-bits of the `ClangId`, we don't even need to
    /// look the node up in the context.
    fn visit_qualified_type(&mut self, node_id: ClangId) -> CQualTypeId {
        let qualifiers = Qualifiers {
            is_const: node_id & TypeNode::CONST_MASK != 0,
            is_restrict: node_id & TypeNode::RESTRICT_MASK != 0,
            is_volatile: node_id & TypeNode::VOLATILE_MASK != 0,
        };
        let ctype = self.visit_type(node_id);

        CQualTypeId { qualifiers, ctype }
    }

    /// Like `visit_node_type`, but specifically for statement nodes
    fn visit_stmt(&mut self, node_id: ClangId) -> CStmtId {
        CStmtId(self.visit_node_type(node_id, node_types::STMT))
    }

    /// Like `visit_node_type`, but specifically for expression nodes
    fn visit_expr(&mut self, node_id: ClangId) -> CExprId {
        CExprId(self.visit_node_type(node_id, node_types::EXPR))
    }

    /// Like `visit_node_type`, but specifically for declaration nodes
    fn visit_decl(&mut self, node_id: ClangId) -> CDeclId {
        CDeclId(self.visit_node_type(node_id, node_types::DECL))
    }

    /// Add a `CType`node into the `TypedAstContext`
    fn add_type(&mut self, id: ImporterId, typ: CType) {
        self.typed_context.c_types.insert(CTypeId(id), typ);
    }

    /// Add a `CStmt` node into the `TypedAstContext`
    fn add_stmt(&mut self, id: ImporterId, stmt: CStmt) {
        self.typed_context.c_stmts.insert(CStmtId(id), stmt);
    }

    /// Add a `CExpr` node into the `TypedAstContext`
    fn add_expr(&mut self, id: ImporterId, expr: CExpr) {
        self.typed_context.c_exprs.insert(CExprId(id), expr);
    }

    /// Add a `CDecl` node into the `TypedAstContext`
    fn add_decl(&mut self, id: ImporterId, decl: CDecl) {
        self.typed_context.c_decls.insert(CDeclId(id), decl);
    }

    /// Clang has `Expression <: Statement`, but we want to make that explicit via the
    /// `CStmtKind::Expr` statement constructor. This function automatically converts expressions
    /// into statements depending on the expected type argument.
    fn expr_possibly_as_stmt(
        &mut self,
        expected_ty: NodeType, // Should be one of `EXPR` or `STMT`
        new_id: ImporterId,
        node: &AstNode,
        expr: CExprKind,
    ) {
        if expected_ty & node_types::STMT != 0 {
            // This is going to be an extra node not present in the Clang AST
            let new_expr_id = self.id_mapper.fresh_id();
            self.add_expr(new_expr_id, located(node, expr));
            self.processed_nodes.insert(new_expr_id, node_types::EXPR);

            // We wrap the expression in a STMT
            let semi_stmt = CStmtKind::Expr(CExprId(new_expr_id));
            self.add_stmt(new_id, located(node, semi_stmt));
            self.processed_nodes.insert(new_id, node_types::STMT);
        } else if expected_ty & node_types::EXPR != 0 {
            // No special work to do...
            self.add_expr(new_id, located(node, expr));
            self.processed_nodes.insert(new_id, node_types::EXPR);
        } else {
            panic!("'expr_possibly_as_stmt' expects 'expected_ty' to be either 'EXPR' or 'STMT'");
        }
    }

    /// Convert the contents of an `AstContext`, starting from the top-level declarations passed
    /// into the `ConversionContext` on creation.
    ///
    /// This populates the `typed_context` of the `ConversionContext` it is called on.
    fn convert(&mut self, untyped_context: &AstContext) {
        for raw_comment in &untyped_context.comments {
            let comment = Located {
                loc: Some(raw_comment.loc.into()),
                kind: raw_comment.string.clone(),
            };
            self.typed_context.comments.push(comment);
        }

        // Add Rust fixed-size types.
        for rust_type_kind in CTypeKind::PULLBACK_KINDS {
            let new_id = self.id_mapper.fresh_id();
            self.add_type(new_id, not_located(rust_type_kind));
            self.processed_nodes
                .insert(new_id, self::node_types::OTHER_TYPE);
        }

        // Continue popping Clang nodes off of the stack of nodes we have promised to visit
        while let Some((node_id, expected_ty)) = self.visit_as.pop() {
            // Check if we've already processed this node. If so, ascertain that it has the right
            // type.
            if let Some(ty) = self
                .id_mapper
                .get_new(node_id)
                .and_then(|new_id| self.processed_nodes.get(&new_id))
            {
                if ty & expected_ty != 0 {
                    continue;
                }
                panic!(
                    "Expected {} to be a node of type {}, not {}",
                    &node_id, expected_ty, ty
                );
            }

            // Create a `NewId` for this node
            let new_id = self.id_mapper.get_or_create_new(node_id);

            // If the node is top-level, add it as such to the new context
            if untyped_context.top_nodes.contains(&node_id) {
                self.typed_context.c_decls_top.push(CDeclId(new_id));
            }

            self.visit_node(untyped_context, node_id, new_id, expected_ty)
        }

        // Function declarations' types look through typedefs, but we want to use the types with
        // typedefs intact in some cases during translation. To ensure that these types exist in the
        // `TypedAstContext`, iterate over all function decls, compute their adjusted type using
        // argument types from arg declarations, and then ensure the existence of both this type and
        // the type of pointers to it.
        for (_decl_id, located_kind) in self.typed_context.c_decls.iter() {
            if let kind @ CDeclKind::Function { .. } = &located_kind.kind {
                let new_kind = self.typed_context.fn_decl_ty_with_declared_args(kind);
                if self.typed_context.type_for_kind(&new_kind).is_none() {
                    // Create and insert fn type
                    let new_id = CTypeId(self.id_mapper.fresh_id());
                    self.typed_context
                        .c_types
                        .insert(new_id, not_located(new_kind));
                    // Create and insert fn ptr type
                    let ptr_kind = CTypeKind::Pointer(CQualTypeId::new(new_id));
                    let ptr_id = CTypeId(self.id_mapper.fresh_id());
                    self.typed_context
                        .c_types
                        .insert(ptr_id, not_located(ptr_kind));
                }
            }
        }

        // Invert the macro invocations to get a list of macro expansion expressions
        for (expr_id, macro_ids) in &self.typed_context.macro_invocations {
            for mac_id in macro_ids {
                self.typed_context
                    .macro_expansions
                    .entry(*mac_id)
                    .or_default()
                    .push(*expr_id);
            }
        }

        self.typed_context.va_list_kind = untyped_context.va_list_kind;
        self.typed_context.target = untyped_context.target.clone();
    }

    /// Visit one node.
    fn visit_node(
        &mut self,
        untyped_context: &AstContext,
        node_id: ClangId,      // Clang ID of node to visit
        new_id: ImporterId,    // New ID of node to visit
        expected_ty: NodeType, // Expected type of node to visit
    ) {
        use self::node_types::*;

        if expected_ty & TYPE != 0 {
            // Convert the node
            let ty_node: &TypeNode = match untyped_context.type_nodes.get(&node_id) {
                Some(x) => x,
                None => return,
            };

            match ty_node.tag {
                TypeTag::TagBool if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Bool));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagVoid if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Void));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagChar if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Char));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagInt if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Int));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagShort if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Short));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagLong if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Long));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagLongLong if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::LongLong));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUInt if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::UInt));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUChar if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::UChar));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagSChar if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::SChar));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUShort if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::UShort));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagULong if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::ULong));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagULongLong if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::ULongLong));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagDouble if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Double));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagLongDouble if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::LongDouble));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagFloat if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Float));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagHalf if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Half));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBFloat16 if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::BFloat16));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagInt128 if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Int128));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUInt128 if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::UInt128));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagPointer if expected_ty & OTHER_TYPE != 0 => {
                    let pointed =
                        from_value(ty_node.extras[0].clone()).expect("Pointer child not found");
                    let pointed_new = self.visit_qualified_type(pointed);

                    let pointer_ty = CTypeKind::Pointer(pointed_new);
                    self.add_type(new_id, not_located(pointer_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagReference if expected_ty & OTHER_TYPE != 0 => {
                    let referenced =
                        from_value(ty_node.extras[0].clone()).expect("Reference child not found");
                    let referenced_new = self.visit_qualified_type(referenced);

                    let reference_ty = CTypeKind::Reference(referenced_new);
                    self.add_type(new_id, not_located(reference_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBlockPointer if expected_ty & OTHER_TYPE != 0 => {
                    let pointed = from_value(ty_node.extras[0].clone())
                        .expect("Block pointer child not found");
                    let pointed_new = self.visit_qualified_type(pointed);

                    let pointer_ty = CTypeKind::BlockPointer(pointed_new);
                    self.add_type(new_id, not_located(pointer_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagComplexType if expected_ty & OTHER_TYPE != 0 => {
                    let subelt =
                        from_value(ty_node.extras[0].clone()).expect("Complex child not found");
                    let subelt_new = self.visit_type(subelt);

                    let complex_ty = CTypeKind::Complex(subelt_new);
                    self.add_type(new_id, not_located(complex_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagStructType if expected_ty & OTHER_TYPE != 0 => {
                    let decl =
                        from_value(ty_node.extras[0].clone()).expect("Struct decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, RECORD_DECL));

                    let record_ty = CTypeKind::Struct(decl_new);
                    self.add_type(new_id, not_located(record_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUnionType if expected_ty & OTHER_TYPE != 0 => {
                    let decl = from_value(ty_node.extras[0].clone()).expect("Union decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, RECORD_DECL));

                    let record_ty = CTypeKind::Union(decl_new);
                    self.add_type(new_id, not_located(record_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagFunctionType if expected_ty & FUNC_TYPE != 0 => {
                    let mut arguments: Vec<CQualTypeId> =
                        from_value::<Vec<Value>>(ty_node.extras[0].clone())
                            .expect("Function type expects array argument")
                            .iter()
                            .map(|cbor| {
                                let arg =
                                    from_value(cbor.clone()).expect("Bad function type child id");

                                self.visit_qualified_type(arg)
                            })
                            .collect();
                    let ret = arguments.remove(0);
                    let is_variadic = from_value(ty_node.extras[1].clone())
                        .expect("Variadicity of function type not found");
                    let is_noreturn = from_value(ty_node.extras[2].clone())
                        .expect("NoReturn of function type not found");
                    let has_proto = from_value(ty_node.extras[3].clone())
                        .expect("HasProto of function type not found");
                    let function_ty =
                        CTypeKind::Function(ret, arguments, is_variadic, is_noreturn, has_proto);
                    self.add_type(new_id, not_located(function_ty));
                    self.processed_nodes.insert(new_id, FUNC_TYPE);

                    // In addition to creating the function type for this node, ensure that a
                    // corresponding function pointer type is created. We may need to reference this
                    // type depending on how uses of functions of this type are translated.
                    let pointer_ty = CTypeKind::Pointer(CQualTypeId::new(CTypeId(new_id)));
                    let new_id = self.id_mapper.fresh_id();
                    self.add_type(new_id, not_located(pointer_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagTypeOfType if expected_ty & TYPE != 0 => {
                    let type_of_old = from_value(ty_node.extras[0].clone())
                        .expect("Type of (type) child not found");
                    let type_of = self.visit_type(type_of_old);

                    let type_of_ty = CTypeKind::TypeOf(type_of);
                    self.add_type(new_id, not_located(type_of_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagTypedefType => {
                    let decl =
                        from_value(ty_node.extras[0].clone()).expect("Typedef decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, TYPDEF_DECL));

                    let typedef_ty = CTypeKind::Typedef(decl_new);
                    self.add_type(new_id, not_located(typedef_ty));
                    self.processed_nodes.insert(new_id, expected_ty);
                }

                TypeTag::TagEnumType if expected_ty & OTHER_TYPE != 0 => {
                    let decl = from_value(ty_node.extras[0].clone()).expect("Enum decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, ENUM_DECL));

                    let enum_ty = CTypeKind::Enum(decl_new);
                    self.add_type(new_id, not_located(enum_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagDecayedType if expected_ty & OTHER_TYPE != 0 => {
                    let decayed_id = from_value(ty_node.extras[0].clone())
                        .expect("Decayed type child not found");
                    let decayed = self.visit_type(decayed_id);

                    let decayed_ty = CTypeKind::Decayed(decayed);
                    self.add_type(new_id, not_located(decayed_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagElaboratedType if expected_ty & OTHER_TYPE != 0 => {
                    let elaborated_id = from_value(ty_node.extras[0].clone())
                        .expect("Elaborated type child not found");
                    let elaborated = self.visit_type(elaborated_id);

                    let elaborated_ty = CTypeKind::Elaborated(elaborated);
                    self.add_type(new_id, not_located(elaborated_ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagParenType => {
                    let paren_id =
                        from_value(ty_node.extras[0].clone()).expect("Paren type child not found");
                    let paren = self.visit_type(paren_id);

                    let paren_ty = CTypeKind::Paren(paren);
                    self.add_type(new_id, not_located(paren_ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagAttributedType => {
                    let ty_id = from_value(ty_node.extras[0].clone())
                        .expect("Attributed type child not found");
                    let ty = self.visit_qualified_type(ty_id);

                    let kind = match expect_opt_str(&ty_node.extras[1])
                        .expect("Attributed type kind not found")
                    {
                        None => None,
                        Some("noreturn") => Some(Attribute::NoReturn),
                        Some("nullable") => Some(Attribute::Nullable),
                        Some("notnull") => Some(Attribute::NotNull),
                        Some(other) => panic!("Unknown type attribute: {}", other),
                    };

                    let ty = CTypeKind::Attributed(ty, kind);
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagConstantArrayType => {
                    let element_id = from_value(ty_node.extras[0].clone()).expect("element id");
                    let element = self.visit_type(element_id);

                    let count: usize = from_value(ty_node.extras[1].clone()).expect("count");

                    let element_ty = CTypeKind::ConstantArray(element, count);
                    self.add_type(new_id, not_located(element_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagIncompleteArrayType => {
                    let element_id = from_value(ty_node.extras[0].clone()).expect("element id");
                    let element = self.visit_type(element_id);

                    let element_ty = CTypeKind::IncompleteArray(element);
                    self.add_type(new_id, not_located(element_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagVariableArrayType => {
                    let element_id = from_value(ty_node.extras[0].clone()).expect("element id");
                    let element = self.visit_type(element_id);

                    let count_id = expect_opt_u64(&ty_node.extras[1]).expect("count id");
                    let count = count_id.map(|x| self.visit_expr(x));

                    let element_ty = CTypeKind::VariableArray(element, count);
                    self.add_type(new_id, not_located(element_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBuiltinFn => {
                    let ty = CTypeKind::BuiltinFn;
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBFloat16 => {
                    let ty = CTypeKind::BFloat16;
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagSveCount
                | TypeTag::TagSveBool
                | TypeTag::TagSveBoolx2
                | TypeTag::TagSveBoolx4 => {
                    let ty = CTypeKind::UnhandledSveType;
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagFloat128 => {
                    let ty = CTypeKind::Float128;
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagVectorType => {
                    let elt =
                        from_value(ty_node.extras[0].clone()).expect("Vector child not found");
                    let elt_new = self.visit_qualified_type(elt);
                    let count: usize = from_value(ty_node.extras[1].clone()).expect("count");

                    let vector_ty = CTypeKind::Vector(elt_new, count);
                    self.add_type(new_id, not_located(vector_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagAtomicType => {
                    let qty = from_value(ty_node.extras[0].clone()).expect("Inner type not found");
                    let qty_new = self.visit_qualified_type(qty);
                    let atomic_ty = CTypeKind::Atomic(qty_new);
                    self.add_type(new_id, not_located(atomic_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                t => panic!(
                    "Type conversion not implemented for {:?} expecting {:?}",
                    t, expected_ty
                ),
            }
        } else {
            // Convert the node
            let node: &AstNode = match untyped_context.ast_nodes.get(&node_id) {
                Some(x) => x,
                None => return,
            };

            if expected_ty & EXPR != 0 {
                for mac_id in &node.macro_expansions {
                    let mac = CDeclId(self.visit_node_type(*mac_id, MACRO_DECL));
                    self.typed_context
                        .macro_invocations
                        .entry(CExprId(new_id))
                        .or_default()
                        .push(mac);
                }
            }

            if let Some(text) = &node.macro_expansion_text {
                self.typed_context
                    .macro_expansion_text
                    .insert(CExprId(new_id), text.clone());
            }

            match node.tag {
                // Statements
                ASTEntryTag::TagBreakStmt if expected_ty & OTHER_STMT != 0 => {
                    self.add_stmt(new_id, located(node, CStmtKind::Break));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagContinueStmt if expected_ty & OTHER_STMT != 0 => {
                    self.add_stmt(new_id, located(node, CStmtKind::Continue));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagCompoundStmt if expected_ty & OTHER_STMT != 0 => {
                    let constituent_stmts: Vec<CStmtId> = node
                        .children
                        .iter()
                        .map(|id| {
                            let arg_id = id.expect("Compound stmt child not found");
                            self.visit_stmt(arg_id)
                        })
                        .collect();

                    let compound_stmt = CStmtKind::Compound(constituent_stmts);

                    self.add_stmt(new_id, located(node, compound_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagDeclStmt if expected_ty & OTHER_STMT != 0 => {
                    let decls = node
                        .children
                        .iter()
                        .map(|decl| {
                            let decl_id = decl.expect("Decl not found in decl-statement");
                            self.visit_decl(decl_id)
                        })
                        .collect();

                    let decls_stmt = CStmtKind::Decls(decls);

                    self.add_stmt(new_id, located(node, decls_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagReturnStmt if expected_ty & OTHER_STMT != 0 => {
                    let return_expr_opt = node.children[0].map(|id| self.visit_expr(id));

                    let return_stmt = CStmtKind::Return(return_expr_opt);

                    self.add_stmt(new_id, located(node, return_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagIfStmt if expected_ty & OTHER_STMT != 0 => {
                    let scrutinee_old =
                        node.children[0].expect("If condition expression not found");
                    let scrutinee = self.visit_expr(scrutinee_old);

                    let true_variant_old =
                        node.children[1].expect("If then body statement not found");
                    let true_variant = self.visit_stmt(true_variant_old);

                    let false_variant = node.children[2].map(|id| self.visit_stmt(id));

                    let if_stmt = CStmtKind::If {
                        scrutinee,
                        true_variant,
                        false_variant,
                    };

                    self.add_stmt(new_id, located(node, if_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagGotoStmt if expected_ty & OTHER_STMT != 0 => {
                    let target_label_old = node.children[0].expect("Goto target label not found");
                    let target_label = CStmtId(self.visit_node_type(target_label_old, LABEL_STMT));

                    let goto_stmt = CStmtKind::Goto(target_label);

                    self.add_stmt(new_id, located(node, goto_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagNullStmt if expected_ty & OTHER_STMT != 0 => {
                    let null_stmt = CStmtKind::Empty;

                    self.add_stmt(new_id, located(node, null_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagAttributedStmt if expected_ty & OTHER_STMT != 0 => {
                    let substatement = node.children[0].map(|id| self.visit_stmt(id)).unwrap();
                    let mut attributes = vec![];

                    match expect_opt_str(&node.extras[0])
                        .expect("Attributed statement kind not found")
                    {
                        Some("fallthrough") | Some("__fallthrough__") => {
                            attributes.push(Attribute::Fallthrough)
                        }
                        Some(str) => panic!("Unknown statement attribute: {}", str),
                        None => panic!("Invalid statement attribute"),
                    };

                    let astmt = CStmtKind::Attributed {
                        attributes,
                        substatement,
                    };

                    self.add_stmt(new_id, located(node, astmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagForStmt if expected_ty & OTHER_STMT != 0 => {
                    let init = node.children[0].map(|id| self.visit_stmt(id));

                    let condition = node.children[1].map(|id| self.visit_expr(id));

                    let increment = node.children[2].map(|id| self.visit_expr(id));

                    let body_old = node.children[3].expect("For loop body not found");
                    let body = self.visit_stmt(body_old);

                    let for_stmt = CStmtKind::ForLoop {
                        init,
                        condition,
                        increment,
                        body,
                    };

                    self.add_stmt(new_id, located(node, for_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagWhileStmt if expected_ty & OTHER_STMT != 0 => {
                    let condition_old = node.children[0].expect("While loop condition not found");
                    let condition = self.visit_expr(condition_old);

                    let body_old = node.children[1].expect("While loop body not found");
                    let body = self.visit_stmt(body_old);

                    let while_stmt = CStmtKind::While { condition, body };

                    self.add_stmt(new_id, located(node, while_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagDoStmt if expected_ty & OTHER_STMT != 0 => {
                    let body_old = node.children[0].expect("Do loop body not found");
                    let body = self.visit_stmt(body_old);

                    let condition_old = node.children[1].expect("Do loop condition not found");
                    let condition = self.visit_expr(condition_old);

                    let do_stmt = CStmtKind::DoWhile { body, condition };

                    self.add_stmt(new_id, located(node, do_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagLabelStmt if expected_ty & LABEL_STMT != 0 => {
                    let substmt_old = node.children[0].expect("Label sub-statement not found");
                    let substmt = self.visit_stmt(substmt_old);

                    let label_name = from_value::<Rc<str>>(node.extras[0].clone())
                        .expect("unnamed label in C source code");
                    if let Some(old_label_name) = self
                        .typed_context
                        .label_names
                        .insert(CStmtId(new_id), label_name.clone())
                    {
                        panic!(
                            "Duplicate label name with id {}. Old name: {}. New name: {}",
                            new_id, old_label_name, label_name,
                        );
                    }

                    let label_stmt = CStmtKind::Label(substmt);

                    self.add_stmt(new_id, located(node, label_stmt));
                    self.processed_nodes.insert(new_id, LABEL_STMT);
                }

                ASTEntryTag::TagSwitchStmt if expected_ty & OTHER_STMT != 0 => {
                    let scrutinee_old = node.children[0].expect("Switch expression not found");
                    let scrutinee = self.visit_expr(scrutinee_old);

                    let body_old = node.children[1].expect("Switch body not found");
                    let body = self.visit_stmt(body_old);

                    let switch_stmt = CStmtKind::Switch { scrutinee, body };

                    self.add_stmt(new_id, located(node, switch_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagCaseStmt if expected_ty & OTHER_STMT != 0 => {
                    let expr_old = node.children[0].expect("Case expression not found");
                    let expr = self.visit_expr(expr_old);

                    let substmt_old = node.children[1].expect("Case sub-statement not found");
                    let substmt = self.visit_stmt(substmt_old);

                    let is_signed = from_value(node.extras[0].clone())
                        .expect("Case constant is_signed not found");
                    let cie = match is_signed {
                        false => ConstIntExpr::U(
                            from_value(node.extras[1].clone()).expect("Case constant not found"),
                        ),
                        true => ConstIntExpr::I(
                            from_value(node.extras[1].clone()).expect("Case constant not found"),
                        ),
                    };

                    let case_stmt = CStmtKind::Case(expr, substmt, cie);

                    self.add_stmt(new_id, located(node, case_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagDefaultStmt if expected_ty & OTHER_STMT != 0 => {
                    let substmt_old = node.children[0].expect("Default sub-statement not found");
                    let substmt = self.visit_stmt(substmt_old);

                    let default_stmt = CStmtKind::Default(substmt);

                    self.add_stmt(new_id, located(node, default_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagAsmStmt if expected_ty & OTHER_STMT != 0 => {
                    let is_volatile = from_value(node.extras[0].clone()).expect("volatile flag");
                    let asm = from_value(node.extras[1].clone()).expect("assembly string");
                    let raw_inputs = from_value::<Vec<Value>>(node.extras[2].clone())
                        .expect("input constraints array");
                    let raw_outputs = from_value::<Vec<Value>>(node.extras[3].clone())
                        .expect("output constraints array");
                    let raw_clobbers =
                        from_value::<Vec<Value>>(node.extras[4].clone()).expect("clobber array");

                    let (input_children, output_children) =
                        node.children.split_at(raw_inputs.len());

                    let inputs: Vec<AsmOperand> = raw_inputs
                        .into_iter()
                        .zip(input_children)
                        .map(|(c, e)| {
                            let constraints = from_value(c).expect("constraint string");
                            let expression = self.visit_expr(e.expect("expression"));
                            AsmOperand {
                                constraints,
                                expression,
                            }
                        })
                        .collect();

                    let outputs: Vec<AsmOperand> = raw_outputs
                        .into_iter()
                        .zip(output_children)
                        .map(|(c, e)| {
                            let constraints = from_value(c).expect("constraint string");
                            let expression = self.visit_expr(e.expect("expression"));
                            AsmOperand {
                                constraints,
                                expression,
                            }
                        })
                        .collect();

                    let clobbers: Vec<String> = raw_clobbers
                        .into_iter()
                        .map(|c| from_value(c).expect("clobber string"))
                        .collect();

                    let stmt = CStmtKind::Asm {
                        is_volatile,
                        asm,
                        inputs,
                        outputs,
                        clobbers,
                    };
                    self.add_stmt(new_id, located(node, stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                // Expressions
                ASTEntryTag::TagParenExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let wrapped = node.children[0].expect("Expected wrapped paren expression");
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let expr = CExprKind::Paren(ty, self.visit_expr(wrapped));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, expr);
                }

                ASTEntryTag::TagOffsetOfExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);
                    // Either we're able to evaluate the offsetof to an int constant expr
                    // or else we have to use the offset_of! macro from the memoffset crate
                    let offset_of = if let Ok(value) = from_value(node.extras[0].clone()) {
                        let kind = OffsetOfKind::Constant(value);

                        CExprKind::OffsetOf(ty, kind)
                    } else {
                        let qty_int = from_value(node.extras[1].clone())
                            .expect("Expected offset of to have struct type");
                        let qty = self.visit_qualified_type(qty_int);
                        let field =
                            from_value(node.extras[2].clone()).expect("Expected offset of field");
                        let field_id = self.visit_decl(field);
                        let index = from_value(node.extras[3].clone())
                            .expect("Expected offset of index expr");
                        let index_expr_id = self.visit_expr(index);
                        let kind = OffsetOfKind::Variable(qty, field_id, index_expr_id);

                        CExprKind::OffsetOf(ty, kind)
                    };

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, offset_of);
                }

                ASTEntryTag::TagIntegerLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value =
                        from_value(node.extras[0].clone()).expect("Expected integer literal value");
                    let base =
                        from_value(node.extras[1].clone()).expect("Expected integer base value");

                    let base = match base {
                        8 => IntBase::Oct,
                        10 => IntBase::Dec,
                        16 => IntBase::Hex,
                        _ => panic!("Invalid base: {}", base),
                    };

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let integer_literal = CExprKind::Literal(ty, CLiteral::Integer(value, base));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, integer_literal);
                }

                ASTEntryTag::TagStringLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);
                    let width: u8 =
                        from_value(node.extras[1].clone()).expect("string literal char width");
                    let bytes = from_value::<ByteBuf>(node.extras[2].clone())
                        .expect("string literal bytes");
                    let string_literal =
                        CExprKind::Literal(ty, CLiteral::String(bytes.into_vec(), width));
                    self.expr_possibly_as_stmt(expected_ty, new_id, node, string_literal);
                }

                ASTEntryTag::TagCharacterLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = from_value(node.extras[0].clone())
                        .expect("Expected character literal value");

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let character_literal = CExprKind::Literal(ty, CLiteral::Character(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, character_literal);
                }

                ASTEntryTag::TagFloatingLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value =
                        from_value(node.extras[0].clone()).expect("Expected float literal value");
                    let c_str = from_value::<String>(node.extras[1].clone())
                        .expect("Expected float literal string");
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let floating_literal = CExprKind::Literal(ty, CLiteral::Floating(value, c_str));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, floating_literal);
                }

                ASTEntryTag::TagUnaryOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let prefix =
                        from_value(node.extras[1].clone()).expect("Expected prefix information");

                    let operator = match from_value::<String>(node.extras[0].clone())
                        .expect("Expected operator")
                        .as_str()
                    {
                        "&" => UnOp::AddressOf,
                        "*" => UnOp::Deref,
                        "+" => UnOp::Plus,
                        "-" => UnOp::Negate,
                        "~" => UnOp::Complement,
                        "!" => UnOp::Not,
                        "++" => {
                            if prefix {
                                UnOp::PreIncrement
                            } else {
                                UnOp::PostIncrement
                            }
                        }
                        "--" => {
                            if prefix {
                                UnOp::PreDecrement
                            } else {
                                UnOp::PostDecrement
                            }
                        }
                        "__real" => UnOp::Real,
                        "__imag" => UnOp::Imag,
                        "__extension__" => UnOp::Extension,
                        "co_await" => UnOp::Coawait,
                        o => panic!("Unexpected operator: {}", o),
                    };

                    let operand_old = node.children[0].expect("Expected operand");
                    let operand = self.visit_expr(operand_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let unary = CExprKind::Unary(ty, operator, operand, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, unary);
                }

                ASTEntryTag::TagImplicitCastExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let expression_old =
                        node.children[0].expect("Expected expression for implicit cast");
                    let expression = self.visit_expr(expression_old);

                    let typ_old = node.type_id.expect("Expected type for implicit cast");
                    let typ = self.visit_qualified_type(typ_old);

                    let kind = parse_cast_kind(
                        &from_value::<String>(node.extras[0].clone()).expect("Expected cast kind"),
                    );
                    let implicit =
                        CExprKind::ImplicitCast(typ, expression, kind, None, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, implicit);
                }

                ASTEntryTag::TagCStyleCastExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let expression_old =
                        node.children[0].expect("Expected expression for explicit cast");
                    let expression = self.visit_expr(expression_old);

                    let typ_old = node.type_id.expect("Expected type for explicit cast");
                    let typ = self.visit_qualified_type(typ_old);

                    let kind = parse_cast_kind(
                        &from_value::<String>(node.extras[0].clone()).expect("Expected cast kind"),
                    );

                    let opt_field_id = match kind {
                        CastKind::ToUnion => {
                            let id = node.children[1].expect("Expected field for union cast");
                            Some(self.visit_decl(id))
                        }
                        _ => None,
                    };

                    let implicit =
                        CExprKind::ExplicitCast(typ, expression, kind, opt_field_id, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, implicit);
                }

                ASTEntryTag::TagCallExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let func_old = node.children[0].expect("Expected function for function call");
                    let func = self.visit_expr(func_old);

                    let args: Vec<CExprId> = node
                        .children
                        .iter()
                        .skip(1)
                        .map(|id| {
                            let arg_id = id.expect("Expected call expression argument");
                            self.visit_expr(arg_id)
                        })
                        .collect();

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let call = CExprKind::Call(ty, func, args);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, call);
                }

                ASTEntryTag::TagMemberExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let base_old = node.children[0].expect("Expected base for member expression");
                    let base = self.visit_expr(base_old);

                    let field_old = node.children[1].expect("Expected field for member expression");
                    let field = self.visit_decl(field_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let member_kind = if from_value(node.extras[0].clone()).expect("is arrow") {
                        MemberKind::Arrow
                    } else {
                        MemberKind::Dot
                    };

                    let member = CExprKind::Member(ty, base, field, member_kind, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, member);
                }

                ASTEntryTag::TagBinaryOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let operator = match from_value::<String>(node.extras[0].clone())
                        .expect("Expected operator")
                        .as_str()
                    {
                        "*" => BinOp::Multiply,
                        "/" => BinOp::Divide,
                        "%" => BinOp::Modulus,
                        "+" => BinOp::Add,
                        "-" => BinOp::Subtract,
                        "<<" => BinOp::ShiftLeft,
                        ">>" => BinOp::ShiftRight,
                        "<" => BinOp::Less,
                        ">" => BinOp::Greater,
                        "<=" => BinOp::LessEqual,
                        ">=" => BinOp::GreaterEqual,
                        "==" => BinOp::EqualEqual,
                        "!=" => BinOp::NotEqual,
                        "&" => BinOp::BitAnd,
                        "^" => BinOp::BitXor,
                        "|" => BinOp::BitOr,
                        "&&" => BinOp::And,
                        "||" => BinOp::Or,
                        "+=" => BinOp::AssignAdd,
                        "-=" => BinOp::AssignSubtract,
                        "*=" => BinOp::AssignMultiply,
                        "/=" => BinOp::AssignDivide,
                        "%=" => BinOp::AssignModulus,
                        "^=" => BinOp::AssignBitXor,
                        "<<=" => BinOp::AssignShiftLeft,
                        ">>=" => BinOp::AssignShiftRight,
                        "|=" => BinOp::AssignBitOr,
                        "&=" => BinOp::AssignBitAnd,
                        "=" => BinOp::Assign,
                        "," => BinOp::Comma,
                        _ => unimplemented!(),
                    };

                    let left_operand_old = node.children[0].expect("Expected left operand");
                    let left_operand = self.visit_expr(left_operand_old);

                    let right_operand_old = node.children[1].expect("Expected right operand");
                    let right_operand = self.visit_expr(right_operand_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let opt_lhs_type_id =
                        expect_opt_u64(&node.extras[1]).expect("Expected compute lhs type");
                    let opt_lhs_type = opt_lhs_type_id.map(|x| self.visit_qualified_type(x));

                    let opt_res_type_id =
                        expect_opt_u64(&node.extras[2]).expect("Expected compute lhs type");
                    let opt_res_type = opt_res_type_id.map(|x| self.visit_qualified_type(x));

                    let binary = CExprKind::Binary(
                        ty,
                        operator,
                        left_operand,
                        right_operand,
                        opt_lhs_type,
                        opt_res_type,
                    );

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, binary);
                }

                ASTEntryTag::TagDeclRefExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let declaration_old =
                        node.children[0].expect("Expected declaration on expression tag decl");
                    let declaration = self.visit_decl(declaration_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let decl = CExprKind::DeclRef(ty, declaration, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, decl);
                }

                ASTEntryTag::TagArraySubscriptExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let lhs_old =
                        node.children[0].expect("Expected LHS on array subscript expression");
                    let lhs = self.visit_expr(lhs_old);

                    let rhs_old =
                        node.children[1].expect("Expected RHS on array subscript expression");
                    let rhs = self.visit_expr(rhs_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let subscript = CExprKind::ArraySubscript(ty, lhs, rhs, node.rvalue);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, subscript);
                }

                ASTEntryTag::TagConditionalOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let cond_old = node.children[0].expect("Expected condition on if expression");
                    let cond = self.visit_expr(cond_old);

                    let lhs_old = node.children[1].expect("Expected 'then' on if expression");
                    let lhs = self.visit_expr(lhs_old);

                    let rhs_old = node.children[2].expect("Expected 'else' on if expression");
                    let rhs = self.visit_expr(rhs_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let conditional = CExprKind::Conditional(ty, cond, lhs, rhs);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, conditional);
                }

                ASTEntryTag::TagBinaryConditionalOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let lhs_old = node.children[0].expect("Expected condition on if expression");
                    let lhs = self.visit_expr(lhs_old);

                    let rhs_old = node.children[1].expect("Expected 'else' on if expression");
                    let rhs = self.visit_expr(rhs_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let conditional = CExprKind::BinaryConditional(ty, lhs, rhs);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, conditional);
                }

                ASTEntryTag::TagUnaryExprOrTypeTraitExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let ty = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty);

                    let expr = node.children[0].map(|x| self.visit_expr(x));

                    let kind_name =
                        from_value::<String>(node.extras[0].clone()).expect("expected kind");
                    let kind = match kind_name.as_str() {
                        "sizeof" => UnTypeOp::SizeOf,
                        "alignof" => UnTypeOp::AlignOf,
                        "preferredalignof" => UnTypeOp::PreferredAlignOf,
                        str => panic!("Unsupported operation: {}", str),
                    };

                    let arg_ty = from_value(node.extras[1].clone()).expect("expected type id");
                    let arg_ty = self.visit_qualified_type(arg_ty);

                    let operator = CExprKind::UnaryType(ty, kind, expr, arg_ty);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, operator);
                }

                ASTEntryTag::TagCompoundLiteralExpr => {
                    let ty_old = node
                        .type_id
                        .expect("Expected compound literal to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let val_old = node.children[0].expect("Expected child on compound literal");
                    let val = self.visit_expr(val_old);

                    self.expr_possibly_as_stmt(
                        expected_ty,
                        new_id,
                        node,
                        CExprKind::CompoundLiteral(ty, val),
                    )
                }

                ASTEntryTag::TagPredefinedExpr => {
                    let ty_old = node.type_id.expect("Expected predefined expr to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let val_old = node.children[0].expect("Expected child on predefined expr");
                    let val = self.visit_expr(val_old);

                    self.expr_possibly_as_stmt(
                        expected_ty,
                        new_id,
                        node,
                        CExprKind::Predefined(ty, val),
                    )
                }

                ASTEntryTag::TagImplicitValueInitExpr => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    self.expr_possibly_as_stmt(
                        expected_ty,
                        new_id,
                        node,
                        CExprKind::ImplicitValueInit(ty),
                    )
                }

                ASTEntryTag::TagInitListExpr => {
                    let exprs: Vec<CExprId> = node
                        .children
                        .iter()
                        .map(|id| {
                            let expr_id = id.expect("init expression id");
                            self.visit_expr(expr_id)
                        })
                        .collect();

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let union_field_id = expect_opt_u64(&node.extras[0])
                        .expect("Bad union field ID entry")
                        .map(|x| self.visit_decl(x));
                    let syntax_id = expect_opt_u64(&node.extras[1])
                        .expect("Bad syntax ID entry")
                        .map(|x| self.visit_expr(x));

                    let kind = CExprKind::InitList(ty, exprs, union_field_id, syntax_id);
                    self.expr_possibly_as_stmt(expected_ty, new_id, node, kind)
                }

                ASTEntryTag::TagDesignatedInitExpr => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let designator_cbors = from_value::<Vec<Value>>(node.extras[0].clone())
                        .expect("Expected designators array");
                    let designators = designator_cbors
                        .into_iter()
                        .map(|x| {
                            let entry =
                                from_value::<Vec<Value>>(x).expect("expected designator array");
                            match from_value(entry[0].clone()).expect("expected designator tag") {
                                1 => Designator::Index(
                                    from_value(entry[1].clone()).expect("expected array index"),
                                ),
                                2 => Designator::Field(CDeclId(
                                    from_value(entry[1].clone()).expect("expected field id"),
                                )),
                                3 => Designator::Range(
                                    from_value(entry[1].clone()).expect("expected array start"),
                                    from_value(entry[2].clone()).expect("expected array end"),
                                ),
                                n => panic!("invalid designator tag: {}", n),
                            }
                        })
                        .collect();

                    let init_id = node.children[0]
                        .expect("Expected initializer expression on designated init expr");
                    let init_expr = self.visit_expr(init_id);

                    let kind = CExprKind::DesignatedInitExpr(ty, designators, init_expr);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, kind)
                }

                ASTEntryTag::TagStmtExpr => {
                    let child_id = node.children[0].expect("Expected compound statement ID");
                    let child = self.visit_stmt(child_id);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let stmt_expr = CExprKind::Statements(ty, child);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, stmt_expr)
                }

                ASTEntryTag::TagVAArgExpr => {
                    let child_id = node.children[0].expect("Expected subexpression");
                    let child = self.visit_expr(child_id);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let vaarg_expr = CExprKind::VAArg(ty, child);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, vaarg_expr)
                }

                ASTEntryTag::TagShuffleVectorExpr => {
                    let kids: Vec<CExprId> = node
                        .children
                        .iter()
                        .map(|id| {
                            let child_id = id.expect("Missing shuffle argument");
                            self.visit_expr(child_id)
                        })
                        .collect();

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let e = CExprKind::ShuffleVector(ty, kids);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagConvertVectorExpr => {
                    let kids: Vec<CExprId> = node
                        .children
                        .iter()
                        .map(|id| {
                            let child_id = id.expect("Missing convert argument");
                            self.visit_expr(child_id)
                        })
                        .collect();

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let e = CExprKind::ConvertVector(ty, kids);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagBuiltinBitCastExpr => {
                    let expression_old = node.children[0].expect("Expected expression for bitcast");
                    let expression = self.visit_expr(expression_old);

                    let ty_old = node.type_id.expect("Expected type for bitcast");
                    let ty = self.visit_qualified_type(ty_old);

                    let e = CExprKind::ExplicitCast(
                        ty,
                        expression,
                        CastKind::BitCast,
                        None,
                        node.rvalue,
                    );

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagMaterializeTemporaryExpr => {
                    let expression_old = node.children[0]
                        .expect("Expected expression for materialize-temporary expr");
                    let expression = self.visit_expr(expression_old);

                    let ty_old = node
                        .type_id
                        .expect("Expected type for materialize-temporary expr");
                    let ty = self.visit_qualified_type(ty_old);

                    // C does not use this expression type (it has to do with C++ references), but
                    // it nonetheless shows up in some system SIMD intrinsic headers even when
                    // parsing as C. For now, just treat it as a trivial wrapper, i.e. parentheses.
                    let e = CExprKind::Paren(ty, expression);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagExprWithCleanups => {
                    let expression_old =
                        node.children[0].expect("Expected expression for expr with cleanups");
                    let expression = self.visit_expr(expression_old);

                    let ty_old = node.type_id.expect("Expected type for expr with cleanups");
                    let ty = self.visit_qualified_type(ty_old);

                    // C does not use this expression type, but it nonetheless shows up in some
                    // system SIMD intrinsic headers even when parsing as C. For now, just treat it
                    // as a trivial wrapper, i.e. parentheses.
                    let e = CExprKind::Paren(ty, expression);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagConstantExpr => {
                    let expr = node.children[0].expect("Missing ConstantExpr subexpression");
                    let expr = self.visit_expr(expr);

                    let has_value = from_value(node.extras[0].clone())
                        .expect("Case constant has_value not found");
                    let cie = if has_value {
                        let is_signed = from_value(node.extras[1].clone())
                            .expect("Case constant is_signed not found");
                        Some(match is_signed {
                            false => ConstIntExpr::U(
                                from_value(node.extras[2].clone())
                                    .expect("Case constant not found"),
                            ),
                            true => ConstIntExpr::I(
                                from_value(node.extras[2].clone())
                                    .expect("Case constant not found"),
                            ),
                        })
                    } else {
                        None
                    };

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let e = CExprKind::ConstantExpr(ty, expr, cie);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagChooseExpr => {
                    let condition = node.children[0].expect("ChooseExpr condition not found");
                    let condition = self.visit_expr(condition);

                    let true_expr = node.children[1].expect("ChooseExpr true expression not found");
                    let true_expr = self.visit_expr(true_expr);

                    let false_expr =
                        node.children[2].expect("ChooseExpr false expression not found");
                    let false_expr = self.visit_expr(false_expr);

                    let ty = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty);

                    let condition_is_true =
                        from_value(node.extras[0].clone()).expect("Expected evaluated condition");

                    let e =
                        CExprKind::Choose(ty, condition, true_expr, false_expr, condition_is_true);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                ASTEntryTag::TagAtomicExpr => {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Expected to find builtin operator name");

                    // The order of children is defined by Clang in class
                    // AtomicExpr
                    let mut children = node.children.iter();
                    let ptr = self.visit_expr(
                        children
                            .next()
                            .unwrap()
                            .expect("Atomic must have a ptr argument"),
                    );
                    let order = self.visit_expr(
                        children
                            .next()
                            .unwrap()
                            .expect("Atomic must have an order argument"),
                    );
                    let val1 = children.next().map(|e| self.visit_expr(e.unwrap()));
                    let order_fail = children.next().map(|e| self.visit_expr(e.unwrap()));
                    let val2 = children.next().map(|e| self.visit_expr(e.unwrap()));
                    let weak = children.next().map(|e| self.visit_expr(e.unwrap()));

                    let typ = node.type_id.expect("Expected expression to have type");
                    let typ = self.visit_qualified_type(typ);

                    // Perhaps as an optimization since atomic_init has no order,
                    // clang stores val1 in the position otherwise used for order
                    let is_atomic = name == "__c11_atomic_init" || name == "__opencl_atomic_init";
                    let val1 = if is_atomic { Some(order) } else { val1 };

                    let e = CExprKind::Atomic {
                        typ,
                        name,
                        ptr,
                        order,
                        val1,
                        order_fail,
                        val2,
                        weak,
                    };

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, e)
                }

                // Declarations
                ASTEntryTag::TagFunctionDecl if expected_ty & OTHER_DECL != 0 => {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Expected to find function name");

                    let is_global =
                        from_value(node.extras[1].clone()).expect("Expected to find visibility");
                    let mut is_inline =
                        from_value(node.extras[2].clone()).expect("Expected to find inline");

                    let is_main =
                        from_value(node.extras[3].clone()).expect("Expected to find main");
                    if is_main {
                        self.typed_context.c_main = Some(CDeclId(new_id));
                    }

                    let is_implicit =
                        from_value(node.extras[4].clone()).expect("Expected to find implicit");
                    let is_extern =
                        from_value(node.extras[5].clone()).expect("Expected to find externness");
                    let is_inline_externally_visible = from_value(node.extras[6].clone())
                        .expect("Expected to find inline visibliity");
                    let attributes = from_value::<Vec<Value>>(node.extras[7].clone())
                        .expect("Expected to find attributes");
                    let attrs = parse_attributes(attributes);

                    // The always_inline attribute implies inline even if the
                    // inline keyword is not present.
                    is_inline |= attrs.contains(&Attribute::AlwaysInline);

                    let typ_old = node
                        .type_id
                        .expect("Expected to find a type on a function decl");
                    let typ = CTypeId(self.visit_node_type(typ_old, TYPE));

                    let (body_id, parameter_ids) = node
                        .children
                        .split_last()
                        .expect("Expected to find a function body");

                    let body = body_id.map(|b| self.visit_stmt(b));

                    let parameters = parameter_ids
                        .iter()
                        .map(|id| {
                            let param = id.expect("Param field decl not found");
                            CDeclId(self.visit_node_type(param, VAR_DECL))
                        })
                        .collect();

                    let function_decl = CDeclKind::Function {
                        attrs,
                        body,
                        is_extern,
                        is_global,
                        is_implicit,
                        is_inline,
                        is_inline_externally_visible,
                        name,
                        parameters,
                        typ,
                    };

                    self.add_decl(new_id, located(node, function_decl));
                    self.processed_nodes.insert(new_id, OTHER_DECL);
                }

                ASTEntryTag::TagTypedefDecl if expected_ty & TYPDEF_DECL != 0 => {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Expected to find typedef name");
                    let is_implicit =
                        from_value(node.extras[1].clone()).expect("Expected to find implicit");

                    let typ_old = node
                        .type_id
                        .expect("Expected to find type on typedef declaration");
                    let mut typ = self.visit_qualified_type(typ_old);

                    // Clang injects definitions of the form `#define __SIZE_TYPE__ unsigned int` into
                    // compilation units based on the target. (See lib/Frontend/InitPreprocessor.cpp
                    // in Clang).
                    //
                    // Later, headers contain defns like: `typedef __SIZE_TYPE__ size_t;`
                    //
                    // We detect these typedefs and alter them replacing the type to which the macro
                    // expands with a synthetic, portable `CTypeKind` chosen from the macro's name.
                    //
                    // This allows us to generate platform-independent Rust types like u64 or usize
                    // despite the C side internally working with a target-specific type.
                    let target_dependent_macro: Option<String> = from_value(node.extras[2].clone())
                        .expect("Expected to find optional target-dependent macro name");

                    typ = target_dependent_macro
                        .as_deref()
                        .and_then(|macro_name| {
                            let kind = match macro_name {
                                // Match names in the order Clang defines them.
                                "__INTMAX_TYPE__" => CTypeKind::IntMax,
                                "__UINTMAX_TYPE__" => CTypeKind::UIntMax,
                                "__PTRDIFF_TYPE__" => CTypeKind::PtrDiff,
                                "__INTPTR_TYPE__" => CTypeKind::IntPtr,
                                "__SIZE_TYPE__" => CTypeKind::Size,
                                "__WCHAR_TYPE__" => CTypeKind::WChar,
                                // __WINT_TYPE__ is defined by Clang but has no obvious translation
                                // __CHARn_TYPE__ for n ∈ {8, 16, 32} also lack obvious translation
                                "__UINTPTR_TYPE__" => CTypeKind::UIntPtr,
                                _ => {
                                    log::debug!("Unknown target-dependent macro {macro_name}!");
                                    return None;
                                }
                            };
                            log::trace!("Selected kind {kind} for typedef {name}");
                            Some(CQualTypeId::new(
                                self.typed_context.type_for_kind(&kind).unwrap(),
                            ))
                        })
                        .unwrap_or(typ);

                    // Other fixed-size types are defined without special compiler involvement, by
                    // standard headers and their transitive includes. In these contexts, we
                    // recognize these typedefs by the name of the typedef type.
                    let id_for_name = |name| -> Option<_> {
                        let kind = match name {
                            "intmax_t" => CTypeKind::IntMax,
                            "uintmax_t" => CTypeKind::UIntMax,
                            "intptr_t" => CTypeKind::IntPtr,
                            "uintptr_t" => CTypeKind::UIntPtr,
                            // unlike `size_t`, `ssize_t` does not have a clang-provided `#define`.
                            "ssize_t" => CTypeKind::SSize,
                            "__uint8_t" => CTypeKind::UInt8,
                            "__uint16_t" => CTypeKind::UInt16,
                            "__uint32_t" => CTypeKind::UInt32,
                            "__uint64_t" => CTypeKind::UInt64,
                            "__uint128_t" => CTypeKind::UInt128,
                            "__int8_t" => CTypeKind::Int8,
                            "__int16_t" => CTypeKind::Int16,
                            "__int32_t" => CTypeKind::Int32,
                            "__int64_t" => CTypeKind::Int64,
                            "__int128_t" => CTypeKind::Int128,
                            _ => {
                                log::debug!("Unknown fixed-size type typedef {name}!");
                                return None;
                            }
                        };
                        log::trace!("Selected kind {kind} for typedef {name}");
                        Some(CQualTypeId::new(
                            self.typed_context.type_for_kind(&kind).unwrap(),
                        ))
                    };
                    let file = self
                        .typed_context
                        .files
                        .get(self.typed_context.file_map[node.loc.fileid as usize]);
                    let file = file.unwrap();
                    if let Some(path) = file.path.as_ref() {
                        if let Some(filename) = path.file_name() {
                            if filename == "stdint.h"
                                || filename == "types.h"
                                || filename
                                    .to_str()
                                    .map(|s| s.starts_with("__stddef_"))
                                    .unwrap_or(false)
                            // for macos
                                || filename == "_types.h"
                                || filename
                                    .to_str()
                                    .map(|s| s.starts_with("_int") || s.starts_with("_uint"))
                                    .unwrap_or(false)
                            {
                                typ = id_for_name(&*name).unwrap_or(typ);
                            }
                        }
                    }

                    let typdef_decl = CDeclKind::Typedef {
                        name,
                        typ,
                        is_implicit,
                        target_dependent_macro,
                    };

                    self.add_decl(new_id, located(node, typdef_decl));
                    self.processed_nodes.insert(new_id, TYPDEF_DECL);
                }

                ASTEntryTag::TagEnumDecl if expected_ty & ENUM_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);

                    let variants = node
                        .children
                        .iter()
                        .map(|id| {
                            let con = id.expect("Enum constant not found");
                            let id = CDeclId(self.visit_node_type(con, ENUM_CON));
                            self.typed_context.parents.insert(id, CDeclId(new_id));
                            id
                        })
                        .collect();

                    let integral_type = node.type_id.map(|x| self.visit_qualified_type(x));

                    let enum_decl = CDeclKind::Enum {
                        name,
                        variants,
                        integral_type,
                    };

                    self.add_decl(new_id, located(node, enum_decl));
                    self.processed_nodes.insert(new_id, ENUM_DECL);
                }

                ASTEntryTag::TagEnumConstantDecl if expected_ty & ENUM_CON != 0 => {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Expected to find enum constant name");
                    let is_signed = from_value(node.extras[1].clone())
                        .expect("Enum constant signedness not found");
                    let value = match is_signed {
                        false => ConstIntExpr::U(
                            from_value(node.extras[2].clone()).expect("Enum constant not found"),
                        ),
                        true => ConstIntExpr::I(
                            from_value(node.extras[2].clone()).expect("Enum constant not found"),
                        ),
                    };

                    let enum_constant_decl = CDeclKind::EnumConstant { name, value };

                    self.add_decl(new_id, located(node, enum_constant_decl));
                    self.processed_nodes.insert(new_id, ENUM_CON);
                }

                ASTEntryTag::TagVarDecl if expected_ty & VAR_DECL != 0 => {
                    let ident = from_value::<String>(node.extras[0].clone())
                        .expect("Expected to find variable name");

                    let has_static_duration = from_value(node.extras[1].clone())
                        .expect("Expected to find static duration");
                    let has_thread_duration = from_value(node.extras[2].clone())
                        .expect("Expected to find thread duration");
                    let is_externally_visible = from_value::<bool>(node.extras[3].clone())
                        .expect("Expected to find visibility");
                    let is_defn = from_value(node.extras[4].clone())
                        .expect("Expected to find whether decl is definition");
                    let attributes = from_value::<Vec<Value>>(node.extras[5].clone())
                        .expect("Expected attribute array on var decl");

                    assert!(
                        has_static_duration || has_thread_duration || !is_externally_visible,
                        "Variable cannot be extern without also being static or thread-local: {}",
                        ident
                    );

                    let initializer = node
                        .children
                        .get(0)
                        .into_iter()
                        .flatten()
                        .map(|id| self.visit_expr(*id))
                        .next();

                    let typ_id = node
                        .type_id
                        .expect("Expected to find type on variable declaration");
                    let typ = self.visit_qualified_type(typ_id);

                    let attrs = parse_attributes(attributes);

                    let variable_decl = CDeclKind::Variable {
                        has_static_duration,
                        has_thread_duration,
                        is_externally_visible,
                        is_defn,
                        ident,
                        initializer,
                        typ,
                        attrs,
                    };

                    self.add_decl(new_id, located(node, variable_decl));
                    self.processed_nodes.insert(new_id, VAR_DECL);
                }

                ASTEntryTag::TagStructDecl if expected_ty & RECORD_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);
                    let has_def = from_value(node.extras[1].clone())
                        .expect("Expected has_def flag on struct");
                    let attrs = from_value::<Vec<Value>>(node.extras[2].clone())
                        .expect("Expected attribute array on record");
                    let manual_alignment =
                        expect_opt_u64(&node.extras[3]).expect("Expected struct alignment");
                    let max_field_alignment =
                        expect_opt_u64(&node.extras[4]).expect("Expected struct field align");
                    let platform_byte_size =
                        from_value(node.extras[5].clone()).expect("Expected struct size");
                    let platform_alignment =
                        from_value(node.extras[6].clone()).expect("Expected struct alignment");

                    let fields: Option<Vec<CDeclId>> = if has_def {
                        Some(
                            node.children
                                .iter()
                                .map(|id| {
                                    let field = id.expect("Record field decl not found");
                                    let id = CDeclId(self.visit_node_type(field, FIELD_DECL));
                                    self.typed_context.parents.insert(id, CDeclId(new_id));
                                    id
                                })
                                .collect(),
                        )
                    } else {
                        None
                    };

                    let is_packed = has_packed_attribute(attrs);

                    let record = CDeclKind::Struct {
                        name,
                        fields,
                        is_packed,
                        manual_alignment,
                        max_field_alignment,
                        platform_byte_size,
                        platform_alignment,
                    };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, RECORD_DECL);
                }

                ASTEntryTag::TagUnionDecl if expected_ty & RECORD_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);
                    let has_def = from_value(node.extras[1].clone())
                        .expect("Expected has_def flag on struct");
                    let attrs = from_value::<Vec<Value>>(node.extras[2].clone())
                        .expect("Expected attribute array on record");
                    let fields: Option<Vec<CDeclId>> = if has_def {
                        Some(
                            node.children
                                .iter()
                                .map(|id| {
                                    let field = id.expect("Record field decl not found");
                                    let id = CDeclId(self.visit_node_type(field, FIELD_DECL));
                                    self.typed_context.parents.insert(id, CDeclId(new_id));
                                    id
                                })
                                .collect(),
                        )
                    } else {
                        None
                    };

                    let is_packed = has_packed_attribute(attrs);

                    let record = CDeclKind::Union {
                        name,
                        fields,
                        is_packed,
                    };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, RECORD_DECL);
                }

                ASTEntryTag::TagFieldDecl if expected_ty & FIELD_DECL != 0 => {
                    let name =
                        from_value::<String>(node.extras[0].clone()).expect("A field needs a name");
                    let typ_id = node
                        .type_id
                        .expect("Expected to find type on field declaration");
                    let typ = self.visit_qualified_type(typ_id);
                    let bitfield_width = from_value(node.extras[1].clone()).ok();
                    let platform_bit_offset =
                        from_value(node.extras[2].clone()).expect("Did not find field bit offset");
                    let platform_type_bitwidth =
                        from_value(node.extras[3].clone()).expect("Did not find field bitwidth");
                    let field = CDeclKind::Field {
                        name,
                        typ,
                        bitfield_width,
                        platform_bit_offset,
                        platform_type_bitwidth,
                    };
                    self.add_decl(new_id, located(node, field));
                    self.processed_nodes.insert(new_id, FIELD_DECL);
                }

                ASTEntryTag::TagMacroObjectDef | ASTEntryTag::TagMacroFunctionDef
                    if expected_ty & MACRO_DECL != 0 =>
                {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Macros must have a name");

                    let mac_object = match node.tag {
                        ASTEntryTag::TagMacroObjectDef => CDeclKind::MacroObject { name },
                        ASTEntryTag::TagMacroFunctionDef => CDeclKind::MacroFunction { name },
                        _ => unreachable!("Unexpected tag for macro"),
                    };

                    self.add_decl(new_id, located(node, mac_object));
                    self.processed_nodes.insert(new_id, MACRO_DECL);

                    // Macros aren't technically top-level decls, so clang
                    // doesn't put them in top_nodes, but we do need to process
                    // them early.
                    self.typed_context.c_decls_top.push(CDeclId(new_id));
                }

                ASTEntryTag::TagMacroFunctionDef if expected_ty & MACRO_DECL != 0 => {
                    let name = from_value::<String>(node.extras[0].clone())
                        .expect("Macros must have a name");

                    let mac_object = CDeclKind::MacroFunction { name };
                    self.add_decl(new_id, located(node, mac_object));
                    self.processed_nodes.insert(new_id, MACRO_DECL);

                    // Macros aren't technically top-level decls, so clang
                    // doesn't put them in top_nodes, but we do need to process
                    // them early.
                    self.typed_context.c_decls_top.push(CDeclId(new_id));
                }

                ASTEntryTag::TagNonCanonicalDecl if expected_ty & DECL != 0 => {
                    let canonical_decl =
                        node.children[0].expect("NonCanonicalDecl must point to a canonical decl");
                    let canonical_decl = self.visit_decl(canonical_decl);
                    let record = CDeclKind::NonCanonicalDecl { canonical_decl };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, OTHER_DECL);

                    // Look up the canonical declaration and see if it declares a
                    // struct. If so, check attributes of the non-canonical declaration
                    // and potentially update its `is_packed` property.
                    if let Some(v) = self.typed_context.c_decls.get_mut(&canonical_decl) {
                        match &mut v.kind {
                            CDeclKind::Struct { is_packed, .. }
                            | CDeclKind::Union { is_packed, .. } => {
                                let attrs = from_value::<Vec<Value>>(node.extras[0].clone())
                                    .expect(
                                        "Expected attribute array on non-canonical record decl",
                                    );

                                *is_packed = has_packed_attribute(attrs);
                            }
                            _ => {}
                        }
                    }
                }

                ASTEntryTag::TagStaticAssertDecl if expected_ty & DECL != 0 => {
                    let assert_expr = CExprId(
                        node.children[0].expect("StaticAssert must point to an expression"),
                    );
                    let message = if node.children.len() > 1 {
                        Some(CExprId(
                            node.children[1].expect("Expected static assert message"),
                        ))
                    } else {
                        None
                    };
                    let static_assert = CDeclKind::StaticAssert {
                        assert_expr,
                        message,
                    };
                    self.add_decl(new_id, located(node, static_assert));
                }

                t => panic!("Could not translate node {:?} as type {}", t, expected_ty),
            }
        }
    }
}
