use std::collections::HashMap;
use std::vec::Vec;
use c_ast::*;
use clang_ast::*;


/// Possible node types
pub type NodeType = u16;

mod node_types {
    pub const FUNC_TYPE  : super::NodeType = 0b000000000001;
    pub const OTHER_TYPE : super::NodeType = 0b000000000010;
    pub const TYPE       : super::NodeType = FUNC_TYPE | OTHER_TYPE;

    pub const EXPR       : super::NodeType = 0b000000000100;

    pub const FIELD_DECL : super::NodeType = 0b000000001000;
    pub const VAR_DECL   : super::NodeType = 0b000000010000;
    pub const RECORD_DECL: super::NodeType = 0b000000100000;
    pub const TYPDEF_DECL: super::NodeType = 0b000001000000;
    pub const ENUM_DECL  : super::NodeType = 0b000010000000;
    pub const ENUM_CON   : super::NodeType = 0b000100000000;
    pub const OTHER_DECL : super::NodeType = 0b001000000000;
    pub const DECL       : super::NodeType = FIELD_DECL | VAR_DECL | RECORD_DECL | TYPDEF_DECL
                                           | ENUM_DECL | ENUM_CON | OTHER_DECL;

    pub const LABEL_STMT : super::NodeType = 0b010000000000;
    pub const OTHER_STMT : super::NodeType = 0b100000000000;
    pub const STMT       : super::NodeType = LABEL_STMT | OTHER_STMT;
}

type ClangId = u64;
type NewId = u64;

/// Correspondence between old/new IDs.
///
/// We need to re-ID nodes since the mapping from Clang's AST to ours is not one-to-one. Sometimes
/// we need to add nodes (such as 'Semi' nodes to make the lifting of expressions into statements
/// explicit), sometimes we need to collapse (such as inlining 'FieldDecl' into the 'StructDecl').
#[derive(Debug)]
pub struct IdMapper {
    new_id_source: NewId,
    old_to_new: HashMap<ClangId, NewId>,
    new_to_old: HashMap<NewId, ClangId>,
}

impl IdMapper {
    pub fn new() -> IdMapper {
        IdMapper {
            new_id_source: 0,
            old_to_new: HashMap::new(),
            new_to_old: HashMap::new(),
        }
    }

    /// Create a fresh NEW_ID not corresponding to a CLANG_ID
    fn fresh_id(&mut self) -> NewId {
        self.new_id_source += 1;
        self.new_id_source
    }

    /// Lookup the NEW_ID corresponding to a CLANG_ID
    pub fn get_new(&mut self, old_id: ClangId) -> Option<NewId> {
        self.old_to_new.get(&old_id).map(|o| *o)
    }

    /// Lookup (or create if not a found) a NEW_ID corresponding to a CLANG_ID
    pub fn get_or_create_new(&mut self, old_id: ClangId) -> NewId {
        match self.get_new(old_id) {
            Some(new_id) => new_id,
            None => {
                let new_id = self.fresh_id();
                let inserted = self.old_to_new.insert(old_id, new_id).is_some();
                assert!(!inserted, "get_or_create_new: overwrote an old id at {}", old_id);
                new_id
            }
        }
    }

    /// Lookup the CLANG_ID corresponding to a NEW_ID
    pub fn get_old(&mut self, new_id: NewId) -> Option<ClangId> {
        self.new_to_old.get(&new_id).map(|n| *n)
    }

    /// If the `old_id` is present in the mapper, make `other_old_id` map to the same value. Note
    /// that `other_old_id` should not already be in the mapper.
    pub fn merge_old(&mut self, old_id: ClangId, other_old_id: ClangId) -> Option<NewId> {
        self.get_new(old_id)
            .map(|new_id| {
                let inserted = self.old_to_new.insert(other_old_id, new_id).is_some();
                assert!(!inserted, "get_or_create_new: overwrote an old id at {}", other_old_id);
                new_id
            })
    }
}

/// Transfer location information off of an `AstNode` and onto something that is `Located`
fn located<T>(node: &AstNode, t: T) -> Located<T> {
    Located {
        loc: Some(SrcLoc { line: node.line, column: node.column, fileid: node.fileid }),
        kind: t
    }
}

/// Wrap something into a `Located` node without any location information
fn not_located<T>(t: T) -> Located<T> {
    Located {
        loc: None,
        kind: t
    }
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
        k => panic!("Unsupported implicit cast: {}", k),
    }
}

/// This stores the information needed to convert an `AstContext` into a `TypedAstContext`.
pub struct ConversionContext {

    /// Keeps track of the mapping between old and new IDs
    pub id_mapper: IdMapper,

    /// Keep track of new nodes already processed and their types
    processed_nodes: HashMap<NewId, NodeType>,

    /// Stack of nodes to visit, and the types we expect to see out of them
    visit_as: Vec<(ClangId, NodeType)>,

    /// Typed context we are building up during the conversion
    pub typed_context: TypedAstContext,
}

impl ConversionContext {

    /// Create a new 'ConversionContext' seeded with top-level nodes from an 'AstContext'.
    pub fn new(untyped_context: &AstContext) -> ConversionContext {
        // This starts out as all of the top-level nodes, which we expect to be 'DECL's
        let mut visit_as: Vec<(ClangId, NodeType)> = Vec::new();
        for top_node in untyped_context.top_nodes.iter().rev() {
            if untyped_context.ast_nodes.contains_key(&top_node) {
                visit_as.push((*top_node, node_types::DECL));
            }
        }

        ConversionContext {
            id_mapper: IdMapper::new(),
            processed_nodes: HashMap::new(),
            visit_as,
            typed_context: TypedAstContext::new(),
        }
    }

    /// Records the fact that we will need to visit a Clang node and the type we want it to have.
    ///
    /// Returns the new ID that identifies this new node.
    fn visit_node_type(&mut self, node_id: ClangId, node_ty: NodeType) -> NewId {

        // Type node IDs have extract information on them
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
    fn add_type(&mut self, id: NewId, typ: CType) -> () {
        self.typed_context.c_types.insert(CTypeId(id), typ);
    }

    /// Add a `CStmt` node into the `TypedAstContext`
    fn add_stmt(&mut self, id: NewId, stmt: CStmt) -> () {
        self.typed_context.c_stmts.insert(CStmtId(id), stmt);
    }

    /// Add a `CExpr` node into the `TypedAstContext`
    fn add_expr(&mut self, id: NewId, expr: CExpr) -> () {
        self.typed_context.c_exprs.insert(CExprId(id), expr);
    }

    /// Add a `CDecl` node into the `TypedAstContext`
    fn add_decl(&mut self, id: NewId, decl: CDecl) -> () {
        self.typed_context.c_decls.insert(CDeclId(id), decl);
    }

    /// Clang has `Expression <: Statement`, but we want to make that explicit via the
    /// `CStmtKind::Expr` statement constructor. This function automatically converts expressions
    /// into statements depending on the expected type argument.
    fn expr_possibly_as_stmt(
        &mut self,
        expected_ty: NodeType, // Should be one of `EXPR` or `STMT`
        new_id: NewId,
        node: &AstNode,
        expr: CExprKind,
    ) -> () {
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
    pub fn convert(&mut self, untyped_context: &AstContext) -> () {

        // Continue popping Clang nodes off of the stack of nodes we have promised to visit
        while let Some((node_id, expected_ty)) = self.visit_as.pop() {

            // Check if we've already processed this node. If so, ascertain that it has the right
            // type.
            if let Some(ty) = self.id_mapper.get_new(node_id).and_then(|new_id| self.processed_nodes.get(&new_id)) {
                if ty & expected_ty != 0 {
                    continue;
                }
                panic!("Expected {} to be a node of type {}, not {}", &node_id, expected_ty, ty);
            }

            // Create a `NewId` for this node
            let new_id = self.id_mapper.get_or_create_new(node_id);

            // If the node is top-level, add it as such to the new context
            if untyped_context.top_nodes.contains(&node_id) {
                self.typed_context.c_decls_top.push(CDeclId(new_id));
            }

            self.visit_node(untyped_context, node_id, new_id, expected_ty)
        }
    }


    /// Visit one node.
    fn visit_node(
        &mut self,
        untyped_context: &AstContext,
        node_id: ClangId,                 // Clang ID of node to visit
        new_id: NewId,                    // New ID of node to visit
        expected_ty: NodeType             // Expected type of node to visit
    ) -> () {
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

                TypeTag::TagInt128 if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::Int128));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUInt128 if expected_ty & OTHER_TYPE != 0 => {
                    self.add_type(new_id, not_located(CTypeKind::UInt128));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagPointer if expected_ty & OTHER_TYPE != 0 => {
                    let pointed = expect_u64(&ty_node.extras[0])
                        .expect("Pointer child not found");
                    let pointed_new = self.visit_qualified_type( pointed);

                    let pointer_ty = CTypeKind::Pointer(pointed_new);
                    self.add_type(new_id, not_located(pointer_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBlockPointer if expected_ty & OTHER_TYPE != 0 => {
                    let pointed = expect_u64(&ty_node.extras[0])
                        .expect("Block pointer child not found");
                    let pointed_new = self.visit_qualified_type( pointed);

                    let pointer_ty = CTypeKind::BlockPointer(pointed_new);
                    self.add_type(new_id, not_located(pointer_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagStructType if expected_ty & OTHER_TYPE != 0 => {
                    let decl = expect_u64(&ty_node.extras[0])
                        .expect("Struct decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, RECORD_DECL));

                    let record_ty = CTypeKind::Struct(decl_new);
                    self.add_type(new_id, not_located(record_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagUnionType if expected_ty & OTHER_TYPE != 0 => {
                    let decl = expect_u64(&ty_node.extras[0])
                        .expect("Union decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, RECORD_DECL));

                    let record_ty = CTypeKind::Union(decl_new);
                    self.add_type(new_id, not_located(record_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagFunctionType if expected_ty & FUNC_TYPE != 0 => {
                    let mut arguments: Vec<CQualTypeId> = expect_array(&ty_node.extras[0])
                        .expect("Function type expects array argument")
                        .iter()
                        .map(|cbor| {
                            let arg = expect_u64(cbor).expect("Bad function type child id");
                            let arg_new = self.visit_qualified_type(arg);

                            arg_new
                        })
                        .collect();
                    let ret = arguments.remove(0);
                    let function_ty = CTypeKind::Function(ret, arguments);
                    self.add_type(new_id, not_located(function_ty));
                    self.processed_nodes.insert(new_id, FUNC_TYPE);
                }

                TypeTag::TagTypeOfType if expected_ty & OTHER_TYPE != 0 => {
                    let type_of_old = expect_u64(&ty_node.extras[0]).expect("Type of (type) child not found");
                    let type_of = self.visit_type(type_of_old);

                    let type_of_ty = CTypeKind::TypeOf(type_of);
                    self.add_type(new_id, not_located(type_of_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagTypedefType => {
                    let decl = expect_u64(&ty_node.extras[0])
                        .expect("Typedef decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, TYPDEF_DECL));

                    let typedef_ty = CTypeKind::Typedef(decl_new);
                    self.add_type(new_id, not_located(typedef_ty));
                    self.processed_nodes.insert(new_id, expected_ty);
                }

                TypeTag::TagEnumType if expected_ty & OTHER_TYPE != 0 => {
                    let decl = expect_u64(&ty_node.extras[0])
                        .expect("Enum decl not found");
                    let decl_new = CDeclId(self.visit_node_type(decl, ENUM_DECL));

                    let enum_ty = CTypeKind::Enum(decl_new);
                    self.add_type(new_id, not_located(enum_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagDecayedType if expected_ty & OTHER_TYPE != 0 => {
                    let decayed_id = expect_u64(&ty_node.extras[0]).expect("Decayed type child not found");
                    let decayed = self.visit_type(decayed_id);

                    let decayed_ty = CTypeKind::Decayed(decayed);
                    self.add_type(new_id, not_located(decayed_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagElaboratedType if expected_ty & OTHER_TYPE != 0 => {
                    let elaborated_id = expect_u64(&ty_node.extras[0]).expect("Elaborated type child not found");
                    let elaborated = self.visit_type(elaborated_id);

                    let elaborated_ty = CTypeKind::Elaborated(elaborated);
                    self.add_type(new_id, not_located(elaborated_ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagParenType => {
                    let paren_id = expect_u64(&ty_node.extras[0]).expect("Paren type child not found");
                    let paren = self.visit_type(paren_id);

                    let paren_ty = CTypeKind::Paren(paren);
                    self.add_type(new_id, not_located(paren_ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagAttributedType => {
                    let ty_id = expect_u64(&ty_node.extras[0]).expect("Attributed type child not found");
                    let ty = self.visit_qualified_type(ty_id);
                    let ty = CTypeKind::Attributed(ty);
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, TYPE);
                }

                TypeTag::TagConstantArrayType => {
                    let element_id = expect_u64(&ty_node.extras[0]).expect("element id");
                    let element = self.visit_type(element_id);

                    let count = expect_u64(&ty_node.extras[1]).expect("count");

                    let element_ty = CTypeKind::ConstantArray(element, count as usize);
                    self.add_type(new_id, not_located(element_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagIncompleteArrayType => {
                    let element_id = expect_u64(&ty_node.extras[0]).expect("element id");
                    let element = self.visit_type(element_id);

                    let element_ty = CTypeKind::IncompleteArray(element);
                    self.add_type(new_id, not_located(element_ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                TypeTag::TagBuiltinFn => {
                    let ty = CTypeKind::BuiltinFn;
                    self.add_type(new_id, not_located(ty));
                    self.processed_nodes.insert(new_id, OTHER_TYPE);
                }

                t => panic!("Type conversion not implemented for {:?} expecting {:?}", t, expected_ty),
            }

        } else {
            // Convert the node
            let node: &AstNode = match untyped_context.ast_nodes.get(&node_id) {
                Some(x) => x,
                None => return,
            };


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
                    let constituent_stmts: Vec<CStmtId> = node.children
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
                    let decls = node.children
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
                    let return_expr_opt = node.children[0]
                        .map(|id| self.visit_expr(id));

                    let return_stmt = CStmtKind::Return(return_expr_opt);

                    self.add_stmt(new_id, located(node, return_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagIfStmt if expected_ty & OTHER_STMT != 0 => {
                    let scrutinee_old = node.children[0].expect("If condition expression not found");
                    let scrutinee = self.visit_expr(scrutinee_old);

                    let true_variant_old = node.children[1].expect("If then body statement not found");
                    let true_variant = self.visit_stmt(true_variant_old);

                    let false_variant = node.children[2]
                        .map(|id| self.visit_stmt(id));

                    let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };

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

                ASTEntryTag::TagForStmt if expected_ty & OTHER_STMT != 0 => {
                    let init = node.children[0].map(|id| self.visit_stmt(id));

                    let condition = node.children[1].map(|id| self.visit_expr(id));

                    let increment = node.children[2].map(|id| self.visit_expr(id));

                    let body_old = node.children[3].expect("For loop body not found");
                    let body = self.visit_stmt(body_old);

                    let for_stmt = CStmtKind::ForLoop { init, condition, increment, body };

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

                    let cie =
                        expect_u64(&node.extras[0])
                            .map(ConstIntExpr::U)
                            .unwrap_or_else(|_|
                                expect_i64(&node.extras[0])
                                    .map(ConstIntExpr::I).expect("Expected constant int expr")
                        );


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

                // Expressions

                ASTEntryTag::TagParenExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let wrapped = node.children[0].expect("Expected wrapped paren expression");

                    self.id_mapper.merge_old(node_id, wrapped);
                    self.visit_node_type(wrapped, expected_ty);
                }

                ASTEntryTag::TagIntegerLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_u64(&node.extras[0]).expect("Expected integer literal value");

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let integer_literal = CExprKind::Literal(ty, CLiteral::Integer(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, integer_literal);
                }

                ASTEntryTag::TagStringLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);
                    let width = expect_u64(&node.extras[1]).expect("string literal char width") as u8;
                    let bytes = expect_vec8(&node.extras[2]).expect("string literal bytes");
                    let string_literal = CExprKind::Literal(ty, CLiteral::String(bytes.to_owned(), width));
                    self.expr_possibly_as_stmt(expected_ty, new_id, node, string_literal);
                }

                ASTEntryTag::TagCharacterLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_u64(&node.extras[0]).expect("Expected character literal value");

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let character_literal = CExprKind::Literal(ty, CLiteral::Character(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, character_literal);
                }

                ASTEntryTag::TagFloatingLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_f64(&node.extras[0]).expect("Expected float literal value");

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let floating_literal = CExprKind::Literal(ty, CLiteral::Floating(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, floating_literal);
                }

                ASTEntryTag::TagUnaryOperator if expected_ty & (EXPR | STMT) != 0 => {

                    let prefix = expect_bool(&node.extras[1]).expect("Expected prefix information");

                    let operator = match expect_str(&node.extras[0]).expect("Expected operator") {
                        "&" => UnOp::AddressOf,
                        "*" => UnOp::Deref,
                        "+" => UnOp::Plus,
                        "-" => UnOp::Negate,
                        "~" => UnOp::Complement,
                        "!" => UnOp::Not,
                        "++" => if prefix { UnOp::PreIncrement } else { UnOp::PostIncrement },
                        "--" => if prefix { UnOp::PreDecrement } else { UnOp::PostDecrement },
                        o => panic!("Unexpected operator: {}", o),
                    };

                    let operand_old = node.children[0].expect("Expected operand");
                    let operand = self.visit_expr(operand_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);


                    let unary = CExprKind::Unary(ty, operator, operand);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, unary);
                }

                ASTEntryTag::TagImplicitCastExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let expression_old = node.children[0].expect("Expected expression for implicit cast");
                    let expression = self.visit_expr(expression_old);

                    let typ_old = node.type_id.expect("Expected type for implicit cast");
                    let typ = self.visit_qualified_type(typ_old);


                    let kind = parse_cast_kind(expect_str(&node.extras[0]).expect("Expected cast kind"));
                    let implicit = CExprKind::ImplicitCast(typ, expression, kind, None);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, implicit);
                }

                ASTEntryTag::TagCStyleCastExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let expression_old = node.children[0].expect("Expected expression for explicit cast");
                    let expression = self.visit_expr(expression_old);

                    let typ_old = node.type_id.expect("Expected type for explicit cast");
                    let typ = self.visit_qualified_type(typ_old);


                    let kind = parse_cast_kind(expect_str(&node.extras[0]).expect("Expected cast kind"));

                    let opt_field_id = match kind {
                        CastKind::ToUnion => {
                            let id = node.children[1].expect("Expected field for union cast");
                            Some(self.visit_decl(id))
                        }
                        _ => None,
                    };

                    let implicit = CExprKind::ExplicitCast(typ, expression, kind, opt_field_id);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, implicit);
                }

                ASTEntryTag::TagCallExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let func_old = node.children[0].expect("Expected function for function call");
                    let func = self.visit_expr(func_old);

                    let args: Vec<CExprId> = node.children
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

                    let member_kind =
                        if expect_bool(&node.extras[0]).expect("is arrow")
                            { MemberKind::Arrow } else { MemberKind::Dot };

                    let member = CExprKind::Member(ty, base, field, member_kind);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, member);
                }

                ASTEntryTag::TagBinaryOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let operator = match expect_str(&node.extras[0]).expect("Expected operator") {
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

                    let opt_lhs_type_id = expect_opt_u64(&node.extras[1]).expect("Expected compute lhs type");
                    let opt_lhs_type = opt_lhs_type_id.map(|x| self.visit_qualified_type(x));

                    let opt_res_type_id = expect_opt_u64(&node.extras[2]).expect("Expected compute lhs type");
                    let opt_res_type = opt_res_type_id.map(|x| self.visit_qualified_type(x));

                    let binary = CExprKind::Binary(ty, operator, left_operand, right_operand, opt_lhs_type, opt_res_type);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, binary);
                }

                ASTEntryTag::TagDeclRefExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let declaration_old = node.children[0].expect("Expected declaration on expression tag decl");
                    let declaration = self.visit_decl(declaration_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let decl = CExprKind::DeclRef(ty, declaration);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, decl);
                }

                ASTEntryTag::TagArraySubscriptExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let lhs_old = node.children[0].expect("Expected LHS on array subscript expression");
                    let lhs = self.visit_expr(lhs_old);

                    let rhs_old = node.children[1].expect("Expected RHS on array subscript expression");
                    let rhs = self.visit_expr(rhs_old);

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let subscript = CExprKind::ArraySubscript(ty, lhs, rhs);

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

                    let kind_name = expect_str(&node.extras[0]).expect("expected kind");
                    let kind = match kind_name {
                        "sizeof" => UnTypeOp::SizeOf,
                        "alignof" => UnTypeOp::AlignOf,
                        str => panic!("Unsupported operation: {}", str),
                    };

                    let arg_ty = expect_u64(&node.extras[1]).expect("expected type id");
                    let arg_ty = self.visit_qualified_type(arg_ty);

                    let operator = CExprKind::UnaryType(ty, kind, arg_ty);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, operator);
                }

                ASTEntryTag::TagCompoundLiteralExpr => {
                    let ty_old = node.type_id.expect("Expected compound literal to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let val_old = node.children[0].expect("Expected child on compound literal");
                    let val = self.visit_expr(val_old);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, CExprKind::CompoundLiteral(ty, val))
                }

                ASTEntryTag::TagPredefinedExpr => {
                    let ty_old = node.type_id.expect("Expected predefined expr to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let val_old = node.children[0].expect("Expected child on predefined expr");
                    let val = self.visit_expr(val_old);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, CExprKind::Predefined(ty, val))
                }

                ASTEntryTag::TagImplicitValueInitExpr => {
                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, CExprKind::ImplicitValueInit(ty))
                }

                ASTEntryTag::TagInitListExpr => {

                    let exprs: Vec<CExprId> = node.children
                        .iter()
                        .map(|id| {
                            let expr_id = id.expect("init expression id");
                            self.visit_expr(expr_id)
                        })
                        .collect();

                    let ty_old = node.type_id.expect("Expected expression to have type");
                    let ty = self.visit_qualified_type(ty_old);

                    let union_field_id = expect_opt_u64(&node.extras[0]).expect("Bad union field ID entry").map(|x| self.visit_decl(x));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, CExprKind::InitList(ty, exprs, union_field_id))
                }

                // Declarations

                ASTEntryTag::TagFunctionDecl if expected_ty & OTHER_DECL != 0 => {
                    let name = expect_str(&node.extras[0])
                        .expect("Expected to find function name").to_string();

                    let is_extern = expect_bool(&node.extras[1]).expect("Expected to find visibility");
                    let is_inline = expect_bool(&node.extras[2]).expect("Expected to find inline");

                    let typ_old = node.type_id.expect("Expected to find a type on a function decl");
                    let typ = CTypeId(self.visit_node_type(typ_old, FUNC_TYPE));

                    let (body_id, parameter_ids) = node.children.split_last()
                        .expect("Expected to find a function body");

                    let body = body_id.map(|b| self.visit_stmt(b));

                    let parameters = parameter_ids
                        .iter()
                        .map(|id| {
                            let param = id.expect("Param field decl not found");
                            CDeclId(self.visit_node_type(param, VAR_DECL))
                        })
                        .collect();

                    let function_decl = CDeclKind::Function { is_extern, is_inline, typ, name, parameters, body };

                    self.add_decl(new_id, located(node, function_decl));
                    self.processed_nodes.insert(new_id, OTHER_DECL);
                }

                ASTEntryTag::TagTypedefDecl if expected_ty & TYPDEF_DECL != 0 => {
                    let name = expect_str(&node.extras[0]).expect("Expected to find typedef name").to_string();

                    let typ_old = node.type_id.expect("Expected to find type on typedef declaration");
                    let typ = self.visit_qualified_type(typ_old);

                    let typdef_decl = CDeclKind::Typedef { name, typ };

                    self.add_decl(new_id, located(node, typdef_decl));
                    self.processed_nodes.insert(new_id, TYPDEF_DECL);
                }

                ASTEntryTag::TagEnumDecl if expected_ty & ENUM_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);

                    let variants = node.children
                        .iter()
                        .map(|id| {
                            let con = id.expect("Enum constant not found");
                            CDeclId(self.visit_node_type(con, ENUM_CON))
                        })
                        .collect();

                    let enum_decl = CDeclKind::Enum { name, variants };

                    self.add_decl(new_id, located(node, enum_decl));
                    self.processed_nodes.insert(new_id, ENUM_DECL);
                }

                ASTEntryTag::TagEnumConstantDecl if expected_ty & ENUM_CON != 0 => {
                    let name = expect_str(&node.extras[0]).expect("Expected to find enum constant name").to_string();
                    let value = expect_i64(&node.extras[1]).expect("Expected to find enum constant's value");

                    let enum_constant_decl = CDeclKind::EnumConstant { name, value };

                    self.add_decl(new_id, located(node, enum_constant_decl));
                    self.processed_nodes.insert(new_id, ENUM_CON);
                }

                ASTEntryTag::TagVarDecl if expected_ty & VAR_DECL != 0 => {
                    let ident = expect_str(&node.extras[0]).expect("Expected to find variable name").to_string();

                    let is_static = expect_bool(&node.extras[1]).expect("Expected to find duration");
                    let is_extern = expect_bool(&node.extras[2]).expect("Expected to find visibility");
                    let is_defn = expect_bool(&node.extras[3]).expect("Expected to find whether decl is definition");
                    assert!(if is_extern { is_static } else { true }, "Something cannot be extern without also being static");

                    let initializer = node.children[0]
                        .map(|id| self.visit_expr(id));

                    let typ_id = node.type_id.expect("Expected to find type on variable declaration");
                    let typ = self.visit_qualified_type(typ_id);

                    let variable_decl = CDeclKind::Variable { is_static, is_extern, is_defn, ident, initializer, typ };

                    self.add_decl(new_id, located(node, variable_decl));
                    self.processed_nodes.insert(new_id, VAR_DECL);
                }

                ASTEntryTag::TagStructDecl if expected_ty & RECORD_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);
                    let fields: Vec<CDeclId> = node.children
                        .iter()
                        .map(|id| {
                            let field = id.expect("Record field decl not found");
                            CDeclId(self.visit_node_type(field, FIELD_DECL))
                        })
                        .collect();

                    for &field in &fields {
                        self.typed_context.field_parents.insert(field, CDeclId(new_id));
                    }

                    let record = CDeclKind::Struct { name, fields };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, RECORD_DECL);
                },

                ASTEntryTag::TagUnionDecl if expected_ty & RECORD_DECL != 0 => {
                    let name = expect_opt_str(&node.extras[0]).unwrap().map(str::to_string);
                    let fields: Vec<CDeclId> = node.children
                        .iter()
                        .map(|id| {
                            let field = id.expect("Record field decl not found");
                            CDeclId(self.visit_node_type(field, FIELD_DECL))
                        })
                        .collect();

                    for &field in &fields {
                        self.typed_context.field_parents.insert(field, CDeclId(new_id));
                    }

                    let record = CDeclKind::Union { name, fields };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, RECORD_DECL);
                },

                ASTEntryTag::TagFieldDecl if expected_ty & FIELD_DECL != 0 => {
                    let name = expect_str(&node.extras[0]).expect("A field needs a name").to_string();
                    let typ_id = node.type_id.expect("Expected to find type on field declaration");
                    let typ = self.visit_qualified_type(typ_id);
                    let field = CDeclKind::Field { name, typ };
                    self.add_decl(new_id, located(node, field));
                    self.processed_nodes.insert(new_id, FIELD_DECL);
                }

                t => panic!("Could not translate node {:?} as type {}", t, expected_ty),
            }
        }
    }
}


