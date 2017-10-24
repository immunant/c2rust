use std::collections::HashMap;
use std::vec::Vec;
use c_ast::*;
use clang_ast::*;


/// Possible node types
pub type NodeType = u16;

mod node_types {
    pub const TYPE       : super::NodeType = 0b000001;
    pub const EXPR       : super::NodeType = 0b000010;
    pub const FIELD_DECL : super::NodeType = 0b000100;
    pub const OTHER_DECL : super::NodeType = 0b001000;
    pub const LABEL_STMT : super::NodeType = 0b010000;
    pub const OTHER_STMT : super::NodeType = 0b100000;

    pub const DECL       : super::NodeType = FIELD_DECL | OTHER_DECL;
    pub const STMT       : super::NodeType = LABEL_STMT | OTHER_STMT;
    pub const ANYTHING   : super::NodeType = TYPE | EXPR | DECL | STMT;
}

type ClangId = u64;
type NewId = u64;

/// Correspondance between old/new IDs.
///
/// We need to re-ID nodes since the mapping from Clang's AST to ours is not one-to-one. Sometimes
/// we need to add nodes (such as 'Semi' nodes to make the lifting of expressions into statements
/// explicit), sometimes we need to collapse (such as inlining 'FieldDecl' into the 'StructDecl').
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
                self.old_to_new.insert(old_id, new_id);
                new_id
            }
        }
    }

    /// Lookup the CLANG_ID corresponding to a NEW_ID
    pub fn get_old(&mut self, new_id: NewId) -> Option<ClangId> {
        self.new_to_old.get(&new_id).map(|n| *n)
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

/// Extract the qualifiers off of a `TypeNode`
fn qualifiers(ty_node: &TypeNode) -> Qualifiers {
    Qualifiers {
        is_const: ty_node.constant,
        is_restrict: false,
        is_volatile: false,
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
        for top_node in untyped_context.top_nodes.iter() {
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
    fn visit_node_type(&mut self, node_id: &ClangId, node_ty: NodeType) -> NewId {
        self.visit_as.push((*node_id, node_ty));
        self.id_mapper.get_or_create_new(*node_id)
    }

    /// Add a `CType`node into the `TypedAstContext`
    fn add_type(&mut self, id: NewId, typ: CType) -> () {
        self.typed_context.c_types.insert(id, typ);
    }

    /// Add a `CStmt` node into the `TypedAstContext`
    fn add_stmt(&mut self, id: NewId, stmt: CStmt) -> () {
        self.typed_context.c_stmts.insert(id, stmt);
    }

    /// Add a `CExpr` node into the `TypedAstContext`
    fn add_expr(&mut self, id: NewId, expr: CExpr) -> () {
        self.typed_context.c_exprs.insert(id, expr);
    }

    /// Add a `CDecl` node into the `TypedAstContext`
    fn add_decl(&mut self, id: NewId, decl: CDecl) -> () {
        self.typed_context.c_decls.insert(id, decl);
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
            let semi_stmt = CStmtKind::Expr(new_expr_id);
            self.add_stmt(new_id, located(node, semi_stmt));
            self.processed_nodes.insert(new_id, node_types::STMT);
        } else if (expected_ty & node_types::EXPR != 0) {
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
                self.typed_context.c_decls_top.insert(new_id);
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

        if expected_ty == TYPE {

            // Convert the node
            let ty_node: &TypeNode = untyped_context.type_nodes
                .get(&node_id)
                .expect("Could not find type node");

            match ty_node.tag {
                TypeTag::TagBool => {
                    self.add_type(new_id, not_located(CTypeKind::Bool));
                }

                TypeTag::TagVoid => {
                    self.add_type(new_id, not_located(CTypeKind::Void));
                }

                TypeTag::TagInt => {
                    self.add_type(new_id, not_located(CTypeKind::Int));
                }

                TypeTag::TagShort => {
                    self.add_type(new_id, not_located(CTypeKind::Short));
                }

                TypeTag::TagLong => {
                    self.add_type(new_id, not_located(CTypeKind::Long));
                }

                TypeTag::TagLongLong => {
                    self.add_type(new_id, not_located(CTypeKind::LongLong));
                }

                TypeTag::TagUInt => {
                    self.add_type(new_id, not_located(CTypeKind::UInt));
                }

                TypeTag::TagUShort => {
                    self.add_type(new_id, not_located(CTypeKind::UShort));
                }

                TypeTag::TagULong => {
                    self.add_type(new_id, not_located(CTypeKind::ULong));
                }

                TypeTag::TagULongLong => {
                    self.add_type(new_id, not_located(CTypeKind::ULongLong));
                }

                TypeTag::TagDouble => {
                    self.add_type(new_id, not_located(CTypeKind::Double));
                }

                TypeTag::TagLongDouble => {
                    self.add_type(new_id, not_located(CTypeKind::LongDouble));
                }

                TypeTag::TagFloat => {
                    self.add_type(new_id, not_located(CTypeKind::Float));
                }

                TypeTag::TagPointer => {
                    let pointed = expect_u64(&ty_node.extras[0])
                        .expect("Pointer child not found");
                    let pointed_new = self.visit_node_type( &pointed, TYPE);

                    let pointed_ty = CQualTypeId {
                        qualifiers: qualifiers(ty_node),
                        ctype: pointed_new
                    };
                    let pointer_ty = CTypeKind::Pointer(pointed_ty);
                    self.add_type(new_id, not_located(pointer_ty));
                }

                TypeTag::TagFunctionType => {
                    let mut arguments: Vec<CQualTypeId> = expect_array(&ty_node.extras[0])
                        .expect("Function type expects array argument")
                        .iter()
                        .map(|cbor| {
                            let ty_node_id = expect_u64(cbor).expect("Bad function type child id");
                            let ty_node = untyped_context.type_nodes
                                .get(&ty_node_id)
                                .expect("Function type child not found");

                            let ty_node_new_id = self.visit_node_type( &ty_node_id, TYPE);

                            CQualTypeId { qualifiers: qualifiers(ty_node), ctype: ty_node_new_id }
                        })
                        .collect();
                    let ret = arguments.remove(0);
                    let function_ty = CTypeKind::FunctionProto(ret, arguments);
                    self.add_type(new_id, not_located(function_ty));
                }

                TypeTag::TagTypeOfType => {
                    let type_of_id = expect_u64(&ty_node.extras[0]).expect("Type of (type) child not found");
                    let type_of = untyped_context.type_nodes
                        .get(&type_of_id)
                        .expect("Type of (type) child not found");
                    let type_of_new_id = self.visit_node_type(&type_of_id, TYPE);

                    let type_of_ty_qual = CQualTypeId {
                        qualifiers: qualifiers(type_of),
                        ctype: type_of_new_id
                    };
                    let type_of_ty = CTypeKind::TypeOf(type_of_ty_qual);
                    self.add_type(new_id, not_located(type_of_ty));
                }

                t => panic!("Type conversion not implemented for {:?}", t),
            }
            self.processed_nodes.insert(new_id, TYPE);

        } else {
            // Convert the node
            let node: &AstNode = untyped_context.ast_nodes
                .get(&node_id)
                .expect(format!("Could not find ast node {}", node_id).as_ref());

            match node.tag {
                // Statements

                ASTEntryTag::TagCompoundStmt if expected_ty & OTHER_STMT != 0 => {
                    let constituent_stmts: Vec<CStmtId> = node.children
                        .iter()
                        .map(|id| {
                            let arg_id = id.expect("Compound stmt child not found");
                            self.visit_node_type(&arg_id, STMT)
                        })
                        .collect();

                    let compound_stmt = CStmtKind::Compound(constituent_stmts);

                    self.add_stmt(new_id, located(node, compound_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagDeclStmt if expected_ty & OTHER_STMT != 0 => {
                    let decl = node.children[0].expect("Decl not found in decl-statement");
                    let decl_new = self.visit_node_type(&decl, DECL);

                    let decl_stmt = CStmtKind::Decl(decl_new);

                    self.add_stmt(new_id, located(node, decl_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagReturnStmt if expected_ty & OTHER_STMT != 0 => {
                    let return_expr_opt = node.children[0]
                        .map(|id| self.visit_node_type(&id, EXPR));

                    let return_stmt = CStmtKind::Return(return_expr_opt);

                    self.add_stmt(new_id, located(node, return_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagIfStmt if expected_ty & OTHER_STMT != 0 => {
                    let scrutinee_old = node.children[0].expect("If condition expression not found");
                    let scrutinee = self.visit_node_type(&scrutinee_old, EXPR);

                    let true_variant_old = node.children[1].expect("If then body statement not found");
                    let true_variant = self.visit_node_type(&true_variant_old, STMT);

                    let false_variant = node.children[2]
                        .map(|id| self.visit_node_type(&id, STMT));

                    let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };

                    self.add_stmt(new_id, located(node, if_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagGotoStmt if expected_ty & OTHER_STMT != 0 => {
                    let target_label_old = node.children[0].expect("Goto target label not found");
                    let target_label = self.visit_node_type(&target_label_old, STMT);

                    let goto_stmt = CStmtKind::Goto(target_label);

                    self.add_stmt(new_id, located(node, goto_stmt));
                    self.processed_nodes.insert(new_id, OTHER_STMT);
                }

                ASTEntryTag::TagNullStmt if expected_ty & OTHER_STMT != 0 => {
                    let null_stmt = CStmtKind::Empty;

                    self.add_stmt(new_id, located(node, null_stmt));
                }

                ASTEntryTag::TagLabelStmt if expected_ty & LABEL_STMT != 0 => {
                    let pointed_stmt_old = node.children[0].expect("Label statement not found");
                    let pointed_stmt = self.visit_node_type(&pointed_stmt_old, STMT);

                    let label_stmt = CStmtKind::Label(pointed_stmt);

                    self.add_stmt(new_id, located(node, label_stmt));
                    self.processed_nodes.insert(new_id, LABEL_STMT);
                }

                // Expressions

                ASTEntryTag::TagIntegerLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_u64(&node.extras[0]).expect("Expected integer literal value");

                    let integer_literal = CExprKind::Literal(CLiteral::Integer(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, integer_literal);
                }

                ASTEntryTag::TagCharacterLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_u64(&node.extras[0]).expect("Expected character literal value");

                    let character_literal = CExprKind::Literal(CLiteral::Character(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, character_literal);
                }

                ASTEntryTag::TagFloatingLiteral if expected_ty & (EXPR | STMT) != 0 => {
                    let value = expect_f64(&node.extras[0]).expect("Expected float literal value");

                    let floating_literal = CExprKind::Literal(CLiteral::Floating(value));

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, floating_literal);
                }

                ASTEntryTag::TagUnaryOperator if expected_ty & (EXPR | STMT) != 0 => {
                    let operator = match expect_str(&node.extras[0]).expect("Expected operator") {
                        "&" => UnOp::AddressOf,
                        "*" => UnOp::Deref,
                        "+" => UnOp::Plus,
                        "-" => UnOp::Negate,
                        "~" => UnOp::Complement,
                        "!" => UnOp::Not,
                        _ => unimplemented!(),
                    };

                    let operand_old = node.children[0].expect("Expected operand");
                    let operand = self.visit_node_type(&operand_old, EXPR);

                    let unary = CExprKind::Unary(operator, operand);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, unary);
                }

                ASTEntryTag::TagImplicitCastExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let expression_old = node.children[0].expect("Expected expression for implicit cast");
                    let expression = self.visit_node_type(&expression_old, EXPR);

                    let typ_old = node.type_id.expect("Expected type for implicit cast");
                    let typ = self.visit_node_type(&typ_old, TYPE);

                    let implicit = CExprKind::ImplicitCast(typ, expression);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, implicit);
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
                        _ => unimplemented!(),
                    };

                    let left_operand_old = node.children[0].expect("Expected left operand");
                    let left_operand = self.visit_node_type(&left_operand_old, EXPR);

                    let right_operand_old = node.children[1].expect("Expected right operand");
                    let right_operand = self.visit_node_type(&right_operand_old, EXPR);

                    let binary = CExprKind::Binary(operator, left_operand, right_operand);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, binary);
                }

                ASTEntryTag::TagDeclRefExpr if expected_ty & (EXPR | STMT) != 0 => {
                    let declaration_old = node.children[0].expect("Expected declaration on expression tag decl");
                    let declaration = self.visit_node_type(&declaration_old, DECL);

                    let decl = CExprKind::DeclRef(declaration);

                    self.expr_possibly_as_stmt(expected_ty, new_id, node, decl);
                }

                // Declarations

                ASTEntryTag::TagFunctionDecl if expected_ty & OTHER_DECL != 0 => {
                    let name = expect_str(&node.extras[0]).expect("Expected to find function name").to_string();

                    let typ_old = node.type_id.expect("Expected to find a type on a function decl");
                    let typ = self.visit_node_type(&typ_old, TYPE);

                    let body_old = node.children.last().expect("Function body not found").expect("Function body not found");
                    let body = self.visit_node_type(&body_old, STMT);

                    let function_decl = CDeclKind::Function { typ, name, body };

                    self.add_decl(new_id, located(node, function_decl));
                    self.processed_nodes.insert(new_id, OTHER_DECL);
                }

                ASTEntryTag::TagVarDecl if expected_ty & OTHER_DECL != 0 => {
                    let ident = expect_str(&node.extras[0]).expect("Expected to find variable name").to_string();

                    let initializer = node.children[0]
                        .map(|id| self.visit_node_type(&id, EXPR));

                    let typ_old = node.type_id.expect("Expected to find type on variable declaration");
                    let typ = self.visit_node_type(&typ_old, TYPE);

                    let variable_decl = CDeclKind::Variable { ident, initializer, typ };

                    self.add_decl(new_id, located(node, variable_decl));
                    self. processed_nodes.insert(new_id, OTHER_DECL);
                }

                ASTEntryTag::TagRecordDecl if expected_ty & OTHER_DECL != 0 => {
                    let name = expect_str(&node.extras[0]).ok().map(str::to_string);
                    let fields: Vec<CDeclId> = node.children
                        .iter()
                        .map(|id| {
                            let field = id.expect("Record field decl not found");
                            self.visit_node_type(&field, FIELD_DECL)
                        })
                        .collect();

                    let record = CDeclKind::Record { name, fields };

                    self.add_decl(new_id, located(node, record));
                    self.processed_nodes.insert(new_id, OTHER_DECL);
                },

                ASTEntryTag::TagFieldDecl if expected_ty & FIELD_DECL != 0 => {
                    let name = expect_str(&node.extras[0]).expect("A field needs a name").to_string();
                    let field = CDeclKind::Field { name };
                    self.add_decl(new_id, located(node, field));
                    self.processed_nodes.insert(new_id, FIELD_DECL);
                }

                t => println!("Could not translate node {:?} as type {}", t, expected_ty),
            }
        }
    }
}


