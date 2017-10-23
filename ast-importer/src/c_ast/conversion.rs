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

pub fn typed_ast_context(untyped_context: AstContext) -> TypedAstContext {

  use self::node_types::*;

  // Typed context we will be adding to
  let mut typed_context = TypedAstContext::new();

  // Mapping old to new IDs and reverse
  let mut id_mapper = IdMapper::new();

  // Keep track of new nodes already processed and their types
  let mut processed_nodes: HashMap<NewId, NodeType> = HashMap::new();

  // Stack of things to visit (and their expected types). This starts out as all of the top-level
  // nodes, which we expect to be 'DECL's
  let mut visit_as: Vec<(ClangId, NodeType)> = Vec::new();
  for top_node in untyped_context.top_nodes.iter() {
    if (untyped_context.ast_nodes.contains_key(top_node)) {
      visit_as.push((*top_node, DECL));
    } else {
      println!("Not pushing top-level node {} since it isn't in the AstContext", top_node);
    }
  }

  /// Transfer location information off of an 'AstNode' and onto something that is 'Located'
  fn located<T>(node: &AstNode, t: T) -> Located<T> {
    Located {
      loc: Some(SrcLoc { line: node.line, column: node.column, fileid: node.fileid }),
      kind: t
    }
  }

  fn not_located<T>(t: T) -> Located<T> {
    Located {
      loc: None,
      kind: t
    }
  }

  fn qualifiers(ty_node: &TypeNode) -> Qualifiers {
    Qualifiers {
      is_const: ty_node.constant,
      is_restrict: false,
      is_volatile: false,
    }
  }

  fn visit_node_type(
    id_mapper: &mut IdMapper,
    visit_as: &mut Vec<(ClangId, NodeType)>,
    node_id: &ClangId,
    node_ty: NodeType
  ) -> NewId {
    visit_as.push((*node_id, node_ty));
    id_mapper.get_or_create_new(*node_id)
  }

  fn visit_opt_node_type(
    id_mapper: &mut IdMapper,
    visit_as: &mut Vec<(ClangId, NodeType)>,
    opt_node_id: &Option<ClangId>,
    node_ty: NodeType
  ) -> Option<NewId> {
    let mut to_return: Option<NewId> = None;
    for node_id in opt_node_id.iter() {
      to_return = Some(visit_node_type(id_mapper, visit_as, node_id, node_ty));
    }
    to_return
  }

  fn visit_vec_node_type(
    id_mapper: &mut IdMapper,
    visit_as: &mut Vec<(ClangId, NodeType)>,
    opt_node_id: &Vec<ClangId>,
    node_ty: NodeType
  ) -> Vec<NewId> {
    let mut to_return: Vec<NewId> = Vec::new();
    for node_id in opt_node_id.iter() {
      to_return.push(visit_node_type(id_mapper, visit_as, node_id, node_ty));
    }
    to_return
  }

  /// Clang has `Expression <: Statement`, but we want to make that explicit via the
  /// `CStmtKind::Expr` statement constructor. This function automatically converts expressions into
  /// statements depending on the expected type argument.
  fn expr_possibly_as_stmt(
    expected_ty: NodeType,
    new_id: NewId,
    id_mapper: &mut IdMapper,
    typed_context: &mut TypedAstContext,
    processed_nodes: &mut HashMap<NewId, NodeType>,
    node: &AstNode,
    expr: CExprKind,
  ) -> () {

    if (expected_ty & STMT != 0) {
      // This is going to be an extra node not present in the Clang AST
      let new_expr_id = id_mapper.fresh_id();
      typed_context.c_exprs.insert(new_expr_id, located(node, expr));
      processed_nodes.insert(new_expr_id, EXPR);

      // We wrap the expression in a STMT
      let semi_stmt = CStmtKind::Expr(new_expr_id);
      typed_context.c_stmts.insert(new_id, located(node, semi_stmt));
      processed_nodes.insert(new_id, STMT);
    } else if (expected_ty & EXPR != 0) {
      typed_context.c_exprs.insert(new_id, located(node, expr));
      processed_nodes.insert(new_id, EXPR);
    } else {
      panic!("'expr_possibly_as_stmt' expects 'expected_ty' to be either 'EXPR' or 'STMT'");
    }
  }


  while let Some((node_id, expected_ty)) = visit_as.pop() {

    // Check if we've already processed this node. If so, check that it has the right type.
    if let Some(node_ty) = id_mapper.get_new(node_id)
                                    .and_then(|new_id| processed_nodes.get(&new_id)) {
      if node_ty & expected_ty == 0 {
        panic!("Expected {} to be a node of type {}, but found type {}", &node_id, expected_ty, node_ty);
      } else {
        continue;
      }
    }

    // Create a NewId for this node
    let new_id = id_mapper.get_or_create_new(node_id);

    // If the node is top-level, add it as such to the new context
    if (untyped_context.top_nodes.contains(&node_id)) {
      typed_context.c_decls_top.insert(new_id);
    }

    if expected_ty == TYPE {
      // Convert the node
      let ty_node: &TypeNode = untyped_context.type_nodes.get(&node_id).expect("Could not find type node");

      match ty_node.tag {
        TypeTag::TagBool => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Bool));
        }

        TypeTag::TagVoid => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Void));
        }

        TypeTag::TagInt => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Int));
        }

        TypeTag::TagShort => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Short));
        }

        TypeTag::TagLong => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Long));
        }

        TypeTag::TagLongLong => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::LongLong));
        }

        TypeTag::TagUInt => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::UInt));
        }

        TypeTag::TagUShort => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::UShort));
        }

        TypeTag::TagULong => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::ULong));
        }

        TypeTag::TagULongLong => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::ULongLong));
        }

        TypeTag::TagDouble => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Double));
        }

        TypeTag::TagLongDouble => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::LongDouble));
        }

        TypeTag::TagFloat => {
          typed_context.c_types.insert(new_id, not_located(CTypeKind::Float));
        }

        TypeTag::TagPointer => {
          let pointed = expect_u64(&ty_node.extras[0]).expect("Pointer child not found");
          let pointed_new = visit_node_type(&mut id_mapper, &mut visit_as, &pointed, TYPE);

          let pointed_ty = CQualTypeId { qualifiers: qualifiers(ty_node), ctype: pointed_new };
          let pointer_ty = CTypeKind::Pointer(pointed_ty);
          typed_context.c_types.insert(new_id, not_located(pointer_ty));
        }

        TypeTag::TagFunctionType => {
          let mut arguments: Vec<CQualTypeId> =
            expect_array(&ty_node.extras[0])
                .expect("Function type expects array argument")
                .iter()
                .map(|cbor| {
                  let ty_node_id = expect_u64(cbor).expect("Bad function type child id");
                  let ty_node = untyped_context.type_nodes.get(&ty_node_id).expect("Function type child not found");

                  let ty_node_new_id = visit_node_type(&mut id_mapper, &mut visit_as, &ty_node_id, TYPE);

                  CQualTypeId { qualifiers: qualifiers(ty_node), ctype: ty_node_new_id }
                })
                .collect();
          let ret = arguments.remove(0);
          let function_ty = CTypeKind::FunctionProto(ret, arguments);
          typed_context.c_types.insert(new_id, not_located(function_ty));
        }

        TypeTag::TagTypeOfType => {
          let type_of_id = expect_u64(&ty_node.extras[0]).expect("Type of (type) child not found");
          let type_of = untyped_context.type_nodes.get(&type_of_id).expect("Type of (type) child not found");
          let type_of_new_id = visit_node_type(&mut id_mapper, &mut visit_as, &type_of_id, TYPE);

          let type_of_ty_qual = CQualTypeId { qualifiers: qualifiers(type_of), ctype: type_of_new_id };
          let type_of_ty = CTypeKind::TypeOf(type_of_ty_qual);
          typed_context.c_types.insert(new_id, not_located(type_of_ty));
        }

        t => panic!("Type conversion not implemented for {:?}", t),
      }
      processed_nodes.insert(new_id, TYPE);
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
                visit_node_type(&mut id_mapper, &mut visit_as, &arg_id, STMT)
              })
              .collect();

          let compound_stmt = CStmtKind::Compound(constituent_stmts);

          typed_context.c_stmts.insert(new_id, located(node, compound_stmt));
          processed_nodes.insert(new_id, OTHER_STMT);
        }

        ASTEntryTag::TagDeclStmt if expected_ty & OTHER_STMT != 0 => {
          let decl = node.children[0].expect("Decl not found in decl-statement");
          let decl_new = visit_node_type(&mut id_mapper, &mut visit_as, &decl, DECL);

          let decl_stmt = CStmtKind::Decl(decl_new);

          typed_context.c_stmts.insert(new_id, located(node, decl_stmt));
          processed_nodes.insert(new_id, OTHER_STMT);
        }

        ASTEntryTag::TagReturnStmt if expected_ty & OTHER_STMT != 0 => {
          let return_expr_opt = node.children[0]
              .map(|id| visit_node_type(&mut id_mapper, &mut visit_as, &id, EXPR));

          let return_stmt = CStmtKind::Return(return_expr_opt);

          typed_context.c_stmts.insert(new_id, located(node, return_stmt));
          processed_nodes.insert(new_id, OTHER_STMT);
        }

        ASTEntryTag::TagIfStmt if expected_ty & OTHER_STMT != 0 => {
          let scrutinee_old = node.children[0].expect("If condition expression not found");
          let scrutinee = visit_node_type(&mut id_mapper, &mut visit_as, &scrutinee_old, EXPR);

          let true_variant_old = node.children[1].expect("If then body statement not found");
          let true_variant = visit_node_type(&mut id_mapper, &mut visit_as, &true_variant_old, STMT);

          let false_variant = node.children[2]
              .map(|id| visit_node_type(&mut id_mapper, &mut visit_as, &id, STMT));

          let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };

          typed_context.c_stmts.insert(new_id, located(node, if_stmt));
          processed_nodes.insert(new_id, OTHER_STMT);
        }

        ASTEntryTag::TagGotoStmt if expected_ty & OTHER_STMT != 0 => {
          let target_label_old = node.children[0].expect("Goto target label not found");
          let target_label = visit_node_type(&mut id_mapper, &mut visit_as, &target_label_old, STMT);

          let goto_stmt = CStmtKind::Goto(target_label);

          typed_context.c_stmts.insert(new_id, located(node, goto_stmt));
          processed_nodes.insert(new_id, OTHER_STMT);
        }

        ASTEntryTag::TagNullStmt if expected_ty & OTHER_STMT != 0 => {
          let null_stmt = CStmtKind::Empty;

          typed_context.c_stmts.insert(new_id, located(node, null_stmt));
        }

        ASTEntryTag::TagLabelStmt if expected_ty & LABEL_STMT != 0 => {
          let pointed_stmt_old = node.children[0].expect("Label statement not found");
          let pointed_stmt = visit_node_type(&mut id_mapper, &mut visit_as, &pointed_stmt_old, STMT);

          let label_stmt = CStmtKind::Label(pointed_stmt);

          typed_context.c_stmts.insert(new_id, located(node, label_stmt));
          processed_nodes.insert(new_id, LABEL_STMT);
        }

        // Expressions

        ASTEntryTag::TagIntegerLiteral if expected_ty & (EXPR | STMT) != 0 => {
          let value = expect_u64(&node.extras[0]).expect("Expected integer literal value");

          let integer_literal = CExprKind::Literal(CLiteral::Integer(value));

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, integer_literal);
        }

        ASTEntryTag::TagCharacterLiteral if expected_ty & (EXPR | STMT) != 0 => {
          let value = expect_u64(&node.extras[0]).expect("Expected character literal value");

          let character_literal = CExprKind::Literal(CLiteral::Character(value));

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, character_literal);
        }

        ASTEntryTag::TagFloatingLiteral if expected_ty & (EXPR | STMT) != 0 => {
          let value = expect_f64(&node.extras[0]).expect("Expected float literal value");

          let floating_literal = CExprKind::Literal(CLiteral::Floating(value));

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, floating_literal);
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
          let operand = visit_node_type(&mut id_mapper, &mut visit_as, &operand_old, EXPR);

          let unary = CExprKind::Unary(operator, operand);

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, unary);
        }

        ASTEntryTag::TagImplicitCastExpr if expected_ty & (EXPR | STMT) != 0 => {
          let expression_old = node.children[0].expect("Expected expression for implicit cast");
          let expression = visit_node_type(&mut id_mapper, &mut visit_as, &expression_old, EXPR);

          let typ_old = node.type_id.expect("Expected type for implicit cast");
          let typ = visit_node_type(&mut id_mapper, &mut visit_as, &typ_old, TYPE);

          let implicit = CExprKind::ImplicitCast(typ, expression);

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, implicit);
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
          let left_operand = visit_node_type(&mut id_mapper, &mut visit_as, &left_operand_old, EXPR);

          let right_operand_old = node.children[1].expect("Expected right operand");
          let right_operand = visit_node_type(&mut id_mapper, &mut visit_as, &right_operand_old, EXPR);

          let binary = CExprKind::Binary(operator, left_operand, right_operand);

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, binary);
        }

        ASTEntryTag::TagDeclRefExpr if expected_ty & (EXPR | STMT) != 0 => {
          let declaration_old = node.children[0].expect("Expected declaration on expression tag decl");
          let declaration = visit_node_type(&mut id_mapper, &mut visit_as, &declaration_old, DECL);

          let decl = CExprKind::DeclRef(declaration);

          expr_possibly_as_stmt(expected_ty, new_id, &mut id_mapper, &mut typed_context, &mut processed_nodes, node, decl);
        }

        // Declarations

        ASTEntryTag::TagFunctionDecl if expected_ty & OTHER_DECL != 0 => {
          let name = expect_str(&node.extras[0]).expect("Expected to find function name").to_string();

          let typ_old = node.type_id.expect("Expected to find a type on a function decl");
          let typ = visit_node_type(&mut id_mapper, &mut visit_as, &typ_old, TYPE);

          let body_old = node.children.last().expect("Function body not found").expect("Function body not found");
          let body = visit_node_type(&mut id_mapper, &mut visit_as, &body_old, STMT);

          let function_decl = CDeclKind::Function { typ, name, body };

          typed_context.c_decls.insert(new_id, located(node, function_decl));
          processed_nodes.insert(new_id, OTHER_DECL);
        }

        ASTEntryTag::TagVarDecl if expected_ty & OTHER_DECL != 0 => {
          let ident = expect_str(&node.extras[0]).expect("Expected to find variable name").to_string();

          let initializer = node.children[0]
              .map(|id| visit_node_type(&mut id_mapper, &mut visit_as, &id, EXPR));

          let typ_old = node.type_id.expect("Expected to find type on variable declaration");
          let typ = visit_node_type(&mut id_mapper, &mut visit_as, &typ_old, TYPE);

          let variable_decl = CDeclKind::Variable { ident, initializer, typ };

          typed_context.c_decls.insert(new_id, located(node, variable_decl));
          processed_nodes.insert(new_id, OTHER_DECL);
        }

        ASTEntryTag::TagRecordDecl if expected_ty & OTHER_DECL != 0 => {
          let name = expect_str(&node.extras[0]).ok().map(str::to_string);
          let fields: Vec<CDeclId> = node.children
              .iter()
              .map(|id| {
                let field = id.expect("Record field decl not found");
                visit_node_type(&mut id_mapper, &mut visit_as, &field, FIELD_DECL)
              })
              .collect();

          let record = CDeclKind::Record { name, fields };

          typed_context.c_decls.insert(new_id, located(node, record));
          processed_nodes.insert(new_id, OTHER_DECL);
        },

        ASTEntryTag::TagFieldDecl if expected_ty & FIELD_DECL != 0 => {
          let name = expect_str(&node.extras[0]).expect("A field needs a name").to_string();
          let field = CDeclKind::Field { name };
          typed_context.c_decls.insert(new_id, located(node, field));
          processed_nodes.insert(new_id, FIELD_DECL);
        }

        t => println!("Could not translate node {:?} as type {}", t, expected_ty),
      }
    }
  }

  typed_context
}


