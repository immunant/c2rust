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
/// we need to add nodes, sometimes we need to collapse.
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
  let mut visit_as: Vec<(u64, NodeType)> = Vec::new();
  for top_node in untyped_context.top_nodes.iter() {
    visit_as.push((*top_node, DECL));
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
    visit_as: &mut Vec<(u64, NodeType)>,
    node_id: &u64,
    node_ty: NodeType
  ) -> () {
    visit_as.push((*node_id, node_ty));
  }

  fn visit_opt_node_type(
    visit_as: &mut Vec<(u64, NodeType)>,
    opt_node_id: &Option<u64>,
    node_ty: NodeType
  ) -> () {
    for node_id in opt_node_id.iter() {
      visit_node_type(visit_as, node_id, node_ty);
    }
  }

  fn visit_vec_node_type(
    visit_as: &mut Vec<(u64, NodeType)>,
    opt_node_id: &Vec<u64>,
    node_ty: NodeType
  ) -> () {
    for node_id in opt_node_id.iter() {
      visit_node_type(visit_as, node_id, node_ty);
    }
  }


  while let Some((node_id, expected_ty)) = visit_as.pop() {
    // Check if we've already processed this node. If so, check that it has the right type.
    if let Some(node_ty) = processed_nodes.get(&node_id) {
      if node_ty & expected_ty == 0 {
        panic!("Expected {} to be a node of type {}, but found type {}", &node_id, expected_ty, node_ty);
      } else {
        continue;
      }
    }

    if expected_ty == TYPE {
      // Convert the node
      let ty_node: &TypeNode = untyped_context.type_nodes.get(&node_id).expect("Could not find type node");

      match ty_node.tag {
        TypeTag::TagBool => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Bool));
        }

        TypeTag::TagVoid => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Void));
        }

        TypeTag::TagInt => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Int));
        }

        TypeTag::TagShort => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Short));
        }

        TypeTag::TagLong => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Long));
        }

        TypeTag::TagLongLong => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::LongLong));
        }

        TypeTag::TagUInt => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::UInt));
        }

        TypeTag::TagUShort => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::UShort));
        }

        TypeTag::TagULong => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::ULong));
        }

        TypeTag::TagULongLong => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::ULongLong));
        }

        TypeTag::TagDouble => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Double));
        }

        TypeTag::TagLongDouble => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::LongDouble));
        }

        TypeTag::TagFloat => {
          typed_context.c_types.insert(node_id, not_located(CTypeKind::Float));
        }

        TypeTag::TagPointer => {
          let pointed = expect_u64(&ty_node.extras[0]).expect("Pointer child not found");
          let pointed_ty = CQualTypeId { qualifiers: qualifiers(ty_node), ctype: node_id };
          let pointer_ty = CTypeKind::Pointer(pointed_ty);
          typed_context.c_types.insert(node_id, not_located(pointer_ty));

          visit_node_type(&mut visit_as, &pointed, TYPE);
        }

        TypeTag::TagFunctionType => {
          let mut arguments: Vec<CQualTypeId> =
            expect_array(&ty_node.extras[0])
                .expect("Function type expects array argument")
                .iter()
                .map(|cbor| {
                  let ty_node_id = expect_u64(cbor).expect("Bad function type child id");
                  let ty_node = untyped_context.type_nodes.get(&ty_node_id).expect("Function type child not found");

                  visit_node_type(&mut visit_as, &ty_node_id, TYPE);

                  CQualTypeId { qualifiers: qualifiers(ty_node), ctype: ty_node_id }
                })
                .collect();
          let ret = arguments.remove(0);
          let function_ty = CTypeKind::FunctionProto(ret, arguments);
          typed_context.c_types.insert(node_id, not_located(function_ty));
        }

        TypeTag::TagTypeOfType => {
          let type_of_id = expect_u64(&ty_node.extras[0]).expect("Type of (type) child not found");
          let type_of = untyped_context.type_nodes.get(&type_of_id).expect("Type of (type) child not found");

          visit_node_type(&mut visit_as, &type_of_id, TYPE);

          let type_of_ty_qual = CQualTypeId { qualifiers: qualifiers(type_of), ctype: type_of_id };
          let type_of_ty = CTypeKind::TypeOf(type_of_ty_qual);
          typed_context.c_types.insert(node_id, not_located(type_of_ty));
        }

        t => panic!("Type conversion not implemented for {:?}", t),
      }
      processed_nodes.insert(node_id, TYPE);
    } else {

      // Convert the node
      let node: &AstNode = untyped_context.ast_nodes.get(&node_id).expect("Could not find ast node");

      match node.tag {
        // Statements

        ASTEntryTag::TagCompoundStmt if expected_ty & OTHER_STMT != 0 => {
          let constituent_stmts: Vec<CStmtId> = node.children
              .iter()
              .map(|id| id.expect("Compound stmt child not found"))
              .collect();
          visit_vec_node_type(&mut visit_as, &constituent_stmts, STMT);

          let compound_stmt = CStmtKind::Compound(constituent_stmts);

          typed_context.c_stmts.insert(node_id, located(node, compound_stmt));
          processed_nodes.insert(node_id, OTHER_STMT);
        }

        ASTEntryTag::TagDeclStmt if expected_ty & OTHER_STMT != 0 => {
          let decl = node.children[0].expect("Decl not found in decl-statement");
          visit_node_type(&mut visit_as, &decl, DECL);

          let decl_stmt = CStmtKind::Decl(decl);

          typed_context.c_stmts.insert(node_id, located(node, decl_stmt));
          processed_nodes.insert(node_id, OTHER_STMT);
        }

        ASTEntryTag::TagReturnStmt if expected_ty & OTHER_STMT != 0 => {
          let return_expr_opt = node.children[0];
          visit_opt_node_type(&mut visit_as, &return_expr_opt, EXPR);

          let return_stmt = CStmtKind::Return(return_expr_opt);

          typed_context.c_stmts.insert(node_id, located(node, return_stmt));
          processed_nodes.insert(node_id, OTHER_STMT);
        }

        ASTEntryTag::TagIfStmt if expected_ty & OTHER_STMT != 0 => {
          let scrutinee = node.children[0].expect("If condition expression not found");
          visit_node_type(&mut visit_as, &scrutinee, EXPR);

          let true_variant = node.children[1].expect("If then body statement not found");
          visit_node_type(&mut visit_as, &true_variant, STMT);

          let false_variant = node.children[2];
          visit_opt_node_type(&mut visit_as, &false_variant, STMT);

          let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };

          typed_context.c_stmts.insert(node_id, located(node, if_stmt));
          processed_nodes.insert(node_id, OTHER_STMT);
        }

        ASTEntryTag::TagGotoStmt if expected_ty & OTHER_STMT != 0 => {
          let target_label = node.children[0].expect("Goto target label not found");
          visit_node_type(&mut visit_as, &target_label, STMT);

          let goto_stmt = CStmtKind::Goto(target_label);

          typed_context.c_stmts.insert(node_id, located(node, goto_stmt));
          processed_nodes.insert(node_id, OTHER_STMT);
        }

        ASTEntryTag::TagNullStmt if expected_ty & OTHER_STMT != 0 => {
          let null_stmt = CStmtKind::Empty;

          typed_context.c_stmts.insert(node_id, located(node, null_stmt));
        }

        ASTEntryTag::TagLabelStmt if expected_ty & LABEL_STMT != 0 => {
          let pointed_stmt = node.children[0].expect("Label statement not found");
          visit_node_type(&mut visit_as, &pointed_stmt, STMT);

          let label_stmt = CStmtKind::Label(pointed_stmt);

          typed_context.c_stmts.insert(node_id, located(node, label_stmt));
          processed_nodes.insert(node_id, LABEL_STMT);
        }

        // Expressions

        ASTEntryTag::TagIntegerLiteral if expected_ty & EXPR != 0 => {
          let value = expect_u64(&node.extras[0]).expect("Expected integer literal value");

          let integer_literal = CExprKind::Literal(CLiteral::Integer(value));

          typed_context.c_exprs.insert(node_id, located(node, integer_literal));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagCharacterLiteral if expected_ty & EXPR != 0 => {
          let value = expect_u64(&node.extras[0]).expect("Expected character literal value");

          let character_literal = CExprKind::Literal(CLiteral::Character(value));

          typed_context.c_exprs.insert(node_id, located(node, character_literal));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagFloatingLiteral if expected_ty & EXPR != 0 => {
          let value = expect_f64(&node.extras[0]).expect("Expected float literal value");

          let floating_literal = CExprKind::Literal(CLiteral::Floating(value));

          typed_context.c_exprs.insert(node_id, located(node, floating_literal));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagUnaryOperator if expected_ty & EXPR != 0 => {
          let operator = match expect_str(&node.extras[0]).expect("Expected operator") {
            "&" => UnOp::AddressOf,
            "*" => UnOp::Deref,
            "+" => UnOp::Plus,
            "-" => UnOp::Negate,
            "~" => UnOp::Complement,
            "!" => UnOp::Not,
            _ => unimplemented!(),
          };

          let operand = node.children[0].expect("Expected operand");
          visit_node_type(&mut visit_as, &operand, EXPR);

          let unary = CExprKind::Unary(operator, operand);

          typed_context.c_exprs.insert(node_id, located(node, unary));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagImplicitCastExpr if expected_ty & EXPR != 0 => {
          let expression = node.children[0].expect("Expected expression for implicit cast");
          visit_node_type(&mut visit_as, &expression, EXPR);

          let typ = node.type_id.expect("Expected type for implicit cast");
          visit_node_type(&mut visit_as, &typ, TYPE);

          let implicit = CExprKind::ImplicitCast(typ, expression);

          typed_context.c_exprs.insert(node_id, located(node, implicit));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagBinaryOperator if expected_ty & EXPR != 0 => {
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

          let left_operand = node.children[0].expect("Expected left operand");

          let right_operand = node.children[1].expect("Expected right operand");

          let binary = CExprKind::Binary(operator, left_operand, right_operand);

          typed_context.c_exprs.insert(node_id, located(node, binary));
          processed_nodes.insert(node_id, EXPR);
        }

        ASTEntryTag::TagDeclRefExpr if expected_ty & EXPR != 0 => {
          let declaration = node.children[0].expect("Expected declaration on expression tag decl");
          visit_node_type(&mut visit_as, &declaration, DECL);

          let decl = CExprKind::DeclRef(declaration);

          typed_context.c_exprs.insert(node_id, located(node, decl));
          processed_nodes.insert(node_id, EXPR);
        }

        // Declarations

        ASTEntryTag::TagFunctionDecl if expected_ty & OTHER_DECL != 0 => {
          let name = expect_str(&node.extras[0]).expect("Expected to find function name").to_string();

          let typ = node.type_id.expect("Expected to find a type on a function decl");
          visit_node_type(&mut visit_as, &typ, TYPE);

          let body = node.children.last().expect("Function body not found").expect("Function body not found");
          visit_node_type(&mut visit_as, &body, STMT);

          let function_decl = CDeclKind::Function { typ, name, body };

          typed_context.c_decls.insert(node_id, located(node, function_decl));
          processed_nodes.insert(node_id, OTHER_DECL);
        }

        ASTEntryTag::TagVarDecl if expected_ty & OTHER_DECL != 0 => {
          let ident = expect_str(&node.extras[0]).expect("Expected to find variable name").to_string();

          let initializer = node.children[0];
          visit_opt_node_type(&mut visit_as, &initializer, EXPR);

          let typ = node.type_id.expect("Expected to find type on variable declaration");
          visit_node_type(&mut visit_as, &typ, TYPE);

          let variable_decl = CDeclKind::Variable { ident, initializer, typ };

          typed_context.c_decls.insert(node_id, located(node, variable_decl));
          processed_nodes.insert(node_id, OTHER_DECL);
        }

        ASTEntryTag::TagRecordDecl if expected_ty & OTHER_DECL != 0 => {
          let name = expect_str(&node.extras[0]).ok().map(str::to_string);
          let fields: Vec<CDeclId> = node.children
              .iter()
              .map(|id| id.expect("Record field decl not found"))
              .collect();
          visit_vec_node_type(&mut visit_as, &fields, FIELD_DECL);

          let record = CDeclKind::Record { name, fields };

          typed_context.c_decls.insert(node_id, located(node, record));
          processed_nodes.insert(node_id, OTHER_DECL);
        },

        ASTEntryTag::TagFieldDecl if expected_ty & FIELD_DECL != 0 => {
          let name = expect_str(&node.extras[0]).expect("A field needs a name").to_string();
          let field = CDeclKind::Field { name };
          typed_context.c_decls.insert(node_id, located(node, field));
          processed_nodes.insert(node_id, FIELD_DECL);
        }

        t => println!("Could not translate node {:?} as type {}", t, expected_ty),
      }
    }
  }

  typed_context
}


