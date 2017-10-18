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

pub fn typed_ast_context(untyped_context: AstContext) -> TypedAstContext {

  use self::node_types::*;

  let mut typed_context = TypedAstContext::new(); 
  let mut expected_nodes: HashMap<u64, NodeType> = HashMap::new();

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

  fn expect_node_type(
    expected_nodes: &mut HashMap<u64, NodeType>,
    node_id: &u64,
    node_ty: NodeType
  ) -> () {
    let existing = expected_nodes.entry(*node_id).or_insert(ANYTHING);

    *existing &= node_ty;
  }

  fn expect_opt_node_type(
    expected_nodes: &mut HashMap<u64, NodeType>,
    opt_node_id: &Option<u64>,
    node_ty: NodeType
  ) -> () {
    for node_id in opt_node_id.iter() {
      expect_node_type(expected_nodes, node_id, node_ty);
    }
  }

  fn expect_vec_node_type(
    expected_nodes: &mut HashMap<u64, NodeType>,
    opt_node_id: &Vec<u64>,
    node_ty: NodeType
  ) -> () {
    for node_id in opt_node_id.iter() {
      expect_node_type(expected_nodes, node_id, node_ty);
    }
  }

  // Convert all 'AstNode's
  for (node_id, node) in untyped_context.ast_nodes.iter() {
    match node.tag {

      // Statements

      ASTEntryTag::TagCompoundStmt => {
        let constituent_stmts: Vec<CStmtId> = node.children
          .iter()
          .map(|id| id.expect("Compound stmt child not found"))
          .collect();
        expect_vec_node_type(&mut expected_nodes, &constituent_stmts, STMT);

        let compound_stmt = CStmtKind::Compound(constituent_stmts);

        typed_context.c_stmts.insert(*node_id, located(node, compound_stmt));
      }

      ASTEntryTag::TagDeclStmt => {
        let decl = node.children[0].expect("Decl not found in decl-statement");
        expect_node_type(&mut expected_nodes, &decl, DECL);

        let decl_stmt = CStmtKind::Decl(decl);

        typed_context.c_stmts.insert(*node_id, located(node, decl_stmt));
      }

      ASTEntryTag::TagReturnStmt => {
        let return_expr_opt = node.children[0];
        expect_opt_node_type(&mut expected_nodes, &return_expr_opt, EXPR);

        let return_stmt = CStmtKind::Return(return_expr_opt);

        typed_context.c_stmts.insert(*node_id, located(node, return_stmt));
      }

      ASTEntryTag::TagIfStmt => {
        let scrutinee = node.children[0].expect("If condition expression not found");
        expect_node_type(&mut expected_nodes, &scrutinee, EXPR);

        let true_variant = node.children[1].expect("If then body statement not found");
        expect_node_type(&mut expected_nodes, &true_variant, STMT);

        let false_variant = node.children[2];
        expect_opt_node_type(&mut expected_nodes, &false_variant, STMT);

        let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };

        typed_context.c_stmts.insert(*node_id, located(node, if_stmt));
      }

      ASTEntryTag::TagGotoStmt => {
        let target_label = node.children[0].expect("Goto target label not found");
        expect_node_type(&mut expected_nodes, &target_label, STMT);

        let goto_stmt = CStmtKind::Goto(target_label);

        typed_context.c_stmts.insert(*node_id, located(node, goto_stmt));
      }


      ASTEntryTag::TagNullStmt => {
        let null_stmt = CStmtKind::Empty;

        typed_context.c_stmts.insert(*node_id, located(node, null_stmt));
      }

      ASTEntryTag::TagLabelStmt => {
        let pointed_stmt = node.children[0].expect("Label statement not found");
        expect_node_type(&mut expected_nodes, &pointed_stmt, STMT);

        let label_stmt = CStmtKind::Label(pointed_stmt);

        typed_context.c_stmts.insert(*node_id, located(node, label_stmt));
      }

      // Expressions

      ASTEntryTag::TagIntegerLiteral => {
        let value = expect_u64(&node.extras[0]).expect("Expected integer literal value");

        let integer_literal = CExprKind::Literal(CLiteral::Integer(value));

        typed_context.c_exprs.insert(*node_id, located(node, integer_literal));
      }

      ASTEntryTag::TagCharacterLiteral => {
        let value = expect_u64(&node.extras[0]).expect("Expected character literal value");

        let character_literal = CExprKind::Literal(CLiteral::Character(value));

        typed_context.c_exprs.insert(*node_id, located(node, character_literal));
      }

      ASTEntryTag::TagFloatingLiteral => {
        let value = expect_f64(&node.extras[0]).expect("Expected float literal value");

        let floating_literal = CExprKind::Literal(CLiteral::Floating(value));

        typed_context.c_exprs.insert(*node_id, located(node, floating_literal));
      }

      ASTEntryTag::TagUnaryOperator => {
        let operator = match expect_str(&node.extras[0]).expect("Expected operator") {
          "&" => UnOp::AddressOf,
          "*" => UnOp::Deref,
          "+" => UnOp::Plus,
          "-" => UnOp::Negate,
          "~" => UnOp::Complement,
          "!" => UnOp::Not,
          _   => unimplemented!(),
        };

        let operand = node.children[0].expect("Expected operand");
        expect_node_type(&mut expected_nodes, &operand, EXPR);

        let unary = CExprKind::Unary(operator, operand);

        typed_context.c_exprs.insert(*node_id, located(node, unary));
      }

      ASTEntryTag::TagImplicitCastExpr => {
        let expression = node.children[0].expect("Expected expression for implicit cast");
        expect_node_type(&mut expected_nodes, &expression, EXPR);

        let typ = node.type_id.expect("Expected type for implicit cast");
        expect_node_type(&mut expected_nodes, &typ, TYPE);

        let implicit = CExprKind::ImplicitCast(typ, expression);

        typed_context.c_exprs.insert(*node_id, located(node, implicit));
      }

      ASTEntryTag::TagBinaryOperator => {
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

        typed_context.c_exprs.insert(*node_id, located(node, binary));
      }

      ASTEntryTag::TagDeclRefExpr => {
        let declaration = node.children[0].expect("Expected declaration on expression tag decl");
        expect_node_type(&mut expected_nodes, &declaration, DECL);

        let decl = CExprKind::DeclRef(declaration);

        typed_context.c_exprs.insert(*node_id, located(node, decl));
      }

      // Declarations

      ASTEntryTag::TagFunctionDecl => {
        let name = expect_str(&node.extras[0]).expect("Expected to find function name").to_string();

        let typ = node.type_id.expect("Expected to find a type on a function decl");
        expect_node_type(&mut expected_nodes, &typ, TYPE);

        let body = node.children.last().expect("Function body not found").expect("Function body not found");
        expect_node_type(&mut expected_nodes, &body, STMT);

        let function_decl = CDeclKind::Function { typ, name, body };

        typed_context.c_decls.insert(*node_id, located(node, function_decl));
      }

      ASTEntryTag::TagVarDecl => {
        let ident = expect_str(&node.extras[0]).expect("Expected to find variable name").to_string();

        let initializer = node.children[0];
        expect_opt_node_type(&mut expected_nodes, &initializer, EXPR);

        let typ = node.type_id.expect("Expected to find type on variable declaration");
        expect_node_type(&mut expected_nodes, &typ, TYPE);

        let variable_decl = CDeclKind::Variable { ident, initializer, typ };

        typed_context.c_decls.insert(*node_id, located(node, variable_decl));
      }

      ASTEntryTag::TagRecordDecl => {
        let name = expect_str(&node.extras[0]).ok().map(str::to_string);
        let fields: Vec<CDeclId> = node.children
            .iter()
            .map(|id| id.expect("Record field decl not found"))
            .collect();
        expect_vec_node_type(&mut expected_nodes, &fields, FIELD_DECL);

        let record = CDeclKind::Record { name, fields };

        typed_context.c_decls.insert(*node_id, located(node, record));
      },

      ASTEntryTag::TagFieldDecl => {
        let name = expect_str(&node.extras[0]).expect("A field needs a name").to_string();
        let field = CDeclKind::Field { name };
        typed_context.c_decls.insert(*node_id, located(node, field));
      }


      t => println!("Declaration not implemented {:?}", t),

    }
  }

  // Convert all of the 'TypeNodes'
  for (node_id, ty_node) in untyped_context.type_nodes.iter() {
    match ty_node.tag {
      TypeTag::TagBool => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Bool));
      }

      TypeTag::TagVoid => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Void));
      }

      TypeTag::TagInt => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Int));
      }

      TypeTag::TagShort => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Short));
      }
      
      TypeTag::TagLong => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Long));
      }

      TypeTag::TagLongLong => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::LongLong));
      }
      
      TypeTag::TagUInt => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::UInt));
      }
      
      TypeTag::TagUShort => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::UShort));
      }
      
      TypeTag::TagULong => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::ULong));
      }
      
      TypeTag::TagULongLong => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::ULongLong));
      }
      
      TypeTag::TagDouble => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Double));
      }
      
      TypeTag::TagLongDouble => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::LongDouble));
      }
      
      TypeTag::TagFloat => {
        typed_context.c_types.insert(*node_id, not_located(CTypeKind::Float));
      }

      TypeTag::TagPointer => {
        let pointed = expect_u64(&ty_node.extras[0]).expect("Pointer child not found");
        let pointed_ty = CQualTypeId { qualifiers: qualifiers(ty_node), ctype: *node_id };
        let pointer_ty = CTypeKind::Pointer(pointed_ty);
        typed_context.c_types.insert(*node_id, not_located(pointer_ty));

        expected_nodes.insert(pointed, TYPE);
      }

      TypeTag::TagFunctionType => {
        let mut arguments: Vec<CQualTypeId> =
          expect_array(&ty_node.extras[0])
              .expect("Function type expects array argument")
          .iter()
          .map(|cbor| {
            let ty_node_id = expect_u64(cbor).expect("Bad function type child id");
            let ty_node = untyped_context.type_nodes.get(&ty_node_id).expect("Function type child not found");
          
            expected_nodes.insert(ty_node_id, TYPE);

            CQualTypeId { qualifiers: qualifiers(ty_node), ctype: ty_node_id }
          })
          .collect();
        let ret = arguments.remove(0);
        let function_ty = CTypeKind::FunctionProto(ret, arguments);
        typed_context.c_types.insert(*node_id, not_located(function_ty));
      }

      TypeTag::TagTypeOfType => {
        let type_of_id = expect_u64(&ty_node.extras[0]).expect("Type of (type) child not found");
        let type_of = untyped_context.type_nodes.get(&type_of_id).expect("Type of (type) child not found");

        expected_nodes.insert(type_of_id, TYPE);

        let type_of_ty_qual = CQualTypeId { qualifiers: qualifiers(type_of), ctype: type_of_id };
        let type_of_ty = CTypeKind::TypeOf(type_of_ty_qual);
        typed_context.c_types.insert(*node_id, not_located(type_of_ty));
      }

      t => panic!("Type conversion not implemented for {:?}", t),

    }
  }

  // Disjoint and most refined partition of all the possible types
  let node_types = vec![TYPE, EXPR, FIELD_DECL, OTHER_DECL, LABEL_STMT, OTHER_STMT];

  // Check that 'expected_node' expectations are satisfied
  for (node_id, expect_node_type) in expected_nodes {


    match expect_node_type {

      TYPE if typed_context.c_types.contains_key(&node_id) => { }
      TYPE => panic!("Expected {} to be a type node", node_id),

      EXPR if typed_context.c_exprs.contains_key(&node_id) => { }
      EXPR => panic!("Expected {} to be an expression node", node_id),

      DECL if typed_context.c_decls.contains_key(&node_id) => { }
      DECL => panic!("Expected {} to be a declaration node", node_id),


      STMT if typed_context.c_stmts.contains_key(&node_id) => { }
      STMT if typed_context.c_exprs.contains_key(&node_id) => {

        // Clang expressions are a subclass of statements. We catch that and make it explicit.
        let wrapped = CStmtKind::Expr(node_id);
        let original_ast_node = untyped_context.ast_nodes.get(&node_id).unwrap();

        typed_context.c_stmts.insert(node_id, located(original_ast_node, wrapped));
      }
      STMT => panic!("Expected {} to be a statement node", node_id),

      FIELD_DECL | OTHER_DECL => {
        let is_field = match typed_context.c_decls.get(&node_id) {
          Some(&Located { loc: _, kind: CDeclKind::Field { .. } }) => true,
          _ => false,
        };

        if expect_node_type == FIELD_DECL {
          assert!(is_field, "Expected {} to be a field declaration node", node_id)
        } else {
          assert!(!is_field, "Expected {} to be a non-field declaration node", node_id)
        }
      }

      LABEL_STMT | OTHER_STMT => {
        let is_label = match typed_context.c_stmts.get(&node_id) {
          Some(&Located { loc: _, kind: CStmtKind::Label(_) }) => true,
          _ => false,
        };

        if expect_node_type == LABEL_STMT {
          assert!(is_label, "Expected {} to be a label statement node", node_id)
        } else {
          assert!(!is_label, "Expected {} to be a non-label statement node", node_id)
        }
      }
      _ => panic!("Not a valid node-type"),
    }
  }

  typed_context
}


