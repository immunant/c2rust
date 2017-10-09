use std::collections::HashMap;
use std::vec::Vec;
use c_ast::*;
use clang_ast::*;


/// Possible node types
pub type NodeType = u16;

mod node_types {
  pub const TYPE       : super::NodeType = 0b00001;
  pub const EXPR       : super::NodeType = 0b00010;
  pub const DECL       : super::NodeType = 0b00100;
  pub const LABEL_STMT : super::NodeType = 0b01000;
  pub const OTHER_STMT : super::NodeType = 0b10000;
 
  pub const STMT       : super::NodeType = 0b11000; 
  pub const STMT_DECL  : super::NodeType = 0b11100;
  pub const ANYTHING   : super::NodeType = 0b11111;
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
    node_id: u64,
    node_ty: NodeType
  ) -> () {
    let existing = expected_nodes.entry(node_id).or_insert(ANYTHING);

    *existing &= node_ty;
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
        for constituent_stmt in constituent_stmts.iter() {
          expect_node_type(&mut expected_nodes, *constituent_stmt, STMT_DECL);
        }
        
        let compound_stmt = CStmtKind::Compound(constituent_stmts);
        typed_context.c_stmts.insert(*node_id, located(node, compound_stmt)); 

      }

      ASTEntryTag::TagReturnStmt => {
        let return_expr_opt = node.children[0];
        let return_stmt = CStmtKind::Return(return_expr_opt);
        typed_context.c_stmts.insert(*node_id, located(node, return_stmt));

        for return_expr in return_expr_opt.iter() {
          expect_node_type(&mut expected_nodes, *return_expr, EXPR);
        }
      }

      ASTEntryTag::TagIfStmt => {
        let scrutinee = node.children[0].expect("If condition expression not found");
        let true_variant = node.children[1].expect("If then body statement not found");
        let false_variant = node.children[2];
        let if_stmt = CStmtKind::If { scrutinee, true_variant, false_variant };
        typed_context.c_stmts.insert(*node_id, located(node, if_stmt));

        expect_node_type(&mut expected_nodes, scrutinee, EXPR);
        expect_node_type(&mut expected_nodes, true_variant, STMT);
        for else_stmt in false_variant.iter() {
          expect_node_type(&mut expected_nodes, *else_stmt, STMT);
        }
      }

      ASTEntryTag::TagGotoStmt => {
        let target_label = node.children[0].expect("Goto target label not found");
        let goto_stmt = CStmtKind::Goto(target_label);
        typed_context.c_stmts.insert(*node_id, located(node, goto_stmt));
        
        expected_nodes.insert(target_label, STMT);
      }


      ASTEntryTag::TagNullStmt => {
        let null_stmt = CStmtKind::Empty;
        typed_context.c_stmts.insert(*node_id, located(node, null_stmt));
      }

      ASTEntryTag::TagLabelStmt => {
        let pointed_stmt = node.children[0].expect("Label statement not found");
        let label_stmt = CStmtKind::Label(pointed_stmt);
        typed_context.c_stmts.insert(*node_id, located(node, label_stmt));
        
        expected_nodes.insert(pointed_stmt, STMT);
      }

      // Declarations

      ASTEntryTag::TagFunctionDecl => {
        let name = expect_str(&node.extras[0]).expect("Expected to find function name").to_string();
        let typ = node.type_id.expect("Expected to find a type on a function decl");
        let body = node.children.last().expect("Function body not found").expect("Function body not found");
        let function_decl = CDeclKind::Function { typ, name, body };
        typed_context.c_decls.insert(*node_id, located(node, function_decl)); 

        expected_nodes.insert(typ, TYPE);
        expected_nodes.insert(body, STMT);
      }

      _ => unimplemented!(),

    }
  }

  // Convert all of the 'TypeNodes'
  for (node_id, ty_node) in untyped_context.type_nodes.iter() {
    match ty_node.tag {
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
        let mut arguments: Vec<CQualTypeId> = ty_node.extras
          .iter()
          .map(|cbor| {
            let ty_node_id = expect_u64(cbor).expect("Function type child not found");
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

      _ => unimplemented!(),

    }
  }

  // Check that 'expected_node' expectations are satisfied
  for (node_id, expected_node_type) in expected_nodes {
    
    match expected_node_type {
      TYPE => assert!(typed_context.c_types.contains_key(&node_id), "Expected {} to be a type node", node_id),
      EXPR => assert!(typed_context.c_exprs.contains_key(&node_id), "Expected {} to be an expression node", node_id),
      DECL => assert!(typed_context.c_decls.contains_key(&node_id), "Expected {} to be a declaration node", node_id),
      STMT => assert!(typed_context.c_stmts.contains_key(&node_id), "Expected {} to be a statement node", node_id),
      
      LABEL_STMT | OTHER_STMT => {
        let is_label = match typed_context.c_stmts.get(&node_id) {
          Some(&Located { loc: _, kind: CStmtKind::Label(_) }) => true,
          _ => false,
        };

        if expected_node_type == LABEL_STMT {
          assert!(is_label, "Expected {} to be a label statement node", node_id)
        } else {
          assert!(!is_label, "Expected {} to be a non-label statement node", node_id)
        }
      }
    
      STMT_DECL =>
        assert!(
          typed_context.c_stmts.contains_key(&node_id) || typed_context.c_decls.contains_key(&node_id),
          "Expected {} to be a statement or a declaration node", node_id
        ),
       
      ANYTHING => { }, 

      _ => panic!("Not a valid node-type"),
    }
  }

  typed_context
}


