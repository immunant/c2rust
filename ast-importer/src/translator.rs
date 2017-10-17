
use syntax::ast::*;
use renamer::Renamer;
use convert_type::TypeConverter;
use idiomize::ast_manip::make_ast::*;
use clang_ast::*;
use syntax::ptr::*;
use syntax::print::pprust::*;
use std::collections::HashSet;

pub struct Translation {
    pub items: Vec<P<Item>>,
    pub type_converter: TypeConverter,
    pub ast_context: AstContext,
    renamer: Renamer<String>,
}

pub struct WithStmts<T> {
    stmts: Vec<Stmt>,
    val: T,
}

impl<T> WithStmts<T> {
    pub fn new(val: T) -> Self {
        WithStmts { stmts: vec![], val, }
    }
}

pub fn with_stmts_opt<T>(opt: Option<WithStmts<T>>) -> WithStmts<Option<T>> {
    match opt {
        None => WithStmts::new(None),
        Some(x) => WithStmts { stmts: x.stmts, val: Some(x.val) },
    }
}

pub fn translate(ast_context: AstContext) -> String {
    use clang_ast::*;
    let mut t = Translation::new(ast_context.clone());

    for top_id in ast_context.top_nodes.to_owned() {
        let x = match ast_context.ast_nodes.get(&top_id) {
            Some(n) => n.clone(),
            None => continue,
        };

        if x.tag == ASTEntryTag::TagFunctionDecl {

            let name = expect_string(&x.extras[0]).expect("Expected a name");

            let ty = ast_context.get_type(x.type_id.expect("Expected a type")).expect("Expected a number");
            let funtys = expect_array(&ty.extras[0]).expect("Function declaration type expected");
            let ret = expect_u64(&funtys[0]).expect("Expected a return type");

            let args_n = x.children.len() - 1;
            let args : Vec<(String,u64)> =
                x.children[0 .. args_n]
                 .iter().map(|x| {
                     let p = ast_context.ast_nodes.get(&x.expect("Missing parameter id")).expect("Bad parameter id");
                     let param_name = expect_string(&p.extras[0]).expect("Parameter name required");
                     (param_name, p.type_id.expect("Parameter type required"))
                 }).collect();

            let args : Vec<(&str, u64)> = args.iter().map(|&(ref x,y)| (x.as_str(),y)).collect();
            let body = x.children[args_n].expect("Expected body id");

            t.add_function(&name, &args, ret, body);
        }
    }

    to_string(|s| {

        for x in t.items.iter() {
            s.print_item(x)?
        }

        Ok(())
    })
}



impl Translation {
    pub fn new(ast_context: AstContext) -> Translation {
        Translation {
            items: vec![],
            type_converter: TypeConverter::new(),
            ast_context,
            renamer: Renamer::new(HashSet::new()),
            // XXX: Populate reserved words
        }
    }

    pub fn add_struct(&mut self, name: Ident, fields: &[(&str, u64)]) {
        let struct_fields =
            fields
                .iter()
                .map(|&(id, ty)| {
                    let ty = self.type_converter.convert(&self.ast_context, ty);
                    mk().struct_field(id, ty)
                })
                .collect();

        let item = mk().struct_item(name, struct_fields);

        self.items.push(item);
    }

    pub fn add_typedef(&mut self, name: &str, typeid: u64) {
        let ty = self.convert_type(typeid);
        let item = mk().type_item(name, ty);
        self.items.push(item);
    }

    pub fn add_function(&mut self, name: &str, arguments: &[(&str, u64)], return_type: u64, body: u64) {
        // Start scope for function parameters
        self.renamer.add_scope();

        let args: Vec<Arg> = arguments.iter().map(|&(var, ty)| {
            let rust_var = self.renamer.insert(var.to_string(), var).expect("Failed to insert argument");
            mk().arg(self.convert_type(ty), mk().ident_pat(rust_var))
        }).collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type));

        let decl = mk().fn_decl(args, ret);

        let block = self.convert_function_body(body);

        // End scope for function parameters
        self.renamer.drop_scope();

        self.items.push(mk().fn_item(name, decl, block));
    }

    fn convert_function_body(&mut self, body_id: u64) -> P<Block> {
        let node =
            self.ast_context
                .ast_nodes
                .get(&body_id)
                .expect("Expected function body node")
                .to_owned(); // release immutable borrow on self

        assert_eq!(node.tag, ASTEntryTag::TagCompoundStmt);

        // Open function body scope
        self.renamer.add_scope();

        let stmts: Vec<Stmt> =
            node.children
                .iter()
                .flat_map(|&stmt_id| {
                    self.convert_stmt(stmt_id.unwrap())
                }).collect();

        // Close function body scope
        self.renamer.drop_scope();

        mk().block(stmts)
    }

    fn convert_stmt(&mut self, stmt_id: u64) -> Vec<Stmt> {
        let node: AstNode =
            self.ast_context
                .ast_nodes
                .get(&stmt_id)
                .unwrap()
                .to_owned(); // release immutable borrow on self

        match node.tag {
            ASTEntryTag::TagDeclStmt =>
                node.children.iter().flat_map(|decl_id| self.convert_decl_stmt(decl_id.unwrap())).collect(),
            ASTEntryTag::TagReturnStmt => {
                self.convert_return_stmt(node.children[0])
            }
            ASTEntryTag::TagIfStmt => {
                self.convert_if_stmt(node.children[0].unwrap(), node.children[1].unwrap(), node.children[2])
            }
            ASTEntryTag::TagCompoundStmt => {
                self.renamer.add_scope();

                let stmts = node.children.into_iter().flat_map(|x| x).flat_map(|x| self.convert_stmt(x)).collect();

                self.renamer.drop_scope();

                vec![mk().expr_stmt(mk().block_expr(mk().block(stmts)))]
            }
            t => {
                let res = self.convert_expr(stmt_id);
                let mut stmts = res.stmts;
                stmts.push(mk().expr_stmt(res.val));
                stmts
            },
        }
    }

    fn convert_if_stmt(&mut self, cond_id: u64, then_id: u64, else_id: Option<u64>) -> Vec<Stmt> {
        let mut cond = self.convert_expr(cond_id);
        let then_stmts = mk().block(self.convert_stmt(then_id));
        let else_stmts = else_id.map(|x| { mk().block_expr(mk().block(self.convert_stmt(x)))});

        cond.stmts.push(mk().expr_stmt(mk().ifte_expr(cond.val, then_stmts, else_stmts)));
        cond.stmts
    }

    fn convert_return_stmt(&mut self, result_id: Option<u64>) -> Vec<Stmt> {
        let val = result_id.map(|i| self.convert_expr(i));
        let mut ws = with_stmts_opt(val);
        let ret = mk().expr_stmt(mk().return_expr(ws.val));

        ws.stmts.push(ret);
        ws.stmts
    }

    fn convert_decl_stmt(&mut self, decl_id: u64) -> Vec<Stmt> {
        let node: AstNode =
            self.ast_context
                .ast_nodes
                .get(&decl_id)
                .unwrap()
                .to_owned(); // release immutable borrow on self

        match node.tag {
            ASTEntryTag::TagVarDecl => {
                let var_name = expect_string(&node.extras[0]).unwrap();
                let rust_name = self.renamer.insert(var_name.clone(), &var_name).unwrap();
                let pat = mk().set_mutbl(Mutability::Mutable).ident_pat(rust_name);
                let init = with_stmts_opt(node.children[0].map(|x| self.convert_expr(x)));
                let ty = self.convert_type(node.type_id.unwrap());
                let local = mk().local(pat, Some(ty), init.val);

                let mut stmts = init.stmts;
                stmts.push(mk().local_stmt(P(local)));
                stmts
            }
            t => panic!("Declaration not implemented {:?}", t),
        }
    }

    fn convert_type(&self, type_id: u64) -> P<Ty> {
        self.type_converter.convert(&self.ast_context, type_id)
    }

    fn convert_expr(&mut self, expr_id: u64) -> WithStmts<P<Expr>> {
        println!("Converting {}", expr_id);
        let node = self.ast_context.ast_nodes.get(&expr_id).expect("Expected expression node").clone();

        match node.tag {
            ASTEntryTag::TagDeclRefExpr =>
                {
                    let child =
                        self.ast_context.ast_nodes.get(&node.children[0].expect("Expected decl id"))
                            .expect("Expected decl node");


                    assert_eq!(child.tag, ASTEntryTag::TagVarDecl);
                    let varname = expect_string(&child.extras[0]).expect("expected variable name");
                    let rustname = self.renamer.get(varname).expect("name not declared");
                    WithStmts::new(mk().path_expr(vec![rustname]))
                }
            ASTEntryTag::TagIntegerLiteral =>
                {
                    let val = expect_u64(&node.extras[0]).expect("Expected value");
                    let ty = self.convert_type(node.type_id.expect("Expected type"));
                    WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed)))
                }
            ASTEntryTag::TagImplicitCastExpr =>
                {
                    // TODO actually cast
                    let child = node.children[0].expect("Expected subvalue");
                    self.convert_expr(child)
                }
            ASTEntryTag::TagBinaryOperator =>
                {
                    let name = expect_string(&node.extras[0]).expect("Missing binary operator name");
                    let lhs = self.convert_expr(node.children[0].expect("Missing LHS"));
                    let rhs = self.convert_expr(node.children[1].expect("Missing RHS"));
                    let type_id = node.type_id.unwrap();
                    let cty = self.ast_context.get_type(type_id).unwrap();
                    let ty = self.convert_type(type_id);
                    let bin = self.convert_binary_operator(&name, cty, ty, lhs.val, rhs.val);

                    WithStmts {
                        stmts: lhs.stmts.into_iter().chain(rhs.stmts).chain(bin.stmts).collect(),
                        val: bin.val,
                    }
                }
            t => panic!("Expression not implemented {:?}", t),
        }
    }

    pub fn convert_binary_operator(&mut self, name: &str, ctype: TypeNode, ty: P<Ty>, lhs: P<Expr>, rhs: P<Expr>) -> WithStmts<P<Expr>>
    {
        match name {

            "+" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_add"), vec![rhs])),
            "+" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Add), lhs, rhs)),

            "-" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_sub"), vec![rhs])),
            "-" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Sub), lhs, rhs)),

            "*" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_mul"), vec![rhs])),
            "*" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Mul), lhs, rhs)),

            "/" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_div"), vec![rhs])),
            "/" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Div), lhs, rhs)),

            "%" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_rem"), vec![rhs])),
            "%" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Rem), lhs, rhs)),

            "^" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::BitXor), lhs, rhs)),

            ">>" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Shr), lhs, rhs)),

            "==" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Eq), lhs, rhs)),
            "<" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Lt), lhs, rhs)),
            ">" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Gt), lhs, rhs)),
            ">=" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Ge), lhs, rhs)),
            "<=" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Le), lhs, rhs)),

            "&&" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::And), lhs, rhs)),
            "||" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::Or), lhs, rhs)),

            "&" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::BitAnd), lhs, rhs)),
            "|" => WithStmts::new(mk().binary_expr(mk().spanned(BinOpKind::BitOr), lhs, rhs)),

            "+=" => {
                /*
                let tmpvar = self.renamer.fresh();
                let sum = self.convert_binary_operator("+", ctype, ty, lhs.clone(), rhs);
                let tmplet = mk().local_stmt(P(mk().local(mk().ident_pat(&tmpvar), None as Option<P<Ty>>, Some(sum.val))));
                let tmpval = mk().ident_expr(&tmpvar);
                let assign = mk().expr_stmt(mk().assign_expr(lhs, &tmpval));

                let mut stmts = sum.stmts;
                stmts.push(tmplet);
                stmts.push(assign);

                WithStmts {
                    stmts: stmts,
                    val: tmpval,
                }
                */
                unimplemented!()
            },
            "-=" => unimplemented!(),
            "*=" => unimplemented!(),
            "/=" => unimplemented!(),
            "%=" => unimplemented!(),
            "^=" => unimplemented!(),
            "!=" => unimplemented!(),
            "<<=" => unimplemented!(),
            ">>=" => unimplemented!(),
            "|=" => unimplemented!(),
            "&=" => unimplemented!(),

            _ => panic!("Unknown binary operator"),
        }
    }
}
