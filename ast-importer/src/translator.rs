
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
    pub fn and_then<U,F: FnOnce(T) -> WithStmts<U>>(self, f : F) -> WithStmts<U> {
        let mut next = f(self.val);
        let mut stmts = self.stmts;
        stmts.append(&mut next.stmts);
        WithStmts {
            val: next.val,
            stmts
        }
    }
    pub fn map<U,F: FnOnce(T) -> U>(self, f : F) -> WithStmts<U> {
        WithStmts {
            val: f(self.val),
            stmts: self.stmts,
        }
    }
}

impl WithStmts<P<Expr>> {
    pub fn to_expr(mut self) -> P<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            self.stmts.push(mk().expr_stmt(self.val));
            mk().block_expr(mk().block(self.stmts))
        }
    }
}

fn pointer_offset(ptr: P<Expr>, offset: P<Expr>) -> P<Expr> {
    let offset = mk().cast_expr(offset, mk().path_ty(vec!["isize"]));
    mk().method_call_expr(ptr, "offset", vec![offset])
}


pub fn stmts_block(mut stmts: Vec<Stmt>) -> P<Block> {
    if stmts.len() == 1 {
        if let StmtKind::Expr(ref e) = stmts[0].node {
            if let ExprKind::Block(ref b) = e.node {
                    return b.clone()
            }
        }
    }

    if stmts.len() > 0 {
        let n = stmts.len() - 1;
        let s = stmts.remove(n);
        stmts.push(s.add_trailing_semicolon())
    }

    mk().block(stmts)
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

    // Populate renamer with top-level names
    for top_id in ast_context.top_nodes.to_owned() {
        if let Some(x) = ast_context.ast_nodes.get(&top_id) {
           if let Some(y) = x.get_decl_name() {
               t.renamer.insert(y.to_owned(), &y);
           }
        }
    }

    for top_id in ast_context.top_nodes.to_owned() {
        let x = match ast_context.ast_nodes.get(&top_id) {
            Some(n) => n.clone(),
            None => continue,
        };

        if x.tag == ASTEntryTag::TagFunctionDecl {

            let name = expect_string(&x.extras[0]).expect("Expected a name");

            let type_id = x.type_id.expect("Expected a type");
            let ty = match ast_context.get_type(type_id) {
                None => panic!("Missing type node for type_id {}", type_id),
                Some(_ty) => _ty
            };
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

/// Convert a boolean expression to a c_int
fn bool_to_int(val: P<Expr>) -> P<Expr> {
    mk().cast_expr(val, mk().path_ty(vec!["libc","c_int"]))
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
            mk().arg(self.convert_type(ty), mk().mutbl().ident_pat(rust_var))
        }).collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type));

        let decl = mk().fn_decl(args, ret);

        let block = self.convert_function_body(body);

        // End scope for function parameters
        self.renamer.drop_scope();

        self.items.push(mk().unsafe_().fn_item(name, decl, block));
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

        stmts_block(stmts)
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
            ASTEntryTag::TagWhileStmt => {
                let cond_id = node.children[0].unwrap();
                let body_id = node.children[1].unwrap();
                self.convert_while_stmt(cond_id, body_id)
            }
            ASTEntryTag::TagForStmt => {
                let init_id = node.children[0];
                let cond_id = node.children[1];
                let inc_id = node.children[2];
                let body_id = node.children[3].unwrap();
                self.convert_for_stmt(init_id, cond_id, inc_id, body_id)
            }
            ASTEntryTag::TagNullStmt => {
                vec![]
            }
            ASTEntryTag::TagCompoundStmt => {
                self.renamer.add_scope();

                let stmts = node.children.into_iter().flat_map(|x| x).flat_map(|x| self.convert_stmt(x)).collect();

                self.renamer.drop_scope();

                vec![mk().expr_stmt(mk().block_expr(stmts_block(stmts)))]
            }
            t => {
                let mut xs = self.convert_expr(stmt_id);
                xs.stmts.push(mk().expr_stmt(xs.val));
                xs.stmts
            },
        }
    }

    /// Convert a C expression to a rust boolean expression
    fn convert_condition(&mut self, cond_id: u64) -> WithStmts<P<Expr>> {
        let node: AstNode =
            self.ast_context
                .ast_nodes
                .get(&cond_id)
                .unwrap()
                .to_owned(); // release immutable borrow on self
        let type_id = node.type_id.expect("type id");
        let ty = self.ast_context.get_type(type_id).expect("type");
        let cond = self.convert_expr_node(node);
        cond.map(|e| self.to_bool(ty, e))
    }

    fn convert_while_stmt(&mut self, cond_id: u64, body_id: u64) -> Vec<Stmt> {

        let cond = self.convert_condition(cond_id);
        let body = self.convert_stmt(body_id);

        let rust_cond = cond.to_expr();
        let rust_body = stmts_block(body);

        println!("condition: {:?}", rust_cond);

        vec![mk().expr_stmt(mk().while_expr(rust_cond, rust_body))]
    }

    fn convert_for_stmt(&mut self, init_id: Option<u64>, cond_id: Option<u64>, inc_id: Option<u64>, body_id: u64) -> Vec<Stmt> {

        self.renamer.add_scope();

        let mut init = match init_id {
          Some(i) => self.convert_stmt(i),
          None => vec![],
        };

        let mut inc = match inc_id {
            Some(i) => self.convert_stmt(i),
            None => vec![],
        };

        let mut body = self.convert_stmt(body_id);
        body.append(&mut inc);

        let body_block = stmts_block(body);

        let looper = match cond_id {
            None => mk().loop_expr(body_block), // loop
            Some(i) => mk().while_expr(self.convert_condition(i).to_expr(), body_block), // while
        };

        init.push(mk().expr_stmt(looper));

        self.renamer.drop_scope();

        vec![mk().expr_stmt(mk().block_expr(mk().block(init)))]

    }

    fn convert_if_stmt(&mut self, cond_id: u64, then_id: u64, else_id: Option<u64>) -> Vec<Stmt> {
        let mut cond = self.convert_condition(cond_id);
        let then_stmts = stmts_block(self.convert_stmt(then_id));
        let else_stmts = else_id.map(|x| { mk().block_expr(stmts_block(self.convert_stmt(x)))});

        cond.stmts.push(mk().semi_stmt(mk().ifte_expr(cond.val, then_stmts, else_stmts)));
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
                let pat = mk().mutbl().ident_pat(rust_name);
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
        let node = self.ast_context.ast_nodes.get(&expr_id).expect("Expected expression node").clone();
        self.convert_expr_node(node)

    }
    fn convert_expr_node(&mut self, node: AstNode) -> WithStmts<P<Expr>> {
        match node.tag {
            ASTEntryTag::TagParenExpr => {
                let child_id = node.children[0].expect("child id");
                self.convert_expr(child_id)
            }

            ASTEntryTag::TagDeclRefExpr =>
                {
                    let child =
                        self.ast_context.ast_nodes.get(&node.children[0].expect("Expected decl id"))
                            .expect("Expected decl node");

                    let varname = child.get_decl_name().expect("expected variable name").to_owned();
                    let rustname = self.renamer.get(varname).expect("name not declared");
                    WithStmts::new(mk().path_expr(vec![rustname]))
                }
            ASTEntryTag::TagIntegerLiteral =>
                {
                    let val = expect_u64(&node.extras[0]).expect("Expected value");
                    let _ty = self.convert_type(node.type_id.expect("Expected type"));
                    WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed)))
                }
            ASTEntryTag::TagCharacterLiteral =>
                {
                    let val = expect_u64(&node.extras[0]).expect("Expected value");
                    let _ty = self.convert_type(node.type_id.expect("Expected type"));
                    WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed)))
                }
            ASTEntryTag::TagFloatingLiteral =>
                {
                    let val = expect_f64(&node.extras[0]).expect("Expected value");
                    let str = format!("{}", val);
                    WithStmts::new(mk().lit_expr(mk().float_unsuffixed_lit(str)))
                }
            ASTEntryTag::TagImplicitCastExpr =>
                {
                    // TODO actually cast
                    // Numeric casts with 'as', pointer casts with transmute
                    let child = node.children[0].expect("Expected subvalue");
                    self.convert_expr(child)
                }
            ASTEntryTag::TagUnaryOperator =>
                {
                    let name = expect_string(&node.extras[0]).expect("Missing binary operator name");
                    let prefix = expect_bool(&node.extras[1]).expect("is_prefix");
                    let mut arg = self.convert_expr(node.children[0].expect("Missing value"));
                    let type_id = node.type_id.unwrap();
                    let cty = self.ast_context.get_type(type_id).unwrap();
                    let ty = self.convert_type(type_id);
                    let mut unary = self.convert_unary_operator(&name, prefix, cty, ty, arg.val);
                    arg.stmts.append(&mut unary.stmts);
                    WithStmts {
                        stmts: arg.stmts,
                        val: unary.val,
                    }
                }
            ASTEntryTag::TagBinaryOperator =>
                {
                    let name = expect_string(&node.extras[0]).expect("Missing binary operator name");
                    let lhs_node = self.ast_context.ast_nodes.get(&node.children[0].expect("lhs id")).expect("lhs node").to_owned();
                    let lhs_ty = self.ast_context.get_type(lhs_node.type_id.expect("lhs ty id")).expect("lhs ty");
                    let lhs = self.convert_expr_node(lhs_node);
                    let rhs_node = self.ast_context.ast_nodes.get(&node.children[1].expect("rhs id")).expect("rhs node").to_owned();
                    let rhs_ty = self.ast_context.get_type(rhs_node.type_id.expect("rhs ty id")).expect("rhs ty");
                    let mut rhs = self.convert_expr_node(rhs_node);
                    let type_id = node.type_id.unwrap();
                    let cty = self.ast_context.get_type(type_id).unwrap();
                    let ty = self.convert_type(type_id);

                    match name.as_str() {
                        "," =>
                            lhs.and_then(|x| {
                                rhs.stmts.insert(0,mk().expr_stmt(x));
                                rhs
                            }),

                        "&&" =>
                            lhs.map(|x| mk().binary_expr(BinOpKind::And, x, rhs.to_expr())),

                        "||" =>
                            lhs.map(|x| mk().binary_expr(BinOpKind::Or, x, rhs.to_expr())),

                        // No sequence-point cases
                        _ => {
                            let bin =
                                self.convert_binary_operator(&name, ty, cty, lhs_ty, rhs_ty, lhs.val, rhs.val);

                            WithStmts {
                                stmts: lhs.stmts.into_iter().chain(rhs.stmts).chain(bin.stmts).collect(),
                                val: bin.val,
                            }
                        }
                    }
                },
            ASTEntryTag::TagArraySubscriptExpr => {
                let lhs_node = self.ast_context.ast_nodes.get(&node.children[0].expect("lhs id")).expect("lhs node").to_owned();
                let lhs_ty = self.ast_context.get_type(lhs_node.type_id.expect("lhs ty id")).expect("lhs ty");
                let lhs = self.convert_expr_node(lhs_node);

                let rhs_node = self.ast_context.ast_nodes.get(&node.children[1].expect("rhs id")).expect("rhs node").to_owned();
                let rhs_ty = self.ast_context.get_type(rhs_node.type_id.expect("rhs ty id")).expect("rhs ty");
                let mut rhs = self.convert_expr_node(rhs_node);

                let val =
                    if self.ast_context.resolve_type(lhs_ty).is_pointer() {
                        pointer_offset(lhs.val, rhs.val)
                    } else {
                        pointer_offset(rhs.val, lhs.val)
                    };
                let val = mk().unary_expr(UnOp::Deref, val);

                let mut stmts = lhs.stmts;
                stmts.append(&mut rhs.stmts);

                WithStmts { stmts, val }
            }
            ASTEntryTag::TagCallExpr =>
                {
                    let mut stmts = vec![];
                    let mut exprs = vec![];

                    for x in node.children.iter() {
                        let mut res = self.convert_expr(x.unwrap());
                        stmts.append(&mut res.stmts);
                        exprs.push(res.val);
                    }

                    let fun = exprs.remove(0);

                    WithStmts {
                        stmts,
                        val: mk().call_expr(fun, exprs),
                    }
                }
            ASTEntryTag::TagMemberExpr => {
                let mut struct_val = self.convert_expr(node.children[0].expect("Missing structval"));
                let field_node = self.ast_context.ast_nodes.get(&node.children[1].expect("Missing structfield id")).expect("Missing structfield").clone();
                let field_name = expect_str(&field_node.extras[0]).expect("expected field name");

                struct_val.val = mk().field_expr(struct_val.val, field_name);
                struct_val
            }
            t => panic!("Expression not implemented {:?}", t),
        }
    }

    pub fn name_reference(&mut self, reference: P<Expr>) -> WithStmts<P<Expr>> {

        fn is_path_expr(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path{..} => true,
                _ => false,
            }
        }

        let is_simple = match &reference.node {
            &ExprKind::Path{..} => true,
            &ExprKind::Unary(UnOp::Deref, ref e) => is_path_expr(e),
            _ => false,
        };

        if is_simple {
            WithStmts::new(reference)
        } else {
            let ptr_name = self.renamer.fresh();
            // let ref mut p = lhs;
            let compute_ref =
                mk().local_stmt(
                    P(mk().local(mk().mutbl().ident_ref_pat(&ptr_name),
                                 None as Option<P<Ty>>,
                                 Some(reference)))
                );
            WithStmts {
                val: mk().unary_expr(UnOp::Deref, mk().ident_expr(&ptr_name)),
                stmts: vec![compute_ref],
            }
        }
    }

    pub fn convert_pre_increment(&mut self, ctype: TypeNode, up: bool, arg: P<Expr>) -> WithStmts<P<Expr>> {

        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(arg);

        let one = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        // *p + 1
        let val =
            if self.ast_context.resolve_type(ctype).is_pointer() {
                // This calls the offset with a number literal directly, and doesn't need
                // the cast that the pointer_offset function adds
                let n = if up { one } else { mk().unary_expr(UnOp::Neg, one) };
                mk().method_call_expr(deref_lhs.clone(), "offset", vec![n])
            } else {
                let k = if up { BinOpKind::Add } else { BinOpKind::Sub };
                mk().binary_expr(k, deref_lhs.clone(), one)
            };

        // *p = *p + rhs
        let assign_stmt = mk().assign_expr(&deref_lhs.clone(), val);

        lhs_stmts.push(mk().expr_stmt(assign_stmt));

        WithStmts {
            stmts: lhs_stmts,
            val: deref_lhs,
        }
    }

    pub fn convert_post_increment(&mut self, ctype: TypeNode, up: bool, arg: P<Expr>) -> WithStmts<P<Expr>> {

        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(arg);

        let val_name = self.renamer.fresh();
        let save_old_val =
            mk().local_stmt(
                P(mk().local(mk().ident_pat(&val_name),
                             None as Option<P<Ty>>,
                             Some(deref_lhs.clone())))
            );

        let one = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        // *p + 1
        let val =
            if self.ast_context.resolve_type(ctype).is_pointer() {
                let n = if up { one } else { mk().unary_expr(UnOp::Neg, one) };
                mk().method_call_expr(deref_lhs.clone(), "offset", vec![n])
            } else {
                let k = if up { BinOpKind::Add } else { BinOpKind::Sub };
                mk().binary_expr(k, deref_lhs.clone(), one)
            };

        // *p = *p + rhs
        let assign_stmt = mk().assign_expr(&deref_lhs.clone(), val);

        lhs_stmts.push(save_old_val);
        lhs_stmts.push(mk().expr_stmt(assign_stmt));

        WithStmts {
            stmts: lhs_stmts,
            val: mk().ident_expr(val_name),
        }
    }

    pub fn convert_unary_operator(&mut self, name: &str, prefix: bool, ctype: TypeNode, ty: P<Ty>, arg: P<Expr>) -> WithStmts<P<Expr>> {
        match name {
            "&" => {
                let addr_of_arg = mk().mutbl().addr_of_expr(arg);
                let ptr = mk().cast_expr(addr_of_arg, ty);
                WithStmts::new(ptr)
            },
            "++" if prefix => self.convert_pre_increment(ctype, true, arg),
            "++" => self.convert_post_increment(ctype, true, arg),
            "--" if prefix => self.convert_pre_increment(ctype, false, arg),
            "--" => self.convert_post_increment(ctype, false, arg),
            "*" => WithStmts::new(mk().unary_expr(UnOp::Deref, arg)),
            "+" => WithStmts::new(arg), // promotion is explicit in the clang AST
            "-" => {
                let val = if self.ast_context.resolve_type(ctype).is_unsigned_integral_type() {
                    mk().method_call_expr(arg, "wrapping_neg", vec![] as Vec<P<Expr>>)
                } else {
                    mk().unary_expr(UnOp::Neg, arg)
                };
                WithStmts::new(val)
            }
            "~" => WithStmts::new(mk().unary_expr(UnOp::Not, arg)),
            "!" => WithStmts::new(self.convert_not(ctype, arg)),
            n => panic!("Unknown unary operator {}", n),
        }
    }

    pub fn convert_binary_operator(&mut self, name: &str, ty: P<Ty>, ctype: TypeNode, lhs_type: TypeNode, rhs_type: TypeNode, lhs: P<Expr>, rhs: P<Expr>) -> WithStmts<P<Expr>>
    {
        match name {

            "+" => WithStmts::new(self.convert_addition(lhs_type, rhs_type, lhs, rhs)),
            "-" => WithStmts::new(self.convert_subtraction(ty, lhs_type, rhs_type, lhs, rhs)),

            "*" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_mul"), vec![rhs])),
            "*" => WithStmts::new(mk().binary_expr(BinOpKind::Mul, lhs, rhs)),

            "/" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_div"), vec![rhs])),
            "/" => WithStmts::new(mk().binary_expr(BinOpKind::Div, lhs, rhs)),

            "%" if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_rem"), vec![rhs])),
            "%" => WithStmts::new(mk().binary_expr(BinOpKind::Rem, lhs, rhs)),

            "^" => WithStmts::new(mk().binary_expr(BinOpKind::BitXor, lhs, rhs)),

            ">>" => WithStmts::new(mk().binary_expr(BinOpKind::Shr, lhs, rhs)),

            "==" => WithStmts::new(mk().binary_expr(BinOpKind::Eq, lhs, rhs)).map(bool_to_int),
            "!=" => WithStmts::new(mk().binary_expr(BinOpKind::Ne, lhs, rhs)).map(bool_to_int),
            "<" => WithStmts::new(mk().binary_expr(BinOpKind::Lt, lhs, rhs)).map(bool_to_int),
            ">" => WithStmts::new(mk().binary_expr(BinOpKind::Gt, lhs, rhs)).map(bool_to_int),
            ">=" => WithStmts::new(mk().binary_expr(BinOpKind::Ge, lhs, rhs)).map(bool_to_int),
            "<=" => WithStmts::new(mk().binary_expr(BinOpKind::Le, lhs, rhs)).map(bool_to_int),

            "&" => WithStmts::new(mk().binary_expr(BinOpKind::BitAnd, lhs, rhs)),
            "|" => WithStmts::new(mk().binary_expr(BinOpKind::BitOr, lhs, rhs)),

            "+="  => self.convert_binary_assignment("+",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "-="  => self.convert_binary_assignment("-",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "*="  => self.convert_binary_assignment("*",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "/="  => self.convert_binary_assignment("/",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "%="  => self.convert_binary_assignment("%",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "^="  => self.convert_binary_assignment("^",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "<<=" => self.convert_binary_assignment("<<", ty, ctype, lhs_type, rhs_type, lhs, rhs),
            ">>=" => self.convert_binary_assignment(">>", ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "|="  => self.convert_binary_assignment("|",  ty, ctype, lhs_type, rhs_type, lhs, rhs),
            "&="  => self.convert_binary_assignment("&",  ty, ctype, lhs_type, rhs_type, lhs, rhs),

            "=" => self.convert_assignment(lhs, rhs),

            op => panic!("Unknown binary operator {}", op),
        }
    }

    fn convert_binary_assignment(&mut self, name: &str, ty: P<Ty>, ctype: TypeNode, lhs_type: TypeNode, rhs_type: TypeNode, lhs: P<Expr>, rhs: P<Expr>) -> WithStmts<P<Expr>> {

        // Improvements:
        // * Don't create block, use += for a statement
        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(lhs);
        // *p + rhs
        let mut val = self.convert_binary_operator(name, ty, ctype, lhs_type, rhs_type, deref_lhs.clone(), rhs);
        // *p = *p + rhs
        let assign_stmt = mk().assign_expr(&deref_lhs, val.val);

        lhs_stmts.append(&mut val.stmts);
        lhs_stmts.push(mk().expr_stmt(assign_stmt));

        WithStmts {
            stmts: lhs_stmts,
            val: deref_lhs,
        }
    }

    fn convert_addition(&mut self, lhs_type: TypeNode, rhs_type: TypeNode, lhs: P<Expr>, rhs: P<Expr>) -> P<Expr> {
        let lhs_type = self.ast_context.resolve_type(lhs_type);
        let rhs_type = self.ast_context.resolve_type(rhs_type);

        if lhs_type.is_pointer() {
            pointer_offset(lhs, rhs)
        } else if rhs_type.is_pointer() {
            pointer_offset(rhs, lhs)
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_add"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Add, lhs, rhs)
        }
    }

    fn convert_subtraction(&mut self, ty: P<Ty>, lhs_type: TypeNode, rhs_type: TypeNode, lhs: P<Expr>, rhs: P<Expr>) -> P<Expr> {
        let lhs_type = self.ast_context.resolve_type(lhs_type);
        let rhs_type = self.ast_context.resolve_type(rhs_type);

        if rhs_type.is_pointer() {
            // offset_to returns None when a pointer
            // offset_opt := rhs.offset_to(lhs)
            let offset_opt = mk().method_call_expr(rhs, "offset_to", vec![lhs]);
            // msg := "bad offset_to"
            let msg = mk().lit_expr(mk().str_lit("bad offset_to"));
            // offset := offset_opt.expect(msg)
            let offset = mk().method_call_expr(offset_opt, "expect", vec![msg]);
            mk().cast_expr(offset, ty)
        } else if lhs_type.is_pointer() {
            let neg_rhs = mk().unary_expr(UnOp::Neg, rhs);
            pointer_offset(lhs, neg_rhs)
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_sub"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Sub, lhs, rhs)
        }
    }

    fn convert_assignment(&mut self, lhs: P<Expr>, rhs: P<Expr>) -> WithStmts<P<Expr>> {
        // Improvements:
        // * Don't create block, use += for a statement

        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(lhs);

        // *p = rhs
        let assign_stmt = mk().expr_stmt(mk().assign_expr(&deref_lhs, rhs));
        lhs_stmts.push(assign_stmt);

        WithStmts {
            stmts: lhs_stmts,
            val: deref_lhs,
        }
    }

    /// Convert a boolean expression to a boolean for use in && or || or if
    fn to_bool(&self, ty: TypeNode, val: P<Expr>) -> P<Expr> {
        let ty = self.ast_context.resolve_type(ty);

        if ty.is_pointer() {
            mk().unary_expr(UnOp::Not, mk().method_call_expr(val, "is_null", vec![] as Vec<P<Expr>>))
        } else {
            let zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
            mk().binary_expr(BinOpKind::Ne, zero, val)
        }
    }

    /// Convert expression to c_int using '!' behavior
    fn convert_not(&self, ty: TypeNode, val: P<Expr>) -> P<Expr> {
        let ty = self.ast_context.resolve_type(ty);

        let b = if ty.is_pointer() {
            mk().method_call_expr(val, "is_null", vec![] as Vec<P<Expr>>)
        } else {
            let zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
            mk().binary_expr(BinOpKind::Eq, zero, val)
        };

        mk().cast_expr(b, mk().path_ty(vec!["libc","c_int"]))
    }
}
