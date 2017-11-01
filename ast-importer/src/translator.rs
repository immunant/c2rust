
use syntax::ast;
use syntax::ast::*;
use renamer::Renamer;
use convert_type::TypeConverter;
use idiomize::ast_manip::make_ast::*;
use c_ast;
use c_ast::*;
use syntax::ptr::*;
use syntax::print::pprust::*;
use std::collections::HashSet;
use std::ops::Index;
use std::cell::RefCell;

pub struct Translation {
    pub items: Vec<P<Item>>,
    pub type_converter: TypeConverter,
    pub ast_context: TypedAstContext,
    renamer: RefCell<Renamer<String>>,
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

pub fn translate(ast_context: &TypedAstContext) -> String {
    use c_ast::*;
    let mut t = Translation::new(ast_context.clone());

    // Populate renamer with top-level names
    for top_id in &ast_context.c_decls_top {
        if let Some(y) = ast_context.index(*top_id).kind.get_name() {
            t.renamer.borrow_mut().insert(y.to_owned(), &y);
        }
    }

    for top_id in &ast_context.c_decls_top {

        match ast_context.index(*top_id).kind {
            CDeclKind::Function { ref typ, ref name, ref parameters, ref body } => {

                let ret: CQualTypeId = match ast_context.index(*typ).kind {
                    CTypeKind::Function(ret, _) => ret,
                    _ => panic!("Type of function {:?} was not a function type", *top_id)
                };

                let args: Vec<(String, CQualTypeId)> = parameters
                    .iter()
                    .map(|param_id  | {
                        if let CDeclKind::Variable { ref ident, ref typ, .. } = ast_context.index(*param_id).kind {
                            (ident.clone(), *typ)
                        } else {
                            panic!("Parameter is not variable declaration")
                        }
                    })
                    .collect();

                t.add_function(name, &args, ret, *body);
            }

            /// XXX: Fill in other top-level declaration kinds
            _ => { },
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
    pub fn new(ast_context: TypedAstContext) -> Translation {
        Translation {
            items: vec![],
            type_converter: TypeConverter::new(),
            ast_context,
            renamer: RefCell::new(Renamer::new(HashSet::new())),
            // XXX: Populate reserved words
        }
    }

    pub fn add_struct(&mut self, name: Ident, fields: &[(&str, CTypeId)]) {
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

    pub fn add_typedef(&mut self, name: &str, typeid: CTypeId) {
        let ty = self.convert_type(typeid);
        let item = mk().type_item(name, ty);
        self.items.push(item);
    }

    pub fn add_function(&mut self, name: &str, arguments: &[(String, CQualTypeId)], return_type: CQualTypeId, body: CStmtId) {
        // Start scope for function parameters
        self.renamer.borrow_mut().add_scope();

        let args: Vec<Arg> = arguments
            .iter()
            .map(|&(ref var, CQualTypeId { ref qualifiers, ref ctype })| {
                let rust_var = self.renamer.borrow_mut().insert(var.to_string(), var.as_str()).expect("Failed to insert argument");

                mk().arg(self.convert_type(*ctype), mk().mutbl().ident_pat(rust_var))
            })
            .collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type.ctype));

        let decl = mk().fn_decl(args, ret);

        let block = self.convert_function_body(body);

        // End scope for function parameters
        self.renamer.borrow_mut().drop_scope();

        self.items.push(mk().unsafe_().fn_item(name, decl, block));
    }

    fn convert_function_body(&self, body_id: CStmtId) -> P<Block> {

        // Open function body scope
        self.renamer.borrow_mut().add_scope();

        let stmts = match self.ast_context.index(body_id).kind {
            CStmtKind::Compound(ref stmts) => stmts
                .iter()
                .flat_map(|stmt| self.convert_stmt(*stmt))
                .collect(),
            _ => panic!("function body expects to be a compound statement"),
        };

        // Close function body scope
        self.renamer.borrow_mut().drop_scope();

        stmts_block(stmts)
    }

    fn convert_stmt(&self, stmt_id: CStmtId) -> Vec<Stmt> {
        match self.ast_context.index(stmt_id).kind {
            CStmtKind::Empty => vec![],

            CStmtKind::Decls(ref decls) => {
                decls
                    .iter()
                    .flat_map(|decl| self.convert_decl_stmt(*decl))
                    .collect()
            },

            CStmtKind::Return(ref expr) => {
                self.convert_return_stmt(*expr)
            },

            CStmtKind::If { ref scrutinee, ref true_variant, ref false_variant } => {
                self.convert_if_stmt(*scrutinee, *true_variant, *false_variant)
            },

            CStmtKind::While { ref condition, ref body } => {
                self.convert_while_stmt(*condition, *body)
            },

            CStmtKind::DoWhile { ref body, ref condition } =>
                self.convert_do_stmt(*body, *condition),

            CStmtKind::ForLoop { ref init, ref condition, ref increment, ref body } => {
                self.convert_for_stmt(*init, *condition, *increment, *body)
            },

            CStmtKind::Compound(ref stmts) => {
                self.renamer.borrow_mut().add_scope();

                let stmts = stmts
                    .iter()
                    .flat_map(|stmt| self.convert_stmt(*stmt))
                    .collect();

                self.renamer.borrow_mut().drop_scope();

                vec![mk().expr_stmt(mk().block_expr(stmts_block(stmts)))]
            },

            CStmtKind::Expr(ref expr) => {
                let mut xs = self.convert_expr(*expr);
                xs.stmts.push(mk().expr_stmt(xs.val));
                xs.stmts
            },

            ref stmt => unimplemented!("convert_stmt {:?}", stmt),
        }
    }

    /// Convert a C expression to a rust boolean expression
    fn convert_condition(&self, target: bool, cond_id: CExprId) -> WithStmts<P<Expr>> {
        let ty_id = self.ast_context.index(cond_id).kind.get_type();

        self.convert_expr(cond_id)
            .map(|e| self.match_bool(target, ty_id, e))
    }

    fn convert_while_stmt(&self, cond_id: CExprId, body_id: CStmtId) -> Vec<Stmt> {

        let cond = self.convert_condition(true, cond_id);
        let body = self.convert_stmt(body_id);

        let rust_cond = cond.to_expr();
        let rust_body = stmts_block(body);

        vec![mk().expr_stmt(mk().while_expr(rust_cond, rust_body))]
    }

    fn convert_do_stmt(&self, body_id: CStmtId, cond_id: CExprId) -> Vec<Stmt> {
        let cond = self.convert_condition(false, cond_id);
        let mut body = self.convert_stmt(body_id);

        let rust_cond = cond.to_expr();
        let break_stmt = mk().semi_stmt(mk().break_expr());

        body.push(mk().expr_stmt(mk().ifte_expr(rust_cond, mk().block(vec![break_stmt]), None as Option<P<Expr>>)));

        let rust_body = stmts_block(body);

        vec![mk().semi_stmt(mk().loop_expr(rust_body))]
    }

    fn convert_for_stmt(
        &self,
        init_id: Option<CStmtId>,
        cond_id: Option<CExprId>,
        inc_id: Option<CExprId>,
        body_id: CStmtId,
    ) -> Vec<Stmt> {

        self.renamer.borrow_mut().add_scope();

        let mut init = match init_id {
          Some(i) => self.convert_stmt(i),
          None => vec![],
        };

        let mut inc = match inc_id {
            Some(i) => self.convert_expr(i).stmts,
            None => vec![],
        };

        let mut body = self.convert_stmt(body_id);
        body.append(&mut inc);

        let body_block = stmts_block(body);

        let looper = match cond_id {
            None => mk().loop_expr(body_block), // loop
            Some(i) => mk().while_expr(self.convert_condition(true, i).to_expr(), body_block), // while
        };

        init.push(mk().expr_stmt(looper));

        self.renamer.borrow_mut().drop_scope();

        vec![mk().expr_stmt(mk().block_expr(mk().block(init)))]

    }

    fn convert_if_stmt(
        &self,
        cond_id: CExprId,
        then_id: CStmtId,
        else_id: Option<CStmtId>
    ) -> Vec<Stmt> {
        let mut cond = self.convert_condition(true, cond_id);
        let then_stmts = stmts_block(self.convert_stmt(then_id));
        let else_stmts = else_id.map(|x| { mk().block_expr(stmts_block(self.convert_stmt(x)))});

        cond.stmts.push(mk().semi_stmt(mk().ifte_expr(cond.val, then_stmts, else_stmts)));
        cond.stmts
    }

    fn convert_return_stmt(&self, result_id: Option<CExprId>) -> Vec<Stmt> {
        let val = result_id.map(|i| self.convert_expr(i));
        let mut ws = with_stmts_opt(val);
        let ret = mk().expr_stmt(mk().return_expr(ws.val));

        ws.stmts.push(ret);
        ws.stmts
    }

    fn convert_decl_stmt(&self, decl_id: CDeclId) -> Vec<Stmt> {

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { ref ident, ref initializer, ref typ } => {
                let rust_name = self.renamer.borrow_mut().insert(ident.clone(), &ident).unwrap();
                let pat = mk().mutbl().ident_pat(rust_name);
                let init = with_stmts_opt(initializer.map(|x| self.convert_expr(x)));
                let ty = self.convert_type(typ.ctype);
                let local = mk().local(pat, Some(ty), init.val);

                let mut stmts = init.stmts;
                stmts.push(mk().local_stmt(P(local)));
                stmts
            }

            ref t => panic!("Declaration not implemented {:?}", t),
        }
    }

    fn convert_type(&self, type_id: CTypeId) -> P<Ty> {
        self.type_converter.convert(&self.ast_context, type_id)
    }

    fn convert_expr(&self, expr_id: CExprId) -> WithStmts<P<Expr>> {

        match self.ast_context.index(expr_id).kind {

            CExprKind::DeclRef(_, ref decl_id) => {
                let varname = self.ast_context.index(*decl_id).kind.get_name().expect("expected variable name").to_owned();
                let rustname = self.renamer.borrow_mut().get(varname).expect("name not declared");

                WithStmts::new(mk().path_expr(vec![rustname]))
            }

            CExprKind::Literal(_, CLiteral::Integer(ref val)) => {
                let val: u64 = *val;
                WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed)))
            }

            CExprKind::Literal(_, CLiteral::Character(ref val)) => {
                let val: u64 = *val;
                WithStmts::new(mk().lit_expr(mk().int_lit(val.into(), LitIntType::Unsuffixed)))
            }

            CExprKind::Literal(_, CLiteral::Floating(ref val)) => {
                let str = format!("{}", val);
                WithStmts::new(mk().lit_expr(mk().float_unsuffixed_lit(str)))
            }

            CExprKind::ImplicitCast(_, ref expr) => {
                // TODO actually cast
                // Numeric casts with 'as', pointer casts with transmute
                self.convert_expr(*expr)
            }

            CExprKind::Unary(ref type_id, ref op, ref prefix, ref expr) => {
                let arg = self.convert_expr(*expr);
                let ty = self.convert_type(*type_id);

                arg.and_then(|v| self.convert_unary_operator(*op, *prefix, *type_id, ty, v))
            }

            CExprKind::Binary(ref ty, ref op, ref lhs, ref rhs) => {

                let lhs_node = self.ast_context.index(*lhs);
                let rhs_node = self.ast_context.index(*rhs);

                let lhs_ty = lhs_node.kind.get_type();
                let rhs_ty = rhs_node.kind.get_type();

                let cty = &self.ast_context.index(*ty).kind;
                let ty = self.convert_type(*ty);

                let lhs = self.convert_expr(*lhs);
                let rhs = self.convert_expr(*rhs);

                match *op {
                    c_ast::BinOp::Comma => {
                        let mut stmts = vec![];
                        stmts.extend(lhs.stmts);
                        stmts.push(mk().expr_stmt(lhs.val));
                        stmts.extend(rhs.stmts);

                        let val = rhs.val;

                        WithStmts { stmts, val }
                    }

                    c_ast::BinOp::And =>
                        lhs.map(|x| mk().binary_expr(BinOpKind::And, x, rhs.to_expr())),

                    c_ast::BinOp::Or =>
                        lhs.map(|x| mk().binary_expr(BinOpKind::Or, x, rhs.to_expr())),

                    // No sequence-point cases
                    _ => {
                        let bin =
                            self.convert_binary_operator(*op, ty, cty, lhs_ty, rhs_ty, lhs.val, rhs.val);

                        WithStmts {
                            stmts: lhs.stmts.into_iter().chain(rhs.stmts).chain(bin.stmts).collect(),
                            val: bin.val,
                        }
                    }
                }
            }

            CExprKind::ArraySubscript(ref ty, ref lhs, ref rhs) => {
                let lhs_ty = self.ast_context.index(*lhs).kind.get_type();
                let lhs_ty = &self.ast_context.resolve_type(lhs_ty).kind;

                let lhs = self.convert_expr(*lhs);
                let rhs = self.convert_expr(*rhs);

                let val = if lhs_ty.is_pointer() {
                    pointer_offset(lhs.val, rhs.val)
                } else {
                    pointer_offset(rhs.val, lhs.val)
                };

                let val = mk().unary_expr(ast::UnOp::Deref, val);

                let mut stmts = vec![];
                stmts.extend(lhs.stmts);
                stmts.extend(rhs.stmts);

                WithStmts { stmts, val }
            }

            CExprKind::Call(ref ty, ref func, ref args) => {
                let moved_args = args;
                let mut stmts = vec![];

                let func = {
                    let WithStmts { stmts: ss, val } = self.convert_expr(*func);
                    stmts.extend(ss);
                    val
                };

                let mut args_new: Vec<P<Expr>> = vec![];
                for arg in moved_args {
                    let WithStmts { stmts: ss, val } = self.convert_expr(*arg);
                    stmts.extend(ss);
                    args_new.push(val);
                }

                WithStmts {
                    stmts,
                    val: mk().call_expr(func, args_new),
                }
            }

            CExprKind::Member(_, ref expr, ref decl) => {
                let struct_val = self.convert_expr(*expr);
                let field_name = self.ast_context.index(*decl).kind.get_name().expect("expected field name");

                struct_val.map(|v| mk().field_expr(v, field_name))
            }

            ref t => panic!("Expression not implemented {:?}", t),
        }
    }

    pub fn name_reference(&self, reference: P<Expr>) -> WithStmts<P<Expr>> {
        fn is_path_expr(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path{..} => true,
                _ => false,
            }
        }

        let is_simple = match &reference.node {
            &ExprKind::Path{..} => true,
            &ExprKind::Unary(ast::UnOp::Deref, ref e) => is_path_expr(e),
            _ => false,
        };

        if is_simple {
            WithStmts::new(reference)
        } else {
            let ptr_name = self.renamer.borrow_mut().fresh();
            // let ref mut p = lhs;
            let compute_ref =
                mk().local_stmt(
                    P(mk().local(mk().mutbl().ident_ref_pat(&ptr_name),
                                 None as Option<P<Ty>>,
                                 Some(reference)))
                );
            WithStmts {
                val: mk().unary_expr(ast::UnOp::Deref, mk().ident_expr(&ptr_name)),
                stmts: vec![compute_ref],
            }
        }
    }

    pub fn convert_pre_increment(&self, ctype: CTypeId, up: bool, arg: P<Expr>) -> WithStmts<P<Expr>> {

        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(arg);

        let one = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        // *p + 1
        let val =
            if self.ast_context.resolve_type(ctype).kind.is_pointer() {
                // This calls the offset with a number literal directly, and doesn't need
                // the cast that the pointer_offset function adds
                let n = if up { one } else { mk().unary_expr(ast::UnOp::Neg, one) };
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

    pub fn convert_post_increment(&self, ctype: CTypeId, up: bool, arg: P<Expr>) -> WithStmts<P<Expr>> {

        let WithStmts{ val: deref_lhs, stmts: mut lhs_stmts } = self.name_reference(arg);

        let val_name = self.renamer.borrow_mut().fresh();
        let save_old_val =
            mk().local_stmt(
                P(mk().local(mk().ident_pat(&val_name),
                             None as Option<P<Ty>>,
                             Some(deref_lhs.clone())))
            );

        let one = mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed));
        // *p + 1
        let val =
            if self.ast_context.resolve_type(ctype).kind.is_pointer() {
                let n = if up { one } else { mk().unary_expr(ast::UnOp::Neg, one) };
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

    pub fn convert_unary_operator(&self, name: c_ast::UnOp, prefix: bool, ctype: CTypeId, ty: P<Ty>, arg: P<Expr>) -> WithStmts<P<Expr>> {
        match name {
            c_ast::UnOp::AddressOf => {
                let addr_of_arg = mk().mutbl().addr_of_expr(arg);
                let ptr = mk().cast_expr(addr_of_arg, ty);
                WithStmts::new(ptr)
            },
            c_ast::UnOp::Increment if prefix => self.convert_pre_increment(ctype, true, arg),
            c_ast::UnOp::Increment => self.convert_post_increment(ctype, true, arg),
            c_ast::UnOp::Decrement if prefix => self.convert_pre_increment(ctype, false, arg),
            c_ast::UnOp::Decrement => self.convert_post_increment(ctype, false, arg),
            c_ast::UnOp::Deref => WithStmts::new(mk().unary_expr(ast::UnOp::Deref, arg)),
            c_ast::UnOp::Plus => WithStmts::new(arg), // promotion is explicit in the clang AST
            c_ast::UnOp::Negate => {
                let val = if self.ast_context.resolve_type(ctype).kind.is_unsigned_integral_type() {
                    mk().method_call_expr(arg, "wrapping_neg", vec![] as Vec<P<Expr>>)
                } else {
                    mk().unary_expr(ast::UnOp::Neg, arg)
                };
                WithStmts::new(val)
            }
            c_ast::UnOp::Complement => WithStmts::new(mk().unary_expr(ast::UnOp::Not, arg)),
            c_ast::UnOp::Not => WithStmts::new(self.convert_not(ctype, arg)),
        }
    }

    pub fn convert_binary_operator(
        &self,
        op: c_ast::BinOp,
        ty: P<Ty>,
        ctype: &CTypeKind,
        lhs_type: CTypeId,
        rhs_type: CTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> WithStmts<P<Expr>> {
        match op {
            c_ast::BinOp::Add => WithStmts::new(self.convert_addition(lhs_type, rhs_type, lhs, rhs)),
            c_ast::BinOp::Subtract => WithStmts::new(self.convert_subtraction(ty, lhs_type, rhs_type, lhs, rhs)),

            c_ast::BinOp::Multiply if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_mul"), vec![rhs])),
            c_ast::BinOp::Multiply => WithStmts::new(mk().binary_expr(BinOpKind::Mul, lhs, rhs)),

            c_ast::BinOp::Divide if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_div"), vec![rhs])),
            c_ast::BinOp::Divide => WithStmts::new(mk().binary_expr(BinOpKind::Div, lhs, rhs)),

            c_ast::BinOp::Modulus if ctype.is_unsigned_integral_type() =>
                WithStmts::new(mk().method_call_expr(lhs, mk().path_segment("wrapping_rem"), vec![rhs])),
            c_ast::BinOp::Modulus => WithStmts::new(mk().binary_expr(BinOpKind::Rem, lhs, rhs)),

            c_ast::BinOp::BitXor => WithStmts::new(mk().binary_expr(BinOpKind::BitXor, lhs, rhs)),

            c_ast::BinOp::ShiftRight => WithStmts::new(mk().binary_expr(BinOpKind::Shr, lhs, rhs)),

            c_ast::BinOp::EqualEqual => WithStmts::new(mk().binary_expr(BinOpKind::Eq, lhs, rhs)).map(bool_to_int),
            c_ast::BinOp::NotEqual => WithStmts::new(mk().binary_expr(BinOpKind::Ne, lhs, rhs)).map(bool_to_int),
            c_ast::BinOp::Less => WithStmts::new(mk().binary_expr(BinOpKind::Lt, lhs, rhs)).map(bool_to_int),
            c_ast::BinOp::Greater => WithStmts::new(mk().binary_expr(BinOpKind::Gt, lhs, rhs)).map(bool_to_int),
            c_ast::BinOp::GreaterEqual => WithStmts::new(mk().binary_expr(BinOpKind::Ge, lhs, rhs)).map(bool_to_int),
            c_ast::BinOp::LessEqual => WithStmts::new(mk().binary_expr(BinOpKind::Le, lhs, rhs)).map(bool_to_int),

            c_ast::BinOp::BitAnd => WithStmts::new(mk().binary_expr(BinOpKind::BitAnd, lhs, rhs)),
            c_ast::BinOp::BitOr => WithStmts::new(mk().binary_expr(BinOpKind::BitOr, lhs, rhs)),

            c_ast::BinOp::AssignAdd  => self.convert_binary_assignment(c_ast::BinOp::Add,              ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignSubtract => self.convert_binary_assignment(c_ast::BinOp::Subtract,     ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignMultiply => self.convert_binary_assignment(c_ast::BinOp::Multiply,     ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignDivide => self.convert_binary_assignment(c_ast::BinOp::Divide,         ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignModulus => self.convert_binary_assignment(c_ast::BinOp::Modulus,       ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignBitXor => self.convert_binary_assignment(c_ast::BinOp::BitXor,         ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignShiftLeft => self.convert_binary_assignment(c_ast::BinOp::ShiftLeft,   ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignShiftRight => self.convert_binary_assignment(c_ast::BinOp::ShiftRight, ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignBitOr => self.convert_binary_assignment(c_ast::BinOp::BitOr,           ty, ctype, lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::AssignBitAnd => self.convert_binary_assignment(c_ast::BinOp::BitAnd,         ty, ctype, lhs_type, rhs_type, lhs, rhs),

            c_ast::BinOp::Assign => self.convert_assignment(lhs, rhs),

            op => unimplemented!("Translation of binary operator {:?}", op),
        }
    }

    fn convert_binary_assignment(
        &self,
        name: c_ast::BinOp,
        ty: P<Ty>,
        ctype: &CTypeKind,
        lhs_type: CTypeId,
        rhs_type: CTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> WithStmts<P<Expr>> {

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

    fn convert_addition(
        &self,
        lhs_type_id: CTypeId,
        rhs_type_id: CTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id).kind;

        if lhs_type.is_pointer() {
            pointer_offset(lhs, rhs)
        } else if rhs_type.is_pointer() {
            pointer_offset(lhs, rhs)
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_add"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Add, lhs, rhs)
        }
    }

    fn convert_subtraction(
        &self,
        ty: P<Ty>,
        lhs_type_id: CTypeId,
        rhs_type_id: CTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id).kind;

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
            let neg_rhs = mk().unary_expr(ast::UnOp::Neg, rhs);
            pointer_offset(lhs, neg_rhs)
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_sub"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Sub, lhs, rhs)
        }
    }

    fn convert_assignment(&self, lhs: P<Expr>, rhs: P<Expr>) -> WithStmts<P<Expr>> {
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
    fn match_bool(&self, target: bool, ty_id: CTypeId, val: P<Expr>) -> P<Expr> {
        let ty = &self.ast_context.resolve_type(ty_id).kind;

        if ty.is_pointer() {
            let mut res = mk().method_call_expr(val, "is_null", vec![] as Vec<P<Expr>>);
            if target {
                res = mk().unary_expr(ast::UnOp::Not, res)
            }
            res
        } else {
            let zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
            if target {
                mk().binary_expr(BinOpKind::Ne, zero, val)
            } else {
                mk().binary_expr(BinOpKind::Eq, zero, val)
            }
        }
    }

    /// Convert expression to c_int using '!' behavior
    fn convert_not(&self, ty_id: CTypeId, val: P<Expr>) -> P<Expr> {
        let ty = &self.ast_context.resolve_type(ty_id).kind;

        let b = if ty.is_pointer() {
            mk().method_call_expr(val, "is_null", vec![] as Vec<P<Expr>>)
        } else {
            let zero = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
            mk().binary_expr(BinOpKind::Eq, zero, val)
        };

        mk().cast_expr(b, mk().path_ty(vec!["libc","c_int"]))
    }
}
