
use syntax::ast;
use syntax::ast::*;
use syntax::tokenstream::{TokenStream};
use syntax::parse::token::{DelimToken,Token};
use syntax::abi::Abi;
use renamer::Renamer;
use convert_type::{TypeConverter, mk_qualified};
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
    type_converter: TypeConverter,
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

    /// Package a series of statements and an expression into one block expression
    pub fn to_expr(mut self) -> P<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            self.stmts.push(mk().expr_stmt(self.val));
            mk().block_expr(mk().block(self.stmts))
        }
    }

    /// Package a series of statements and an expression into one block
    pub fn to_block(mut self) -> P<Block> {
        self.stmts.push(mk().expr_stmt(self.val));
        mk().block(self.stmts)
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
            },

            CDeclKind::Typedef { ref name, ref typ } => {
                t.add_typedef(name, typ.ctype);
            },

            // XXX: Fill in other top-level declaration kinds
            _ => { },
        }
    }

    to_string(|s| {

        // Add `#[feature(libc)]` to the top of the file
        s.print_attribute(&mk().attribute::<_,TokenStream>(
            AttrStyle::Inner,
            vec!["feature"],
            vec![
                Token::OpenDelim(DelimToken::Paren),
                Token::Ident(mk().ident("libc")),
                Token::CloseDelim(DelimToken::Paren),
            ].into_iter().collect(),
        ))?;

        // Add `extern crate libc` to the top of the file
        s.print_item(&mk().extern_crate_item("libc", None))?;

        // Add the items accumulated
        for x in t.items.iter() {
            s.print_item(x)?;
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
                    let ty = self.convert_type(ty);
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
            .map(|&(ref var, ref typ)| {
                let rust_var = self.renamer.borrow_mut().insert(var.to_string(), var.as_str()).expect("Failed to insert argument");

                let ty = self.convert_type(typ.ctype);

                let pat = mk_qualified(&typ.qualifiers).ident_pat(rust_var);
                mk().arg(ty , pat)
            })
            .collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type.ctype));

        let decl = mk().fn_decl(args, ret);

        let block = self.convert_function_body(body);

        // End scope for function parameters
        self.renamer.borrow_mut().drop_scope();

        let function =mk()
            .single_attr("no_mangle")
            .vis(Visibility::Public)
            .unsafe_()
            .abi(Abi::C)
            .fn_item(name, decl, block);

        self.items.push(function);
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
                self.convert_expr(false, *expr).stmts
            },

            CStmtKind::Break => vec![mk().expr_stmt(mk().break_expr())],

            ref stmt => unimplemented!("convert_stmt {:?}", stmt),
        }
    }

    /// Convert a C expression to a rust boolean expression
    fn convert_condition(&self, target: bool, cond_id: CExprId) -> WithStmts<P<Expr>> {
        let ty_id = self.ast_context.index(cond_id).kind.get_type();

        self.convert_expr(true, cond_id)
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
            Some(i) => self.convert_expr(false,i).stmts,
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
        let val = result_id.map(|i| self.convert_expr(true,i));
        let mut ws = with_stmts_opt(val);
        let ret = mk().expr_stmt(mk().return_expr(ws.val));

        ws.stmts.push(ret);
        ws.stmts
    }

    fn convert_decl_stmt(&self, decl_id: CDeclId) -> Vec<Stmt> {

        match self.ast_context.index(decl_id).kind {
            CDeclKind::Variable { ref ident, ref initializer, ref typ } => {
                let rust_name = self.renamer.borrow_mut().insert(ident.clone(), &ident).unwrap();
                let pat = mk_qualified(&typ.qualifiers).ident_pat(rust_name);

                let init = with_stmts_opt(initializer.map(|x| self.convert_expr(true, x)));

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

    /// Write to a `lhs` that is volatile
    pub fn volatile_write(&self, lhs: &P<Expr>, lhs_type: CTypeId, rhs: P<Expr>) -> P<Expr> {

        let addr_lhs = match lhs.node {
            ExprKind::Unary(ast::UnOp::Deref, ref e) => e.clone(),
            _ => {
                let addr_lhs = mk().mutbl().addr_of_expr(lhs);

                let lhs_type = self.convert_type(lhs_type);
                let ty = mk().mutbl().ptr_ty(lhs_type);

                mk().cast_expr(addr_lhs, ty)
            },
        };

        mk().call_expr(mk().path_expr(vec!["std","ptr","write_volatile"]), vec![addr_lhs, rhs])
    }

    /// Read from a `lhs` that is volatile
    pub fn volatile_read(&self, lhs: &P<Expr>, lhs_type: CTypeId) -> P<Expr> {

        let addr_lhs = match lhs.node {
            ExprKind::Unary(ast::UnOp::Deref, ref e) => e.clone(),
            _ => {
                let addr_lhs = mk().addr_of_expr(lhs);

                let lhs_type = self.convert_type(lhs_type);
                let ty = mk().ptr_ty(lhs_type);

                mk().cast_expr(addr_lhs, ty)
            }
        };

        mk().call_expr(mk().path_expr(vec!["std","ptr","read_volatile"]), vec![addr_lhs])
    }

    /// The second argument indicates whether the resulting expression is used or not.
    ///
    /// In the case that it is not, all side-effecting components will be in the `stmts` field of
    /// the output and it is expected that the `val` field of the output will be ignored.
    fn convert_expr(&self, used: bool, expr_id: CExprId) -> WithStmts<P<Expr>> {

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

            CExprKind::ImplicitCast(ty, expr, kind) | CExprKind::ExplicitCast(ty, expr, kind) => {
                let val = self.convert_expr(true, expr);

                match kind {
                    CastKind::BitCast | CastKind::IntegralToPointer | CastKind::PointerToIntegral => {
                        val.map(|x| {
                            let source_ty = self.convert_type(self.ast_context.index(expr).kind.get_type());
                            let target_ty = self.convert_type(ty.ctype);
                            let type_args = vec![source_ty, target_ty];
                            let path = vec![
                                mk().path_segment("std"),
                                mk().path_segment("mem"),
                                mk().path_segment_with_params("transmute",
                                                              mk().angle_bracketed_param_types(type_args)),
                            ];
                            mk().call_expr(mk().path_expr(path), vec![x])
                        })
                    }

                    CastKind::IntegralCast | CastKind::FloatingCast | CastKind::FloatingToIntegral | CastKind::IntegralToFloating =>  {
                        let ty = self.convert_type(ty.ctype);
                        // this explicit use of paren_expr is to work around a bug in libsyntax
                        // Normally parentheses are added automatically as needed
                        // The library is rendering ''(x as uint) as < y'' as ''x as uint < y''
                        val.map(|x| mk().paren_expr(mk().cast_expr(x, ty)))
                    }

                    CastKind::LValueToRValue | CastKind::NoOp | CastKind::ToVoid => val,

                    CastKind::FunctionToPointerDecay => val,

                    CastKind::ArrayToPointerDecay =>
                        val.map(|x| mk().method_call_expr(x, "as_mut_ptr", vec![] as Vec<P<Expr>>)),

                    CastKind::NullToPointer => {
                        assert!(val.stmts.is_empty());
                        WithStmts::new(mk().call_expr(mk().path_expr(vec!["ptr", "null_mut"]), vec![] as Vec<P<Expr>>))
                    }

                    CastKind::ToUnion => panic!("TODO cast to union not supported"),

                    CastKind::IntegralToBoolean | CastKind::FloatingToBoolean |
                    CastKind::BooleanToSignedIntegral | CastKind::IntegralComplexToBoolean =>
                        panic!("TODO cast to boolean not supported"),

                    CastKind::FloatingRealToComplex | CastKind::FloatingComplexToIntegralComplex |
                    CastKind::FloatingComplexCast | CastKind::FloatingComplexToReal |
                    CastKind::IntegralComplexToReal | CastKind::IntegralRealToComplex |
                    CastKind::IntegralComplexCast | CastKind:: IntegralComplexToFloatingComplex =>
                        panic!("TODO casts with complex numbers not supported"),
                }
            }

            CExprKind::Unary(type_id, ref op, expr) => {
                let arg = self.convert_expr(true,expr);

                arg.and_then(|v| self.convert_unary_operator(used,*op, type_id, v))
            }

            CExprKind::Conditional(_, ref cond, lhs, rhs) => {
                let cond = self.convert_condition(true, *cond);

                let lhs = self.convert_expr(used, lhs);
                let rhs = self.convert_expr(used, rhs);

                if used {
                    let then: P<Block> = lhs.to_block();
                    let els: P<Expr> = rhs.to_expr();

                    cond.map(|c| mk().ifte_expr(c, then, Some(els)))
                } else {
                    // This node should _never_ show up in the final generated code. This is an easy
                    // way to notice if it does.
                    let panic = mk().mac_expr(mk().mac(vec!["panic"], vec![]));

                    let then: P<Block> = mk().block(lhs.stmts);
                    let els: P<Expr> = mk().block_expr(mk().block(rhs.stmts));

                    cond.and_then(|c| WithStmts {
                        stmts: vec![mk().semi_stmt(mk().ifte_expr(c, then, Some(els)))],
                        val: panic,
                    })
                }
            },

            CExprKind::Binary(ref type_id, ref op, lhs, rhs) => {

                match *op {
                    c_ast::BinOp::Comma => {

                        // The value of the LHS of a comma expression is always discarded
                        let lhs = self.convert_expr(false, lhs);
                        let rhs = self.convert_expr(used, rhs);

                        WithStmts {
                            stmts: lhs.stmts.into_iter().chain(rhs.stmts).collect(),
                            val: rhs.val,
                        }
                    }

                    c_ast::BinOp::And => {
                        // XXX: do we need the RHS to always be used?
                        let lhs = self.convert_expr(true, lhs);
                        let rhs = self.convert_expr(true, rhs);

                        lhs.map(|x| mk().binary_expr(BinOpKind::And, x, rhs.to_expr()))
                    }

                    c_ast::BinOp::Or => {
                        // XXX: do we need the RHS to always be used?
                        let lhs = self.convert_expr(true,lhs);
                        let rhs = self.convert_expr(true,rhs);

                        lhs.map(|x| mk().binary_expr(BinOpKind::Or, x, rhs.to_expr()))
                    }

                    // No sequence-point cases
                    _ => {

                        let lhs_ty = self.ast_context.index(lhs).kind.get_qual_type();
                        let rhs_ty = self.ast_context.index(rhs).kind.get_qual_type();

                        let ty = self.convert_type(type_id.ctype);

                        let lhs = self.convert_expr(true,lhs,);
                        let rhs = self.convert_expr(true,rhs,);

                        let bin =
                            self.convert_binary_operator(*op, ty, type_id.ctype, lhs_ty, rhs_ty, lhs.val, rhs.val);

                        WithStmts {
                            stmts: lhs.stmts.into_iter().chain(rhs.stmts).chain(bin.stmts).collect(),
                            val: bin.val,
                        }
                    }
                }
            }

            CExprKind::ArraySubscript(_, ref lhs, ref rhs) => {
                let lhs_ty = self.ast_context.index(*lhs).kind.get_type();
                let lhs_ty = &self.ast_context.resolve_type(lhs_ty).kind;

                let lhs = self.convert_expr(true, *lhs);
                let rhs = self.convert_expr(true, *rhs);

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

            CExprKind::Call(_, ref func, ref args) => {
                let moved_args = args;
                let mut stmts = vec![];

                let func = {
                    let WithStmts { stmts: ss, val } = self.convert_expr(true, *func);
                    stmts.extend(ss);
                    val
                };

                let mut args_new: Vec<P<Expr>> = vec![];
                for arg in moved_args {
                    let WithStmts { stmts: ss, val } = self.convert_expr(true, *arg);
                    stmts.extend(ss);
                    args_new.push(val);
                }

                let call_expr = mk().call_expr(func, args_new);


                if used {
                    WithStmts { stmts, val: call_expr }
                } else {
                    // Recall that if `used` is false, the `stmts` field of the output must contain
                    // all side-effects (and a function call can always have side-effects)
                    stmts.push(mk().semi_stmt(call_expr));

                    // This node should _never_ show up in the final generated code. This is an easy
                    // way to notice if it does.
                    let panic = mk().mac_expr(mk().mac(vec!["panic"], vec![]));

                    WithStmts { stmts, val: panic }
                }
            }

            CExprKind::Member(_, expr, decl, kind) => {
                let struct_val = self.convert_expr(used, expr);
                let field_name = self.ast_context.index(decl).kind.get_name().expect("expected field name");

                if used {
                    struct_val.map(|v| {
                        let v = match kind {
                            MemberKind::Arrow => mk().unary_expr(ast::UnOp::Deref, v),
                            MemberKind::Dot => v,
                        };
                        mk().field_expr(v, field_name)}
                    )
                } else {
                    struct_val
                }
            }

            CExprKind::InitList(ty, ref ids) => {
                let resolved = &self.ast_context.resolve_type(ty.ctype).kind;

                if let &CTypeKind::ConstantArray(ty, n) = resolved {

                    // Convert all of the provided initializer values
                    let mut stmts: Vec<Stmt> = vec![];
                    let mut vals: Vec<P<Expr>> = vec![];
                    for v in ids {
                        let mut x = self.convert_expr(true, *v);
                        stmts.append(&mut x.stmts);
                        vals.push(x.val);
                    }

                    // Pad out the array literal with default values to the desired size
                    for _i in ids.len() .. n {
                        vals.push(self.implicit_default_expr(ty))
                    }

                    WithStmts {
                        stmts,
                        val: mk().array_expr(vals),
                    }
                } else {
                    panic!("Init list not implemented for structs");
                }
            }
            CExprKind::ImplicitValueInit(ref ty) =>
                WithStmts::new(self.implicit_default_expr(ty.ctype)),
        }
    }

    pub fn implicit_default_expr(&self, ty_id: CTypeId) -> P<Expr> {
        let resolved_ty = &self.ast_context.resolve_type(ty_id).kind;

        if resolved_ty.is_integral_type() {
            mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed))
        } else if resolved_ty.is_floating_type() {
            mk().lit_expr(mk().float_unsuffixed_lit("0."))
        } else {
            mk().call_expr(mk().ident_expr("default"), vec![] as Vec<P<Expr>>)
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

    pub fn convert_post_increment(&self, used: bool, ctype: CTypeId, up: bool, arg: P<Expr>) -> WithStmts<P<Expr>> {

        // If we aren't going to be using the result, may as well do a simple pre-increment
        if !used {
            return self.convert_pre_increment(ctype, up, arg)
        }

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

    pub fn convert_unary_operator(&self, used: bool, name: c_ast::UnOp, cqual_type: CQualTypeId, arg: P<Expr>) -> WithStmts<P<Expr>> {

        let CQualTypeId { ctype, qualifiers } = cqual_type;
        let ty = self.convert_type(ctype);

        match name {
            c_ast::UnOp::AddressOf => {

                // In this translation, there are only pointers to functions and
                // & becomes a no-op when applied to a function.

                let is_function_pointer =
                if let CTypeKind::Pointer(p) = self.ast_context.resolve_type(ctype).kind {
                    if let CTypeKind::Function{..} = self.ast_context.resolve_type(p.ctype).kind {
                        true
                    } else { false }
                } else { false };

                if is_function_pointer {
                    WithStmts::new(arg)
                } else {
                    // TODO: Only make mutable if required by the target type
                    let addr_of_arg = mk().mutbl().addr_of_expr(arg);
                    let ptr = mk().cast_expr(addr_of_arg, ty);
                    WithStmts::new(ptr)
                }
            },
            c_ast::UnOp::PreIncrement => self.convert_pre_increment(ctype,true, arg),
            c_ast::UnOp::PreDecrement => self.convert_pre_increment(ctype,false, arg),
            c_ast::UnOp::PostIncrement => self.convert_post_increment(used, ctype,true, arg),
            c_ast::UnOp::PostDecrement => self.convert_post_increment(used, ctype,false, arg),
            c_ast::UnOp::Deref => {
                // This should be a `self.volatile_read` if the type on the other side of the pointer
                // is volatile, _and_ if it is used as an rvalue
                WithStmts::new(mk().unary_expr(ast::UnOp::Deref, arg))
            },
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
        ctype: CTypeId,
        lhs_type: CQualTypeId,
        rhs_type: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> WithStmts<P<Expr>> {

        if let Some(op) = op.underlying_assignment() {
            // Handle compound assignment binary operators

            // Improvements:
            // * Don't create block, use += for a statement
            let WithStmts{ val: deref_lhs, stmts: lhs_stmts } = self.name_reference(lhs);
            // *p + rhs
            let mut val = self.convert_binary_operator(op, ty, ctype, lhs_type, rhs_type, deref_lhs.clone(), rhs);
            // *p = *p + rhs

            let assign_stmt = if lhs_type.qualifiers.is_volatile {
                self.volatile_write(&deref_lhs, lhs_type.ctype, val.val)
            } else {
                mk().assign_expr(&deref_lhs, val.val)
            };

            let mut stmts = lhs_stmts;
            stmts.append(&mut val.stmts);
            stmts.push(mk().expr_stmt(assign_stmt));

            let val = if lhs_type.qualifiers.is_volatile {
                self.volatile_read(&deref_lhs, lhs_type.ctype)
            } else {
                deref_lhs
            };

            WithStmts { stmts, val }
        } else {
            // Handle all other binary operators

            let ctype = &self.ast_context.index(ctype).kind;
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

                c_ast::BinOp::Assign => self.convert_assignment(lhs, lhs_type, rhs),

                op => unimplemented!("Translation of binary operator {:?}", op),
            }
        }
    }

    fn convert_addition(
        &self,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

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
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

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

    fn convert_assignment(&self, lhs: P<Expr>, lhs_type: CQualTypeId, rhs: P<Expr>) -> WithStmts<P<Expr>> {
        // Improvements:
        // * Don't create block, use += for a statement

        let WithStmts{ val: deref_lhs, stmts: lhs_stmts } = self.name_reference(lhs);

        let assign_stmt = if lhs_type.qualifiers.is_volatile {
            self.volatile_write(&deref_lhs, lhs_type.ctype, rhs)
        } else {
            mk().assign_expr(&deref_lhs, rhs)
        };

        let mut stmts = lhs_stmts;
        stmts.push(mk().expr_stmt(assign_stmt));

        let val = if lhs_type.qualifiers.is_volatile {
            self.volatile_read(&deref_lhs, lhs_type.ctype)
        } else {
            deref_lhs
        };

        WithStmts { stmts, val }
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
