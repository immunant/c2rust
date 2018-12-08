use super::*;

#[derive(Copy, Clone, Debug)]
pub enum VaPart {
    Start(CDeclId),
    End(CDeclId),
    Copy,
}

macro_rules! match_or {
    ([$e:expr] $p:pat => $r:tt) => {
        let $r = match $e { $p => $r, _ => return None };
    };
    ([$e:expr] $p:pat if $g:expr => $r:tt) => {
        let $r = match $e { $p if $g => $r, _ => return None };
    };
}

impl<'c> Translation<'c> {

    /// Install a fake variable into the renamer as a kludge until we have
    /// proper variadic function definition support
    pub fn register_va_arg(&self, decl_id: CDeclId) {

        match self.ast_context[decl_id].kind {
            CDeclKind::Variable { ref ident, .. } => {
                self.renamer.borrow_mut()
                        .insert(decl_id, ident)
                        .expect(&format!("Failed to install variadic function kludge"));
            }
            _ => panic!("va_arg was not a variable"),
        }
    }

    pub fn match_vastart(&self, expr: CExprId) -> Option<CDeclId> {
        match_or! { [self.ast_context[expr].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }
        match_or! { [self.ast_context[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }
        Some(va_id)
    }

    pub fn match_vaend(&self, expr: CExprId) -> Option<CDeclId> {
        match_or! { [self.ast_context[expr].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }
        match_or! { [self.ast_context[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }
        Some(va_id)
    }

    pub fn match_vapart(&self, expr: CExprId) -> Option<VaPart> {
        match_or! { [self.ast_context[expr].kind]
            CExprKind::Call(_, func, ref args) => (func, args) }
        match_or! { [self.ast_context[func].kind]
            CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) => fexp }
        match_or! { [self.ast_context[fexp].kind]
            CExprKind::DeclRef(_, decl_id, _) => decl_id }
        match_or! { [self.ast_context[decl_id].kind]
            CDeclKind::Function { ref name, .. } => name }
        match name as &str {
            "__builtin_va_start" => {
                if args.len() != 2 { return None }
                self.match_vastart(args[0]).map(VaPart::Start)
            }

            "__builtin_va_copy" => Some(VaPart::Copy),

            "__builtin_va_end" => {
                if args.len() != 1 { return None }
                self.match_vaend(args[0]).map(VaPart::End)
            }

            _ => None,
        }
    }

    pub fn convert_vaarg(&self, ctx: ExprContext, ty: CQualTypeId, val_id: CExprId) -> Result<WithStmts<P<Expr>>, String> {
        if self.tcfg.translate_valist {
            // https://github.com/rust-lang/rust/pull/49878/files

            let val = self.convert_expr(ctx.used(), val_id)?;
            let ty = self.convert_type(ty.ctype)?;

            let mut res = val.map(|va| {
                let path = mk().path_segment_with_args(
                    mk().ident("arg"),
                    mk().angle_bracketed_args(vec![ty]));
                mk().method_call_expr(va, path, vec![] as Vec<P<Expr>>)
            });
            if ctx.is_unused() {
                res.stmts.push(mk().expr_stmt(res.val));
                res.val = self.panic("convert_vaarg unused");
            }

            Ok(res)
        } else {
            Err(format!("Variable argument lists are not supported (requires --translate-valist)"))
        }
    }

    /// Determine if a variadic function body declares a va_list argument
    /// and contains a va_start and va_end call for that argument list.
    /// If it does the declaration ID for that variable is returned so that
    /// the resulting Rust function can have that variable argument list
    /// variable moved up to the argument list.
    pub fn well_formed_variadic(&self, body: CStmtId) -> Option<CDeclId> {

        let mut va_started: Option<CDeclId> = None;
        let mut va_end_found = false;

        let mut iter = DFExpr::new(&self.ast_context, body.into());
        while let Some(s) = iter.next() {
            if let SomeId::Expr(e) = s {
                if let Some(part) = self.match_vapart(e) {
                    println!("Found: {:?}", part);
                    match part {
                        VaPart::Start(va_id) => {
                            if va_started.is_some() {
                                return None
                            }
                            va_started = Some(va_id);
                        }
                        VaPart::Copy => return None,
                        VaPart::End(va_id) => {
                            if va_started != Some(va_id) || va_end_found {
                                return None
                            }
                            va_end_found = true;
                        }
                    }  
                }
            }    
        }

        if va_end_found { va_started } else { None }
    }
}