use super::*;

impl<'c> Translation<'c> {

    pub fn convert_vaarg(&self, ty: CQualTypeId, val_id: CExprId) -> Result<WithStmts<P<Expr>>, String> {
        if self.tcfg.translate_valist {
            // https://github.com/rust-lang/rust/pull/49878/files
            let val = self.convert_expr(ExprUse::Used, val_id, is_static, decay_ref)?;
            let ty = self.convert_type(ty.ctype)?;

            Ok(val.map(|va| {
                let path = mk().path_segment_with_args(
                    mk().ident("arg"),
                    mk().angle_bracketed_args(vec![ty]));
                mk().method_call_expr(va, path, vec![] as Vec<P<Expr>>)
            }))

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

        macro_rules! match_or {
            ([$e:expr] $p:pat => $r:tt ; $o:expr) => {
                let $r = match $e { $p => $r, _ => $o };
            };
            ([$e:expr] $p:pat if $g:expr => $r:tt ; $o:expr) => {
                let $r = match $e { $p if $g => $r, _ => $o };
            };
        }

        let mut iter = DFExpr::new(&self.ast_context, body.into());
        while let Some(x) = iter.next() {

            // This is a lot of code to match the patterns:
            // va_start((implicitcast)ARG, _)
            // va_copy(_, _)
            // va_end((implicitcast)ARG)
            match_or! { [x] SomeId::Expr(e) => e; continue }
            match_or! { [self.ast_context[e].kind]
                        CExprKind::Call(_, func, ref args) => (func, args); continue }
            match_or! { [self.ast_context[func].kind]
                        CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) => fexp; continue }
            match_or! { [self.ast_context[fexp].kind]
                        CExprKind::DeclRef(_, decl_id, _) => decl_id; continue }
            match_or! { [self.ast_context[decl_id].kind]
                        CDeclKind::Function { ref name, .. } => name; continue }

            match name as &str {
                "__builtin_va_start" => {
                    if va_started.is_some() || args.len() != 2 {
                        return None
                    }
                    match_or! { [self.ast_context[args[0]].kind]
                                CExprKind::ImplicitCast(_, e, _, _, _) => e; return None }
                    match_or! { [self.ast_context[e].kind]
                                CExprKind::DeclRef(_, va_id, _) => va_id; return None }

                    va_started = Some(va_id);
                }

                // We can't support va_copy yet. If a function uses it, we abort.
                "__builtin_va_copy" => return None,

                "__builtin_va_end" => {
                    if va_started.is_none() || va_end_found || args.len() != 1 {
                        return None
                    }
                    match_or! { [self.ast_context[args[0]].kind]
                                CExprKind::ImplicitCast(_, e, _, _, _) => e; return None }
                    match_or! { [self.ast_context[e].kind]
                        CExprKind::DeclRef(_, va_id, _) if Some(va_id) == va_started => (); return None }
                    
                    va_end_found = true;
                }

                _ => {}
            }
                       
        }

        if va_end_found { va_started } else { None }
    }
}