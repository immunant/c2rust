use super::*;
use std::collections::{HashMap};

#[derive(Copy, Clone, Debug)]
pub enum VaPart {
    Start(CDeclId),
    End(CDeclId),
    Copy(CDeclId, CDeclId),
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
    fn register_va_decls(&self, decl_id: CDeclId, copied_decl_ids: IndexSet<CDeclId>) {

        match self.ast_context[decl_id].kind {
            CDeclKind::Variable { ref ident, .. } => {
                self.renamer.borrow_mut()
                        .insert(decl_id, ident)
                        .expect(&format!("Failed to install variadic function kludge"));
            }
            _ => panic!("va_arg was not a variable"),
        }

        let mut fn_ctx = self.function_context.borrow_mut();
        fn_ctx.promoted_va_decl = Some(decl_id);
        fn_ctx.copied_va_decls = Some(copied_decl_ids);
    }

    pub fn is_promoted_va_decl(&self, decl_id: CDeclId) -> bool {
        let fn_ctx = self.function_context.borrow();
        fn_ctx.promoted_va_decl == Some(decl_id)
    }

    pub fn is_copied_va_decl(&self, decl_id: CDeclId) -> bool {
        let fn_ctx = self.function_context.borrow();
        if let Some(ref decls) = fn_ctx.copied_va_decls {
            decls.contains(&decl_id)
        } else { false }
    }

    pub fn get_promoted_va_decl(&self) -> Option<CDeclId> {
        self.function_context.borrow().promoted_va_decl
    }

    pub fn match_vastart(&self, expr: CExprId) -> Option<CDeclId> {
        match_or! { [self.ast_context[expr].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }
        match_or! { [self.ast_context[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }
        Some(va_id)
    }

    pub fn match_vaend(&self, expr: CExprId) -> Option<CDeclId> {
        self.match_vastart(expr)
    }

    pub fn match_vacopy(&self, dst_expr: CExprId, src_expr: CExprId) -> Option<(CDeclId, CDeclId)> {
        let dst_id = self.match_vastart(dst_expr);
        let src_id = self.match_vastart(src_expr);
        if let (Some(did), Some(sid)) = (dst_id, src_id) {
            return Some((did, sid));
        }
        None
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

            "__builtin_va_copy" => {
                if args.len() != 2 { return None }
                self.match_vacopy(args[0], args[1]).map(
                    |(did, sid)| VaPart::Copy(did, sid)
                )
            }

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
    pub fn is_well_formed_variadic(&self, body: CStmtId) -> bool {
        let mut candidates: HashMap<CDeclId, Vec<VaPart>> = HashMap::new();

        let mut iter = DFExpr::new(&self.ast_context, body.into());
        while let Some(s) = iter.next() {
            if let SomeId::Expr(e) = s {
                if let Some(part) = self.match_vapart(e) {
                    let id = match part {
                        VaPart::Start(va_id) | VaPart::End(va_id) => va_id, 
                        VaPart::Copy(dst_va_id, _src_va_id) => dst_va_id,
                    };
                    candidates.entry(id).or_insert(vec![]).push(part);
                }
            }
        }

        let start_called = |k: &CDeclId| candidates[k]
                .iter()
                .any(|e| if let VaPart::Start( _ ) = e { true } else { false });
        let copy_called = |k: &CDeclId| candidates[k]
            .iter()
            .any(|e| if let VaPart::Copy(_, _) = e { true } else { false });
        let end_called = |k: &CDeclId| candidates[k]
                .iter()
                .any(|e| if let VaPart::End( _ ) = e { true } else { false });

        // ids initialized by `va_copy` and finalized by `va_end`
        let copied = candidates
            .keys()
            .filter_map(|k| if copy_called(k) && end_called(k) { Some(*k) } else { None })
            .collect::<IndexSet<CDeclId>>();

        // ids initialized by `va_start` and finalized by `va_end`
        let promotable = candidates
            .keys()
            .filter_map(|k| if start_called(k) && end_called(k) { Some(*k) } else { None })
            .collect::<Vec<CDeclId>>();

        if promotable.len() == 1 {
            self.register_va_decls(promotable[0], copied);
            return true;
        }
        false
    }
}
