use super::*;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub enum VaPart {
    Start(CDeclId),
    End(CDeclId),
    Copy(CDeclId, CDeclId),
}

macro_rules! match_or {
    ([$e:expr] $p:pat => $r:tt) => {
        let $r = match $e {
            $p => $r,
            _ => return None,
        };
    };
    ([$e:expr] $p:pat if $g:expr => $r:tt) => {
        let $r = match $e {
            $p if $g => $r,
            _ => return None,
        };
    };
}

impl<'c> Translation<'c> {
    /// Install a fake variable into the renamer as a kludge until we have
    /// proper variadic function definition support
    fn register_va_decls(
        &self,
        promoted_decl_id: Option<CDeclId>,
        copied_decl_ids: IndexSet<CDeclId>,
    ) {
        let mut fn_ctx = self.function_context.borrow_mut();
        fn_ctx.copied_va_decls = Some(copied_decl_ids);

        if let Some(decl_id) = promoted_decl_id {
            // found a promotable `va_list`
            fn_ctx.promoted_va_decl = Some(decl_id);
            match self.ast_context[decl_id].kind {
                CDeclKind::Variable { ref ident, .. } => {
                    self.renamer
                        .borrow_mut()
                        .insert(decl_id, ident)
                        .expect(&format!("Failed to install variadic function kludge"));
                }
                _ => panic!("va_arg was not a variable"),
            }
        }
    }

    pub fn is_promoted_va_decl(&self, decl_id: CDeclId) -> bool {
        let fn_ctx = self.function_context.borrow();
        fn_ctx.promoted_va_decl == Some(decl_id)
    }

    pub fn is_copied_va_decl(&self, decl_id: CDeclId) -> bool {
        let fn_ctx = self.function_context.borrow();
        if let Some(ref decls) = fn_ctx.copied_va_decls {
            decls.contains(&decl_id)
        } else {
            false
        }
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
                if args.len() != 2 {
                    return None;
                }
                self.match_vastart(args[0]).map(VaPart::Start)
            }

            "__builtin_va_copy" => {
                if args.len() != 2 {
                    return None;
                }
                self.match_vacopy(args[0], args[1])
                    .map(|(did, sid)| VaPart::Copy(did, sid))
            }

            "__builtin_va_end" => {
                if args.len() != 1 {
                    return None;
                }
                self.match_vaend(args[0]).map(VaPart::End)
            }

            _ => None,
        }
    }

    pub fn convert_vaarg(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        val_id: CExprId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        if self.tcfg.translate_valist {
            // https://github.com/rust-lang/rust/pull/49878/files

            let val = self.convert_expr(ctx.used(), val_id)?;

            // The current implementation of the C-variadics feature doesn't allow us to
            // return `Option<fn(...) -> _>` from `VaList::arg`, so we detect function pointers
            // and construct the corresponding unsafe type `* mut fn(...) -> _`.
            let fn_ptr_ty : Option<P<Ty>> = {
                let resolved_ctype = self.ast_context.resolve_type(ty.ctype);
                if let CTypeKind::Pointer(p) = resolved_ctype.kind {
                    // ty is a pointer type
                    let resolved_ctype = self.ast_context.resolve_type(p.ctype);
                    if let CTypeKind::Function(
                                        ret,
                                        ref params,
                                        is_variadic,
                                        is_noreturn, _) = resolved_ctype.kind {
                        // ty is a function pointer type -> build Rust unsafe function pointer type
                        let opt_ret = if is_noreturn { None } else { Some(ret) };
                        
                        let fn_ty = self.type_converter
                            .borrow_mut()
                            .convert_function(&self.ast_context, opt_ret, params, is_variadic)?;

                        let m = if p.qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                        Some(mk().set_mutbl(m).ptr_ty(fn_ty))
                    } else { None }
                } else { None }
            };

            let have_fn_ptr = fn_ptr_ty.is_some();
            let ty = fn_ptr_ty.unwrap_or_else(||self.convert_type(ty.ctype).unwrap());

            val.and_then(|val| {
                let path = mk()
                    .path_segment_with_args(mk().ident("arg"), mk().angle_bracketed_args(vec![ty]));
                let val = mk().method_call_expr(val, path, vec![] as Vec<P<Expr>>);

                let val = if have_fn_ptr {
                    // transmute result of call to `arg` when expecting a function pointer
                    transmute_expr(mk().infer_ty(), mk().infer_ty(), val, self.tcfg.emit_no_std)
                } else {
                    val
                };

                if ctx.is_unused() {
                    Ok(WithStmts::new(
                        vec![mk().expr_stmt(val)],
                        self.panic_or_err("convert_vaarg unused"),
                    ))
                } else {
                    Ok(WithStmts::new_val(val))
                }
            })
        } else {
            Err(format_err!(
                "Variable argument list translation is not enabled."
            ))?
        }
    }

    /// Determine if a variadic function body declares a va_list argument
    /// and contains a va_start and va_end call for that argument list.
    /// If it does, the declaration ID for that variable is registered so that
    /// the resulting Rust function can have that variable argument list
    /// variable moved up to the argument list.
    pub fn is_well_formed_variadic(&self, body: CStmtId) -> bool {
        // maps each va_list to the operations performed on it (e.g. va_start, va_end)
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

        if candidates.len() == 0 {
            // no calls to `va_start`, `va_copy` or `va_end` is fine
            return true;
        }

        let start_called = |k: &CDeclId| {
            candidates[k].iter().any(|e| {
                if let VaPart::Start(_) = e {
                    true
                } else {
                    false
                }
            })
        };
        let copy_called = |k: &CDeclId| {
            candidates[k].iter().any(|e| {
                if let VaPart::Copy(_, _) = e {
                    true
                } else {
                    false
                }
            })
        };

        // va_lists initialized by `va_copy`
        let copied = candidates
            .keys()
            .filter_map(|k| {
                if copy_called(k) {
                    Some(*k)
                } else {
                    None
                }
            })
            .collect::<IndexSet<CDeclId>>();

        // va_lists initialized by `va_start`
        let promotable = candidates
            .keys()
            .filter_map(|k| {
                if start_called(k) {
                    Some(*k)
                } else {
                    None
                }
            })
            .collect::<Vec<CDeclId>>();

        if promotable.len() + copied.len() > 0 {
            // have promotable and/or copied va_lists that need registration
            let promoted = match promotable.len() {
                0 => None,
                1 => Some(promotable[0]),
                _ => panic!(
                    "couldn't determine which va_list to promote in {}",
                    self.function_context.borrow().get_name()
                ),
            };
            self.register_va_decls(promoted, copied);
            return true;
        }
        false
    }
}
