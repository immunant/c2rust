use super::*;

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

    /// Returns true iff `va_start`, `va_end`, or `va_copy` may be called on `decl_id`.
    pub fn is_va_decl(&self, decl_id: CDeclId) -> bool {
        let fn_ctx = self.function_context.borrow();
        if let Some(ref decls) = fn_ctx.va_list_decl_ids {
            decls.contains(&decl_id)
        } else {
            false
        }
    }

    pub fn match_vastart(&self, expr: CExprId) -> Option<CDeclId> {
        // struct-based va_list (e.g. x86_64)
        fn match_vastart_struct(ast_context: &TypedAstContext, expr: CExprId) -> Option<CDeclId> {
            match_or! { [ast_context[expr].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }
            match_or! { [ast_context[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }
            Some(va_id)
        }

        // char pointer-based va_list (e.g. x86)
        fn match_vastart_pointer(ast_context: &TypedAstContext, expr: CExprId) -> Option<CDeclId> {
            match_or! { [ast_context[expr].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }
            Some(va_id)
        }

        match_vastart_struct(&self.ast_context, expr)
            .or_else(|| match_vastart_pointer(&self.ast_context, expr))
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
                    if ctx.is_const { self.use_feature("const_transmute"); }
                    transmute_expr(mk().infer_ty(), mk().infer_ty(), val, self.tcfg.emit_no_std)
                } else {
                    val
                };

                if ctx.is_unused() {
                    Ok(WithStmts::new(
                        vec![mk().semi_stmt(val)],
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

    /// Update the current function context by i) enabling the C variadics feature, ii) naming the
    /// Rust function argument that corresponds to the elipsis in the original C function, and iii)
    /// building a list of variable declarations to be translated into `VaListImpl`s. Returns the
    /// name of the `VaList` function argument for convenience.
    pub fn register_va_decls(&self, body: CStmtId) -> String {
        self.use_feature("c_variadic");

        let va_list_arg_name = self.renamer.borrow_mut().pick_name("args");

        // collect `va_list` variables that are `va_start`ed, `va_end`ed, or `va_copied`.
        let mut va_list_decl_ids: IndexSet<CDeclId> = IndexSet::new();

        let mut iter = DFExpr::new(&self.ast_context, body.into());
        while let Some(s) = iter.next() {
            if let SomeId::Expr(e) = s {
                if let Some(part) = self.match_vapart(e) {
                    let id = match part {
                        VaPart::Start(va_id) | VaPart::End(va_id) => va_id,
                        VaPart::Copy(dst_va_id, _src_va_id) => dst_va_id,
                    };
                    va_list_decl_ids.insert(id);
                }
            }
        }

        // update current function context
        let mut fn_ctx = self.function_context.borrow_mut();
        fn_ctx.va_list_arg_name = Some(va_list_arg_name.clone());
        fn_ctx.va_list_decl_ids = Some(va_list_decl_ids);

        va_list_arg_name
    }
}
