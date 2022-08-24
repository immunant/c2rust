use super::*;

#[derive(Copy, Clone, Debug)]
pub enum VaPart {
    Start(CDeclId),
    End(CDeclId),
    Copy(CDeclId, CDeclId),
}

macro_rules! match_or {
    ([$e:expr] $p:pat => $r:tt) => {
        match $e {
            $p => Some($r),
            _ => None,
        }
    };
    ([$e:expr] $p:pat if $g:expr => $r:tt) => {
        match $e {
            $p if $g => Some($r),
            _ => None,
        }
    };
}

impl<'c> Translation<'c> {
    /// Returns `true` iff `va_start`, `va_end`, or `va_copy` may be called on `decl_id`.
    pub fn is_va_decl(&self, decl_id: CDeclId) -> bool {
        self.function_context
            .borrow()
            .va_list_decl_ids
            .as_ref()
            .map(|decls| decls.contains(&decl_id))
            .unwrap_or(false)
    }

    pub fn match_vastart(&self, expr: CExprId) -> Option<CDeclId> {
        let ast = &self.ast_context;

        // `struct`-based `va_list` (e.g. x86_64).
        let match_vastart_struct = || {
            let e = match_or! { [ast[expr].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }?;
            let va_id = match_or! { [ast[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }?;
            Some(va_id)
        };

        // `struct`-based `va_list` (e.g. x86_64) where `va_list` is accessed as a `struct` member.
        //
        // Supporting this pattern is necessary to transpile apache httpd.
        let match_vastart_struct_member = || {
            let me = match_or! { [ast[expr].kind]
            CExprKind::ImplicitCast(_, me, _, _, _) => me }?;
            let e = match_or! { [ast[me].kind]
            CExprKind::Member(_, e, _, _, _) => e }?;
            let va_id = match_or! { [ast[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }?;
            Some(va_id)
        };

        // `struct`-based `va_list` (e.g. x86_64) where `va_list` is accessed as a member of a `struct *`.
        //
        // Supporting this pattern is necessary to transpile
        // [graphviz](https://gitlab.com/graphviz/graphviz/-/blob/5.0.0/lib/sfio/sftable.c#L321).
        let match_vastart_struct_pointer_member = || {
            let me = match_or! { [ast[expr].kind]
            CExprKind::ImplicitCast(_, me, _, _, _) => me }?;
            let ie = match_or! { [ast[me].kind]
            CExprKind::Member(_, ie, _, _, _) => ie }?;
            let e = match_or! { [ast[ie].kind]
            CExprKind::ImplicitCast(_, e, _, _, _) => e }?;
            let va_id = match_or! { [ast[e].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }?;
            Some(va_id)
        };

        // `char *`-based `va_list` (e.g. x86).
        let match_vastart_pointer = || {
            let va_id = match_or! { [ast[expr].kind]
            CExprKind::DeclRef(_, va_id, _) => va_id }?;
            Some(va_id)
        };

        match_vastart_struct()
            .or_else(match_vastart_pointer)
            .or_else(match_vastart_struct_member)
            .or_else(match_vastart_struct_pointer_member)
    }

    pub fn match_vaend(&self, expr: CExprId) -> Option<CDeclId> {
        self.match_vastart(expr)
    }

    pub fn match_vacopy(&self, dst_expr: CExprId, src_expr: CExprId) -> Option<(CDeclId, CDeclId)> {
        let dst_id = self.match_vastart(dst_expr)?;
        let src_id = self.match_vastart(src_expr)?;
        Some((dst_id, src_id))
    }

    pub fn match_vapart(&self, expr: CExprId) -> Option<VaPart> {
        let (func, args) = match_or! { [self.ast_context[expr].kind]
        CExprKind::Call(_, func, ref args) => (func, args) }?;
        let fexp = match_or! { [self.ast_context[func].kind]
        CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) => fexp }?;
        let decl_id = match_or! { [self.ast_context[fexp].kind]
        CExprKind::DeclRef(_, decl_id, _) => decl_id }?;
        let name = match_or! { [self.ast_context[decl_id].kind]
        CDeclKind::Function { ref name, .. } => name }?;
        let name = name.strip_prefix("__builtin_va_")?;
        match (name, args.as_slice()) {
            ("start", &[expr, _]) => self.match_vastart(expr).map(VaPart::Start),
            ("copy", &[dst_expr, src_expr]) => self
                .match_vacopy(dst_expr, src_expr)
                .map(|(did, sid)| VaPart::Copy(did, sid)),
            ("end", &[expr]) => self.match_vaend(expr).map(VaPart::End),
            _ => None,
        }
    }

    pub fn convert_vaarg(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        val_id: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if self.tcfg.translate_valist {
            let val = self.convert_expr(ctx.expect_valistimpl().used(), val_id)?;

            // The current implementation of the C-variadics feature doesn't allow us to
            // return `Option<fn(...) -> _>` from `VaList::arg`, so we detect function pointers
            // and construct the corresponding unsafe type `* mut fn(...) -> _`.
            let fn_ptr_ty: Option<Box<Type>> = {
                let resolved_ctype = self.ast_context.resolve_type(ty.ctype);
                if let CTypeKind::Pointer(p) = resolved_ctype.kind {
                    // ty is a pointer type
                    let resolved_ctype = self.ast_context.resolve_type(p.ctype);
                    if let CTypeKind::Function(ret, ref params, is_variadic, is_noreturn, _) =
                        resolved_ctype.kind
                    {
                        // ty is a function pointer type -> build Rust unsafe function pointer type
                        let opt_ret = if is_noreturn { None } else { Some(ret) };

                        let fn_ty = self.type_converter.borrow_mut().convert_function(
                            &self.ast_context,
                            opt_ret,
                            params,
                            is_variadic,
                        )?;

                        let m = if p.qualifiers.is_const {
                            Mutability::Immutable
                        } else {
                            Mutability::Mutable
                        };
                        Some(mk().set_mutbl(m).ptr_ty(fn_ty))
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            let have_fn_ptr = fn_ptr_ty.is_some();
            let mut arg_ty = fn_ptr_ty.unwrap_or_else(|| self.convert_type(ty.ctype).unwrap());

            let mut real_arg_ty = None;
            if self
                .ast_context
                .get_pointee_qual_type(ty.ctype)
                .map_or(false, |ty| {
                    self.ast_context.is_forward_declared_type(ty.ctype)
                })
            {
                real_arg_ty = Some(arg_ty.clone());
                arg_ty = mk().mutbl().ptr_ty(mk().path_ty(vec!["libc", "c_void"]));
            }

            val.and_then(|val| {
                let path = mk().path_segment_with_args(
                    mk().ident("arg"),
                    mk().angle_bracketed_args(vec![arg_ty]),
                );
                let mut val = mk().method_call_expr(val, path, vec![]);
                if let Some(ty) = real_arg_ty {
                    val = mk().cast_expr(val, ty);
                }

                if ctx.is_unused() {
                    Ok(WithStmts::new(
                        vec![mk().semi_stmt(val)],
                        self.panic_or_err("convert_vaarg unused"),
                    ))
                } else {
                    let val = if have_fn_ptr {
                        // transmute result of call to `arg` when expecting a function pointer
                        transmute_expr(mk().infer_ty(), mk().infer_ty(), val)
                    } else {
                        val
                    };

                    Ok(WithStmts::new_val(val))
                }
            })
        } else {
            Err(format_err!("Variable argument list translation is not enabled.").into())
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

        for s in DFExpr::new(&self.ast_context, body.into()) {
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
