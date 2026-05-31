use crate::c_ast::iterators::{immediate_children_all_types, NodeVisitor, SomeId};
use crate::c_ast::{
    CBinOp, CDeclId, CDeclKind, CExprId, CExprKind, CQualTypeId, CStmtId, CStmtKind, CTypeKind,
    CUnOp, CastKind, TypedAstContext,
};
use crate::translator::simd::simd_fn_from_builtin_fn;
use std::collections::{HashMap, HashSet};

pub struct ContextVisitor<'a> {
    ast_context: &'a TypedAstContext,

    /// Whether an expression is within a conditional context, meaning that it is in a context
    /// where a Rust `bool` will be expected.
    expr_is_condition: HashSet<CExprId>,

    /// Whether an expression is the argument of an SIMD function call.
    expr_is_simd_arg: HashSet<CExprId>,

    /// Override for the type of an expression.
    pub expr_override_types: HashMap<CExprId, CQualTypeId>,

    /// The stack of functions that is currently being processed.
    in_function: Vec<CDeclId>,
}

impl<'a> ContextVisitor<'a> {
    pub fn new(ast_context: &'a TypedAstContext) -> Self {
        Self {
            ast_context,
            expr_is_condition: HashSet::new(),
            expr_is_simd_arg: HashSet::new(),
            expr_override_types: HashMap::new(),
            in_function: Vec::new(),
        }
    }

    pub fn visit(&mut self, c_decls_top: &[CDeclId]) {
        for &decl in c_decls_top {
            self.visit_tree(SomeId::Decl(decl));
        }
    }

    fn pre_stmt(&mut self, stmt: CStmtId) {
        match self.ast_context[stmt].kind {
            CStmtKind::Return(Some(expr)) => {
                let func_id = match self.in_function.last() {
                    Some(&func_id) => func_id,
                    None => return,
                };
                let func_ty = match self.ast_context[func_id].kind {
                    CDeclKind::Function { typ, .. } => typ,
                    _ => return,
                };
                let return_ty = match self.ast_context.resolve_type(func_ty).kind {
                    CTypeKind::Function(return_ty, _, _, false, _) => return_ty,
                    _ => return,
                };

                self.expr_override_types.insert(expr, return_ty);
            }

            CStmtKind::If {
                scrutinee: condition,
                ..
            }
            | CStmtKind::While { condition, .. }
            | CStmtKind::DoWhile { condition, .. }
            | CStmtKind::ForLoop {
                condition: Some(condition),
                ..
            } => {
                self.expr_is_condition.insert(condition);
            }

            _ => {}
        }
    }

    fn pre_expr(&mut self, parent_expr: CExprId) {
        let expr_override_type = self.expr_override_types.get(&parent_expr).copied();
        let expr_is_condition = self.expr_is_condition.contains(&parent_expr);
        let expr_is_simd_arg = self.expr_is_simd_arg.contains(&parent_expr);

        let expr_kind = &self.ast_context.index_expr_raw(parent_expr).kind;

        match *expr_kind {
            CExprKind::Literal(..) => {}

            CExprKind::Unary(ty, op, arg, _) => match op {
                CUnOp::Plus | CUnOp::Negate | CUnOp::Complement | CUnOp::Extension => {
                    let cqual_type = expr_override_type.unwrap_or(ty);
                    self.expr_override_types.insert(arg, cqual_type);
                }

                CUnOp::Not => {
                    self.expr_is_condition.insert(arg);
                }

                CUnOp::AddressOf
                | CUnOp::Deref
                | CUnOp::PostIncrement
                | CUnOp::PreIncrement
                | CUnOp::PostDecrement
                | CUnOp::PreDecrement
                | CUnOp::Real
                | CUnOp::Imag
                | CUnOp::Coawait => {}
            },

            CExprKind::UnaryType(..) => {}
            CExprKind::OffsetOf(..) => {}

            CExprKind::Binary(ty, op, lhs, rhs, _, _) => {
                match op {
                    CBinOp::Comma => {
                        let expr_type_id = expr_override_type.unwrap_or(ty);
                        self.expr_override_types.insert(rhs, expr_type_id);
                    }

                    CBinOp::And | CBinOp::Or => {
                        self.expr_is_condition.insert(lhs);
                        self.expr_is_condition.insert(rhs);
                    }

                    CBinOp::AssignAdd
                    | CBinOp::AssignSubtract
                    | CBinOp::AssignMultiply
                    | CBinOp::AssignDivide
                    | CBinOp::AssignModulus
                    | CBinOp::AssignBitXor
                    | CBinOp::AssignShiftLeft
                    | CBinOp::AssignShiftRight
                    | CBinOp::AssignBitOr
                    | CBinOp::AssignBitAnd
                    | CBinOp::Assign => {}

                    // Operands of == and != have no override for null pointer comparisons,
                    // because only the non-null operand is converted.
                    CBinOp::EqualEqual | CBinOp::NotEqual
                        if expr_is_condition
                            && (self.ast_context.is_null_expr(lhs)
                                || self.ast_context.is_null_expr(rhs)) => {}

                    _ => {
                        let expr_type_id = expr_override_type.unwrap_or(ty);

                        let lhs_kind = &self.ast_context[lhs].kind;
                        let mut lhs_type_id = match lhs_kind.get_qual_type() {
                            Some(ty) => ty,
                            None => return,
                        };
                        let rhs_kind = &self.ast_context[rhs].kind;
                        let mut rhs_type_id = match rhs_kind.get_qual_type() {
                            Some(ty) => ty,
                            None => return,
                        };

                        // If this operation will (in Rust) take args of the same type, then
                        // propagate our expected type down to the translation of our argument
                        // expressions.
                        let lhs_resolved_ty = self.ast_context.resolve_type(lhs_type_id.ctype);
                        let rhs_resolved_ty = self.ast_context.resolve_type(rhs_type_id.ctype);
                        let expr_ty_kind = &self.ast_context[expr_type_id.ctype].kind;

                        // Addition and subtraction can accept one pointer argument for .offset(),
                        // in which case we don't want to homogenize arg types.
                        if !lhs_resolved_ty.kind.is_pointer()
                            && !rhs_resolved_ty.kind.is_pointer()
                            && !expr_ty_kind.is_pointer()
                        {
                            if op.all_types_same() {
                                // Ops like division and bitxor accept inputs of their expected
                                // result type.
                                lhs_type_id = expr_type_id;
                                rhs_type_id = expr_type_id;
                            } else if op.input_types_same()
                                && lhs_resolved_ty.kind != rhs_resolved_ty.kind
                            {
                                if CTypeKind::PULLBACK_KINDS.contains(&lhs_resolved_ty.kind) {
                                    rhs_type_id = lhs_type_id;
                                } else {
                                    lhs_type_id = rhs_type_id;
                                }
                            } else if matches!(op, CBinOp::ShiftLeft | CBinOp::ShiftRight) {
                                lhs_type_id = expr_type_id;
                            }
                        }

                        self.expr_override_types.insert(lhs, lhs_type_id);
                        self.expr_override_types.insert(rhs, rhs_type_id);
                    }
                }
            }

            CExprKind::ImplicitCast(ty, expr, kind, _, _)
            | CExprKind::ExplicitCast(ty, expr, kind, _, _) => {
                let is_explicit = matches!(expr_kind, CExprKind::ExplicitCast(..));

                if expr_is_simd_arg && !is_explicit && matches!(kind, CastKind::IntegralCast) {
                    return;
                }

                if matches!(
                    kind,
                    CastKind::IntegralToBoolean
                        | CastKind::FloatingToBoolean
                        | CastKind::PointerToBoolean
                ) {
                    self.expr_is_condition.insert(expr);
                }

                let child_expr_kind = &self.ast_context[expr].kind;
                let target_ty = expr_override_type.unwrap_or(ty);

                if !is_explicit {
                    let mut literal_expr_kind = child_expr_kind;
                    let mut is_negated = false;

                    if let &CExprKind::Unary(_, CUnOp::Negate, subexpr_id, _) = literal_expr_kind {
                        literal_expr_kind = &self.ast_context[subexpr_id].kind;
                        is_negated = true;
                    }

                    if let CExprKind::Literal(_, lit) = literal_expr_kind {
                        if self
                            .ast_context
                            .literal_matches_ty(lit, target_ty, is_negated)
                        {
                            self.expr_override_types.insert(expr, target_ty);
                        }
                    }

                    if let Some(expr_override_type) = expr_override_type {
                        if kind == CastKind::LValueToRValue {
                            let source_ty = match self.ast_context[expr].kind.get_qual_type() {
                                Some(ty) => ty,
                                None => return,
                            };

                            if source_ty.ctype != expr_override_type.ctype {
                                self.expr_override_types.insert(expr, expr_override_type);
                            }
                        }
                    }
                }
            }

            CExprKind::ConstantExpr(ty, expr, _) => {
                self.expr_override_types.insert(expr, ty);
            }

            CExprKind::DeclRef(..) => {}

            CExprKind::Call(_, func, ref args) => {
                if let CExprKind::ImplicitCast(_, fexp, CastKind::BuiltinFnToFnPtr, _, _) =
                    self.ast_context[func].kind
                {
                    let CExprKind::DeclRef(_, decl_id, _) = self.ast_context[fexp].kind else {
                        return;
                    };
                    let CDeclKind::Function {
                        name: ref builtin_name,
                        ..
                    } = self.ast_context[decl_id].kind
                    else {
                        return;
                    };

                    if simd_fn_from_builtin_fn(builtin_name).is_some() {
                        for &arg in args {
                            self.expr_is_simd_arg.insert(arg);
                        }
                    }

                    return;
                }

                // Try to get the Variable decl directly,
                // or fall back to querying the function pointer type.
                let tys_of_params = if let Some(CDeclKind::Function { parameters, .. }) =
                    self.ast_context.fn_declref_decl(func)
                {
                    self.ast_context.tys_of_params(parameters)
                } else {
                    self.ast_context[func]
                        .kind
                        .get_type()
                        .and_then(|fn_ptr_qty| self.ast_context.get_pointee_qual_type(fn_ptr_qty))
                        .and_then(
                            |fn_qty| match self.ast_context.resolve_type(fn_qty.ctype).kind {
                                CTypeKind::Function(_, ref param_tys, ..) => {
                                    Some(param_tys.clone())
                                }
                                _ => None,
                            },
                        )
                };

                if let Some(tys_of_params) = tys_of_params {
                    for (&arg, param_ty) in args.iter().zip(tys_of_params) {
                        if !(self.ast_context[arg].kind.get_qual_type())
                            .map_or(false, |qtype| self.ast_context.is_va_list(qtype.ctype))
                        {
                            self.expr_override_types.insert(arg, param_ty);
                        }
                    }
                }
            }

            CExprKind::Member(..) => {}
            CExprKind::ArraySubscript(..) => {}

            CExprKind::Conditional(ty, _, lhs, rhs) => {
                self.expr_is_condition.insert(lhs);

                self.expr_override_types
                    .insert(lhs, expr_override_type.unwrap_or(ty));
                self.expr_override_types
                    .insert(rhs, expr_override_type.unwrap_or(ty));
            }

            CExprKind::BinaryConditional(_, condition, _) => {
                self.expr_is_condition.insert(condition);
            }

            CExprKind::InitList(ty, ref exprs, _, _) => {
                match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::Struct(struct_id) => {
                        let field_decl_ids = match self.ast_context[struct_id].kind {
                            CDeclKind::Struct {
                                fields: Some(ref fields),
                                ..
                            } => fields,
                            _ => return,
                        };

                        let field_info_iter = field_decl_ids.iter().filter_map(|&field_id| {
                            match self.ast_context[field_id].kind {
                                CDeclKind::Field {
                                    bitfield_width: Some(0),
                                    ..
                                } => None,
                                CDeclKind::Field { typ, .. } => Some(typ),
                                _ => None,
                            }
                        });

                        for (&expr, typ) in exprs.iter().zip(field_info_iter) {
                            self.expr_override_types.insert(expr, typ);
                        }
                    }

                    _ => {}
                }
            }

            CExprKind::ImplicitValueInit(..) => {}

            CExprKind::Paren(_, expr) => {
                if expr_is_condition {
                    self.expr_is_condition.insert(expr);
                }

                if expr_is_simd_arg {
                    self.expr_is_simd_arg.insert(expr);
                }

                if let Some(expr_override_type) = expr_override_type {
                    self.expr_override_types.insert(expr, expr_override_type);
                }
            }

            CExprKind::CompoundLiteral(_, expr) => {
                if let Some(expr_override_type) = expr_override_type {
                    self.expr_override_types.insert(expr, expr_override_type);
                }
            }

            CExprKind::Predefined(_, expr) => {
                if let Some(expr_override_type) = expr_override_type {
                    self.expr_override_types.insert(expr, expr_override_type);
                }
            }

            CExprKind::Statements(..) => {}
            CExprKind::VAArg(..) => {}
            CExprKind::ShuffleVector(..) => {}
            CExprKind::ConvertVector(..) => {}
            CExprKind::DesignatedInitExpr(..) => {}

            CExprKind::Choose(_, _, lhs, rhs, _) => {
                if let Some(expr_override_type) = expr_override_type {
                    self.expr_override_types.insert(lhs, expr_override_type);
                    self.expr_override_types.insert(rhs, expr_override_type);
                }
            }

            CExprKind::Atomic { .. } => {}
            CExprKind::BadExpr => {}
        }
    }

    fn pre_decl(&mut self, decl: CDeclId) {
        match self.ast_context[decl].kind {
            CDeclKind::Function { .. } => {
                self.in_function.push(decl);
            }

            CDeclKind::Variable {
                typ,
                initializer: Some(initializer),
                ..
            } => {
                self.expr_override_types.insert(initializer, typ);
            }

            _ => {}
        }
    }

    fn post_decl(&mut self, decl: CDeclId) {
        match self.ast_context[decl].kind {
            CDeclKind::Function { .. } => {
                if matches!(self.in_function.last(), Some(&func) if func == decl) {
                    self.in_function.pop();
                }
            }

            _ => {}
        }
    }
}

impl<'a> NodeVisitor for ContextVisitor<'a> {
    fn children(&mut self, id: SomeId) -> Vec<SomeId> {
        immediate_children_all_types(self.ast_context, id)
    }

    fn pre(&mut self, id: SomeId) -> bool {
        match id {
            SomeId::Stmt(stmt) => self.pre_stmt(stmt),
            SomeId::Expr(expr) => self.pre_expr(expr),
            SomeId::Decl(decl) => self.pre_decl(decl),
            SomeId::Type(_) => {}
        }

        true
    }

    fn post(&mut self, id: SomeId) {
        match id {
            SomeId::Stmt(_stmt) => {}
            SomeId::Expr(_expr) => {}
            SomeId::Decl(decl) => self.post_decl(decl),
            SomeId::Type(_) => {}
        }
    }
}
