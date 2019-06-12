use rlua::UserData;
use rlua::prelude::{LuaContext, LuaError, LuaResult, LuaTable};
use syntax::ast::{
    Arg, Arm, BindingMode, Block, CaptureBy, Crate, Expr, ExprKind, FunctionRetTy, FnDecl,
    FloatTy, Guard, ImplItem, ImplItemKind, InlineAsmOutput, Item, ItemKind, LitKind, Local, Mod,
    Movability, Mutability::*, Pat, PatKind, RangeLimits, Stmt, StmtKind, LitIntType, Ty, TyKind,
    Unsafety, BareFnTy, UnsafeSource, BlockCheckMode, Path, Field, PathSegment, GenericArgs,
    GenericArg, AsmDialect, Constness, UseTree, UseTreeKind,
};
use syntax::ext::hygiene::SyntaxContext;
use syntax::ptr::P;
use syntax_pos::SpanData;

use crate::ast_manip::fn_edit::{FnKind, FnLike};
use crate::scripting::TransformCtxt;

#[derive(Clone, Debug)]
pub(crate) struct LuaSpan(pub(crate) SpanData);

impl UserData for LuaSpan {}

#[derive(Clone, Debug)]
pub(crate) struct LuaSyntaxContext(pub(crate) SyntaxContext);

impl UserData for LuaSyntaxContext {}

pub(crate) trait IntoLuaAst<'lua> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>>;
}

/// AST Stmt
// @table Stmt
// @field type "Stmt"
// @tfield string kind `StmtKind` of this statement
//
// `StmtKind::Local` only:
// @tfield[opt] LuaAstNode ty Type of local
// @tfield[opt] LuaAstNode init Initializer of local
// @tfield LuaAstNode pat Name of local
//
// `StmtKind::Item` only:
// @tfield LuaAstNode item Item node
//
// `StmtKind::Semi` and `StmtKind::Expr` only:
// @tfield LuaAstNode expr Expression in this statement
impl<'lua> IntoLuaAst<'lua> for Stmt {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Stmt")?;
        ast.set("span", LuaSpan(self.span.data()))?;

        match self.node {
            StmtKind::Local(l) => {
                ast.set("kind", "Local")?;
                let Local { pat, ty, init, .. } = l.into_inner();

                ast.set("pat", pat.into_lua_ast(ctx, lua_ctx)?)?;

                if let Some(ty) = ty {
                    ast.set("ty", ctx.intern(ty))?;
                }

                if let Some(init) = init {
                    ast.set("init", init.into_lua_ast(ctx, lua_ctx)?)?;
                }
            }
            StmtKind::Item(i) => {
                ast.set("kind", "Item")?;
                ast.set("item", i.into_lua_ast(ctx, lua_ctx)?)?;
            }
            StmtKind::Semi(e) => {
                ast.set("kind", "Semi")?;
                ast.set("expr_old", ctx.intern(e.clone()))?;
                ast.set("expr", e.into_lua_ast(ctx, lua_ctx)?)?;
            }
            StmtKind::Expr(e) => {
                ast.set("kind", "Expr")?;
                ast.set("expr_old", ctx.intern(e.clone()))?;
                ast.set("expr", e.into_lua_ast(ctx, lua_ctx)?)?;
            }
            StmtKind::Mac(_) => {
                return Err(LuaError::external("StmtKind::Mac is not yet implemented"));
            }
        }

        Ok(ast)
    }
}

/// AST Expr
// @table Expr
// @field type "Expr"
// @tfield string kind `ExprKind` of this expression
//
// `ExprKind::Lit` only:
// @field value Literal value of this expression
impl<'lua> IntoLuaAst<'lua> for P<Expr> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        ast.set("type", "Expr")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("span", LuaSpan(self.span.data()))?;

        let callee_info = ctx.cx.opt_callee_info(&self);

        self.and_then(|expr| {
            match expr.node {
                ExprKind::Lit(l) => {
                    ast.set("kind", "Lit")?;
                    match l.node {
                        LitKind::Str(s, _) => {
                            ast.set("value", s.as_str().get())?;
                            ast.set("is_bytes", false)?;
                        },
                        LitKind::Int(i, suffix) => {
                            ast.set("value", i)?;
                            // (R)Lua will convert any value > i64::max_value()
                            // to a float so we must stringly type that it's
                            // expected to be an integer
                            ast.set("num_kind", "Int")?;

                            match suffix {
                                LitIntType::Signed(int_ty) => {
                                    ast.set("suffix", int_ty.ty_to_string())?;
                                },
                                LitIntType::Unsigned(uint_ty) => {
                                    ast.set("suffix", uint_ty.ty_to_string())?;
                                },
                                LitIntType::Unsuffixed => (),
                            }
                        },
                        LitKind::Bool(b) => ast.set("value", b)?,
                        LitKind::ByteStr(bytes) => {
                            ast.set("value", lua_ctx.create_string(&*bytes)?)?;
                            ast.set("is_bytes", true)?;
                        },
                        LitKind::Char(ch) => {
                            let mut buf = [0; 4];
                            let char_str = ch.encode_utf8(&mut buf);

                            ast.set("value", &*char_str)?;
                        },
                        LitKind::FloatUnsuffixed(symbol) => {
                            let string = symbol.as_str().get();
                            let float = string
                                .parse::<f64>()
                                .map_err(|e| LuaError::external(e))?;

                            ast.set("value", float)?;
                            ast.set("num_kind", "Float")?;
                        },
                        LitKind::Float(symbol, suffix) => {
                            ast.set("num_kind", "Float")?;

                            let string = symbol.as_str().get();

                            match suffix {
                                FloatTy::F32 => {
                                    let float = string
                                        .parse::<f32>()
                                        .map_err(|e| LuaError::external(e))?;

                                    ast.set("value", float)?;
                                    ast.set("suffix", "f32")?;
                                },
                                FloatTy::F64 => {
                                    let float = string
                                        .parse::<f64>()
                                        .map_err(|e| LuaError::external(e))?;

                                    ast.set("value", float)?;
                                    ast.set("suffix", "f64")?;
                                },
                            };
                        },
                        _ => {
                            return Err(LuaError::external(format!(
                                "{:?} is not yet implemented",
                                l.node
                            )));
                        }
                    }
                },
                ExprKind::Box(boxed) => {
                    ast.set("kind", "Box")?;
                    ast.set("boxed", boxed.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::ObsoleteInPlace(_, _) => {
                    ast.set("kind", "ObsoleteInPlace")?;
                    // TODO: Flesh out further
                },
                ExprKind::Array(values) => {
                    let vals: LuaResult<Vec<_>> = values
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Array")?;
                    ast.set("values", lua_ctx.create_sequence_from(vals?.into_iter())?)?;
                },
                ExprKind::Call(path, args) => {
                   let args: LuaResult<Vec<_>> = args
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Call")?;
                    ast.set("path", path.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("args", lua_ctx.create_sequence_from(args?.into_iter())?)?;
                },
                ExprKind::MethodCall(segment, args) => {
                    let args: LuaResult<Vec<_>> = args
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();
                    let callee_info = callee_info
                        .ok_or(LuaError::external("Could not get MethodCall callee_info"))?;

                    ast.set("kind", "MethodCall")?;
                    ast.set("args", lua_ctx.create_sequence_from(args?.into_iter())?)?;
                    ast.set("name", segment.ident.name.as_str().get())?;

                    let self_ty = callee_info.fn_sig
                        .inputs()
                        .first()
                        .expect("Self param on method");

                    ast.set("caller_is", match self_ty.sty {
                        rustc::ty::TyKind::Ref(_, _, mutability) => {
                            match mutability {
                                rustc::hir::Mutability::MutMutable => "ref_mut",
                                rustc::hir::Mutability::MutImmutable => "ref",
                            }
                        },
                        _ => "owned",
                    })?;
                },
                ExprKind::Tup(exprs) => {
                   let exprs: LuaResult<Vec<_>> = exprs
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Tup")?;
                    ast.set("exprs", lua_ctx.create_sequence_from(exprs?.into_iter())?)?;
                },
                ExprKind::Binary(op, lhs, rhs) => {
                    ast.set("kind", "Binary")?;
                    ast.set("op", op.node.to_string())?;
                    ast.set("lhs", lhs.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("rhs", rhs.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Unary(op, expr) => {
                    ast.set("kind", "Unary")?;
                    ast.set("op", syntax::ast::UnOp::to_string(op))?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Cast(expr, ty) => {
                    ast.set("kind", "Cast")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Type(expr, _ty) => {
                    ast.set("kind", "Type")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::If(cond, then, opt_else) => {
                    ast.set("kind", "If")?;
                    ast.set("cond", cond.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("then", then.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(els) = opt_else {
                        ast.set("else", els.into_lua_ast(ctx, lua_ctx)?)?;
                    }
                },
                ExprKind::IfLet(_, expr, then, opt_else) => {
                    ast.set("kind", "IfLet")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("then", then.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(els) = opt_else {
                        ast.set("else", els.into_lua_ast(ctx, lua_ctx)?)?;
                    }

                    // TODO: Flesh out further
                },
                ExprKind::While(cond, block, opt_label) => {
                    ast.set("kind", "While")?;
                    ast.set("cond", cond.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::WhileLet(_pats, expr, block, opt_label) => {
                    ast.set("kind", "WhileLet")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::ForLoop(pat, expr, block, opt_label) => {
                    ast.set("kind", "ForLoop")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("pat", pat.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::Loop(block, opt_label) => {
                    ast.set("kind", "Loop")?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::Match(expr, arms) => {
                    let arms: LuaResult<Vec<_>> = arms
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Match")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("arms", lua_ctx.create_sequence_from(arms?)?)?;
                },
                ExprKind::Closure(capture_by, _is_async, movability, fn_decl, expr, _span) => {
                    ast.set("kind", "Closure")?;
                    ast.set("fn_decl", fn_decl.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("capture_by", match capture_by {
                        CaptureBy::Ref => "Ref",
                        CaptureBy::Value => "Value",
                    })?;
                    ast.set("movability", match movability {
                        Movability::Movable => "Movable",
                        Movability::Static => "Static",
                    })?;
                },
                ExprKind::Block(block, opt_label) => {
                    ast.set("kind", "Block")?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::Async(..) => {
                    ast.set("kind", "Async")?;
                    // TODO: Flesh out further
                },
                ExprKind::TryBlock(block) => {
                    ast.set("kind", "TryBlock")?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Assign(lhs, rhs) => {
                    ast.set("kind", "Assign")?;
                    ast.set("lhs", lhs.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("rhs", rhs.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::AssignOp(op, lhs, rhs) => {
                    ast.set("kind", "AssignOp")?;
                    ast.set("op", op.node.to_string())?;
                    ast.set("lhs", lhs.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("rhs", rhs.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Field(expr, name) => {
                    ast.set("kind", "Field")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("name", name.as_str().get())?;
                },
                ExprKind::Index(indexed, index) => {
                    ast.set("kind", "Index")?;
                    ast.set("indexed", indexed.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("index", index.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Range(opt_lhs, opt_rhs, limits) => {
                    ast.set("kind", "Range")?;

                    if let Some(lhs) = opt_lhs {
                        ast.set("lhs", lhs.into_lua_ast(ctx, lua_ctx)?)?;
                    }

                    if let Some(rhs) = opt_rhs {
                        ast.set("rhs", rhs.into_lua_ast(ctx, lua_ctx)?)?;
                    }

                    ast.set("limits", match limits {
                        RangeLimits::HalfOpen => "HalfOpen",
                        RangeLimits::Closed => "Closed",
                    })?;
                },
                ExprKind::Path(_, path) => {
                    ast.set("kind", "Path")?;
                    ast.set("segments", path.into_lua_ast(ctx, lua_ctx)?)?;
                    // TODO: Flesh out further
                },
                ExprKind::AddrOf(mutability, expr) => {
                    ast.set("kind", "AddrOf")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("mutability", match mutability {
                        Immutable => "Immutable",
                        Mutable => "Mutable",
                    })?;
                },
                ExprKind::Break(opt_label, opt_expr) => {
                    ast.set("kind", "Break")?;

                    if let Some(expr) = opt_expr {
                        ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    }

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::Continue(opt_label) => {
                    ast.set("kind", "Continue")?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::Ret(opt_val) => {
                    ast.set("kind", "Ret")?;

                    if let Some(val) = opt_val {
                        ast.set("value", val.into_lua_ast(ctx, lua_ctx)?)?;
                    }
                },
                ExprKind::InlineAsm(inline_asm) => {
                    let inline_asm = inline_asm.into_inner();
                    let inputs: LuaResult<Vec<_>> = inline_asm.inputs
                        .into_iter()
                        .map(|(sym, expr)| {
                            let input = lua_ctx.create_table()?;
                            let sym = sym.as_str().get();

                            input.set("symbol", sym)?;
                            input.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;

                            Ok(input)
                        })
                        .collect();
                    let outputs: LuaResult<Vec<_>> = inline_asm.outputs
                        .into_iter()
                        .map(|inline_asm_output| inline_asm_output.into_lua_ast(ctx, lua_ctx))
                        .collect();
                    let clobbers: Vec<_> = inline_asm.clobbers
                        .into_iter()
                        .map(|sym| sym.as_str().get())
                        .collect();

                    ast.set("kind", "InlineAsm")?;
                    ast.set("asm", inline_asm.asm.as_str().get())?;
                    ast.set("inputs", lua_ctx.create_sequence_from(inputs?.into_iter())?)?;
                    ast.set("outputs", lua_ctx.create_sequence_from(outputs?.into_iter())?)?;
                    ast.set("clobbers", lua_ctx.create_sequence_from(clobbers.into_iter())?)?;
                    ast.set("volatile", inline_asm.volatile)?;
                    ast.set("alignstack", inline_asm.alignstack)?;
                    ast.set("dialect", match inline_asm.dialect {
                        AsmDialect::Att => "Att",
                        AsmDialect::Intel => "Intel",
                    })?;
                    ast.set("ctxt", LuaSyntaxContext(inline_asm.ctxt))?;
                },
                ExprKind::Mac(mac) => {
                    ast.set("kind", "Mac")?;
                    ast.set("path", mac.node.path.into_lua_ast(ctx, lua_ctx)?)?;
                    // TODO: Flesh out further
                },
                ExprKind::Struct(path, fields, opt_expr) => {
                    let fields: LuaResult<Vec<_>> = fields
                        .into_iter()
                        .map(|f| f.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Struct")?;
                    ast.set("path", path.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("fields", fields?)?;

                    if let Some(expr) = opt_expr {
                        ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    }
                },
                ExprKind::Repeat(expr, anon_const) => {
                    ast.set("kind", "Repeat")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("anon_const", anon_const.value.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("id", anon_const.id.as_u32())?;
                },
                ExprKind::Paren(expr) => {
                    ast.set("kind", "Paren")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Try(expr) => {
                    ast.set("kind", "Try")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Yield(opt_val) => {
                    ast.set("kind", "Yield")?;

                    if let Some(val) = opt_val {
                        ast.set("value", val.into_lua_ast(ctx, lua_ctx)?)?;
                    }
                },
                ExprKind::Err => ast.set("kind", "Err")?,
            }

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for FnLike {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnLike")?;
        ast.set("kind", match self.kind {
            FnKind::Normal => "Normal",
            FnKind::ImplMethod => "ImplMethod",
            FnKind::TraitMethod => "TraitMethod",
            FnKind::Foreign => "Foreign",
        })?;
        ast.set("id", self.id.as_u32())?;
        ast.set("ident", self.ident.as_str().get())?;
        ast.set("decl", self.decl.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("span", LuaSpan(self.span.data()))?;

        let block = self.block
            .map(|b| b.into_lua_ast(ctx, lua_ctx))
            .transpose()?;

        ast.set("block", block)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<FnDecl> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnDecl")?;
        ast.set("c_variadic", self.c_variadic)?;

        self.and_then(|fn_decl| {
            ast.set("return_type", match fn_decl.output {
                FunctionRetTy::Default(_) => None,
                FunctionRetTy::Ty(ty) => Some(ty.into_lua_ast(ctx, lua_ctx)?),
            })?;

            let args: LuaResult<Vec<_>> = fn_decl.inputs
                .into_iter()
                .map(|arg| arg.into_lua_ast(ctx, lua_ctx))
                .collect();

            ast.set("args", lua_ctx.create_sequence_from(args?.into_iter())?)?;

            // TODO: self, self kind

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for Arg {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Arg")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("pat", self.pat.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("ty", self.ty.into_lua_ast(ctx, lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Block> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Block")?;
        ast.set("span", LuaSpan(self.span.data()))?;

        self.and_then(|block| {
            let stmts = block.stmts
                .into_iter()
                .map(|stmt| stmt.into_lua_ast(ctx, lua_ctx))
                .collect::<LuaResult<Vec<_>>>();

            ast.set("stmts", lua_ctx.create_sequence_from(stmts?.into_iter())?)?;
            ast.set("rules", match block.rules {
                BlockCheckMode::Default => "Default",
                BlockCheckMode::Unsafe(UnsafeSource::CompilerGenerated) => "CompilerGeneratedUnsafe",
                BlockCheckMode::Unsafe(UnsafeSource::UserProvided) => "UserProvidedUnsafe",
            })?;

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Pat> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Pat")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("span", LuaSpan(self.span.data()))?;

        self.and_then(|pat| {
            match pat.node {
                PatKind::Wild => ast.set("kind", "Wild")?,
                PatKind::Ident(binding, ident, _sub_pattern) => {
                    ast.set("kind", "Ident")?;
                    ast.set("binding", match binding {
                        BindingMode::ByRef(Immutable) => "ByRefImmutable",
                        BindingMode::ByRef(Mutable) => "ByRefMutable",
                        BindingMode::ByValue(Immutable) => "ByValueImmutable",
                        BindingMode::ByValue(Mutable) => "ByValueMutable",
                    })?;
                    ast.set("ident", ident.as_str().get())?;
                },
                PatKind::Tuple(patterns, fragment_pos) => {
                    let pats = patterns
                        .into_iter()
                        .map(|pat| pat.into_lua_ast(ctx, lua_ctx))
                        .collect::<LuaResult<Vec<_>>>();

                    ast.set("kind", "Tuple")?;
                    ast.set("pats", lua_ctx.create_sequence_from(pats?.into_iter())?)?;
                    ast.set("fragment_pos", fragment_pos)?;
                },
                PatKind::Lit(expr) => {
                    ast.set("kind", "Lit")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                _ => return Err(LuaError::external(format!("unimplemented pattern type: {:?}", pat.node))),
            }

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for Crate {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Crate")?;
        ast.set("module", self.module.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("span", LuaSpan(self.span.data()))?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for Mod {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        let items = self.items
            .into_iter()
            .map(|item| item.into_lua_ast(ctx, lua_ctx))
            .collect::<LuaResult<Vec<_>>>();

        ast.set("type", "Mod")?;
        ast.set("inline", self.inline)?;
        ast.set("items", lua_ctx.create_sequence_from(items?.into_iter())?)?;
        ast.set("inner", LuaSpan(self.inner.data()))?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Item> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Item")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("ident", self.ident.name.as_str().get())?;
        ast.set("span", LuaSpan(self.span.data()))?;

        self.and_then(|item| {
            match item.node {
                ItemKind::ExternCrate(opt_name) => {
                    ast.set("kind", "ExternCrate")?;

                    if let Some(name) = opt_name {
                        ast.set("name", name.as_str().get())?;
                    }
                },
                ItemKind::Mod(module) => {
                    ast.set("kind", "Mod")?;
                    ast.set("mod", module.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ItemKind::Use(use_tree) => {
                    ast.set("kind", "Use")?;
                    ast.set("tree", use_tree.into_inner().into_lua_ast(ctx, lua_ctx)?)?;
                },
                ItemKind::Fn(decl, header, _generics, block) => {
                    ast.set("kind", "Fn")?;
                    ast.set("decl", decl.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("unsafety", match header.unsafety {
                        Unsafety::Unsafe => "Unsafe",
                        Unsafety::Normal => "Normal",
                    })?;
                    ast.set("is_const", match header.constness.node {
                        Constness::Const => true,
                        Constness::NotConst => false,
                    })?;
                    ast.set("abi", header.abi.name())?;
                },
                ItemKind::Struct(_variant_data, _generics) => {
                    ast.set("kind", "Struct")?;
                    // TODO: Variant data, generics
                },
                ItemKind::Impl(.., ty, items) => {
                    let items = items
                        .into_iter()
                        .map(|item| item.into_lua_ast(ctx, lua_ctx))
                        .collect::<LuaResult<Vec<_>>>();

                    ast.set("kind", "Impl")?;
                    ast.set("items", lua_ctx.create_sequence_from(items?.into_iter())?)?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;

                    // TODO: other fields
                },
                ItemKind::Union(..) => {
                    ast.set("kind", "Union")?;

                    // TODO: More fields
                },
                ItemKind::Trait(..) => {
                    ast.set("kind", "Trait")?;

                    // TODO: More fields
                },
                ItemKind::Mac(mac) => {
                    ast.set("kind", "Mac")?;
                    ast.set("path", mac.node.path.into_lua_ast(ctx, lua_ctx)?)?;

                    // TODO: More fields
                },
                ItemKind::MacroDef(..) => {
                    ast.set("kind", "MacroDef")?;

                    // TODO: More fields
                },
                ItemKind::Ty(ty, _generics) => {
                    ast.set("kind", "Ty")?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;

                    // TODO: More fields
                },
                ItemKind::ForeignMod(..) => {
                    ast.set("kind", "ForeignMod")?;

                    // TODO: More fields
                },
                ItemKind::TraitAlias(..) => {
                    ast.set("kind", "TraitAlias")?;

                    // TODO: More fields
                },
                ItemKind::GlobalAsm(..) => {
                    ast.set("kind", "GlobalAsm")?;

                    // TODO: More fields
                },
                ItemKind::Existential(..) => {
                    ast.set("kind", "Existential")?;

                    // TODO: More fields
                },
                ItemKind::Enum(_def, _generics) => {
                    ast.set("kind", "Enum")?;
                    // TODO: More fields
                },
                ItemKind::Static(ty, mutability, expr) => {
                    ast.set("kind", "Static")?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("mutability", match mutability {
                        Immutable => "Immutable",
                        Mutable => "Mutable",
                    })?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ItemKind::Const(..) => {
                    ast.set("kind", "Const")?;

                    // TODO: More fields
                },
            }

            Ok(ast)
        })

    }
}

impl<'lua> IntoLuaAst<'lua> for ImplItem {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "ImplItem")?;
        ast.set("ident", self.ident.as_str().get())?;
        ast.set("span", LuaSpan(self.span.data()))?;

        match self.node {
            ImplItemKind::Method(sig, block) => {
                ast.set("kind", "ImplMethod")?;
                ast.set("decl", sig.decl.into_lua_ast(ctx, lua_ctx)?)?;
                ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;
                // TODO: generics, attrs, ..
            },
            ref e => unimplemented!("IntoLuaAst for {:?}", e),
        }

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for InlineAsmOutput {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        let constraint = self.constraint.as_str().get();
        let expr = self.expr.into_lua_ast(ctx, lua_ctx)?;

        ast.set("type", "InlineAsmOutput")?;
        ast.set("constraint", constraint)?;
        ast.set("expr", expr)?;
        ast.set("is_indirect", self.is_indirect)?;
        ast.set("is_rw", self.is_rw)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for Arm {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        let pats = self.pats
            .into_iter()
            .map(|pat| pat.into_lua_ast(ctx, lua_ctx))
            .collect::<LuaResult<Vec<_>>>();

        ast.set("type", "Arm")?;
        ast.set("body", self.body.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("pats", lua_ctx.create_sequence_from(pats?.into_iter())?)?;

        if let Some(guard) = self.guard {
            ast.set("guard", match guard {
                Guard::If(expr) => expr.into_lua_ast(ctx, lua_ctx)?,
            })?;
        }

        // TODO: More fields

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Ty> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Ty")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("span", LuaSpan(self.span.data()))?;

        self.and_then(|ty| {
            match ty.node {
                TyKind::Path(_opt_qself, path) => {
                    ast.set("kind", "Path")?;
                    ast.set("path", path.into_lua_ast(ctx, lua_ctx)?)?;
                    // TODO: Option<QSelf>
                },
                TyKind::Ptr(mut_ty) => {
                    ast.set("kind", "Ptr")?;
                    ast.set("ty", mut_ty.ty.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("mutbl", match mut_ty.mutbl {
                        Immutable => "Immutable",
                        Mutable => "Mutable",
                    })?;
                },
                TyKind::BareFn(bare_fn) => {
                    let BareFnTy {unsafety, abi, decl, ..} = bare_fn.into_inner();

                    ast.set("kind", "BareFn")?;
                    ast.set("unsafety", match unsafety {
                        Unsafety::Unsafe => "Unsafe",
                        Unsafety::Normal => "Normal",
                    })?;
                    ast.set("abi", abi.name())?;
                    ast.set("decl", decl.into_lua_ast(ctx, lua_ctx)?)?;

                    // TODO: GenericParams
                },
                TyKind::Array(ty, anon_const) => {
                    ast.set("kind", "Array")?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("anon_const", anon_const.value.into_lua_ast(ctx, lua_ctx)?)?;
                },
                TyKind::Rptr(opt_lifetime, mut_ty) => {
                    ast.set("kind", "Rptr")?;
                    ast.set("ty", mut_ty.ty.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("mutbl", match mut_ty.mutbl {
                        Immutable => "Immutable",
                        Mutable => "Mutable",
                    })?;

                    if let Some(lifetime) = opt_lifetime {
                        ast.set("lifetime", lifetime.ident.as_str().get())?;
                    }
                },
                TyKind::Typeof(anon_const) => {
                    ast.set("kind", "Typeof")?;
                    ast.set("anon_const", anon_const.value.into_lua_ast(ctx, lua_ctx)?)?;
                },
                TyKind::Paren(ty) => {
                    ast.set("kind", "Paren")?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
                },
                TyKind::Slice(ty) => {
                    ast.set("kind", "Slice")?;
                    ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
                },
                TyKind::Tup(types) => {
                    let tys = types
                        .into_iter()
                        .map(|ty| ty.into_lua_ast(ctx, lua_ctx))
                        .collect::<LuaResult<Vec<_>>>();

                    ast.set("kind", "Tup")?;
                    ast.set("tys", lua_ctx.create_sequence_from(tys?.into_iter())?)?;
                },
                TyKind::Never => ast.set("kind", "Never")?,
                TyKind::ImplicitSelf => ast.set("kind", "ImplicitSelf")?,
                TyKind::CVarArgs => ast.set("kind", "CVarArgs")?,
                TyKind::Infer => ast.set("kind", "Infer")?,
                e => unimplemented!("IntoLuaAst unimplemented ty kind: {:?}", e),
            }

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for Field {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Field")?;
        ast.set("ident", self.ident.as_str().get())?;
        ast.set("expr", self.expr.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("is_shorthand", self.is_shorthand)?;
        ast.set("span", LuaSpan(self.span.data()))?;
        // TODO: Attrs

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for Path {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let segments: LuaResult<Vec<_>> = self
            .segments
            .into_iter()
            .map(|segment| segment.into_lua_ast(ctx, lua_ctx))
            .collect();

        lua_ctx.create_sequence_from(segments?.into_iter())
    }
}

impl<'lua> IntoLuaAst<'lua> for PathSegment {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "PathSegment")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("ident", self.ident.as_str().get())?;

        if let Some(generics) = self.args {
            generics.and_then(|generics| {
                ast.set("generics", generics.into_lua_ast(ctx, lua_ctx)?)
            })?;
        }

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for GenericArgs {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "GenericArgs")?;
        ast.set("span", LuaSpan(self.span().data()))?;

        match self {
            GenericArgs::AngleBracketed(args) => {
                let args = args.args
                    .into_iter()
                    .map(|arg| arg.into_lua_ast(ctx, lua_ctx))
                    .collect::<LuaResult<Vec<_>>>();

                ast.set("kind", "AngleBracketed")?;
                ast.set("args", args?)?;
                // TODO: Constraints
            },
            GenericArgs::Parenthesized(_args) => {
                ast.set("kind", "Parenthesized")?;

                // TODO
            },
        }

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for GenericArg {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "GenericArg")?;

        match self {
            GenericArg::Type(ty) => {
                ast.set("kind", "Type")?;
                ast.set("ty", ty.into_lua_ast(ctx, lua_ctx)?)?;
            },
            e => warn!("IntoLuaAst unimplemented GenericArg: {:?}", e),
        }

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for UseTree {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "UseTree")?;
        ast.set("prefix", self.prefix.into_lua_ast(ctx, lua_ctx)?)?;
        ast.set("span", LuaSpan(self.span.data()))?;

        match self.kind {
            UseTreeKind::Simple(opt_ident, ..) => {
                ast.set("kind", "Simple")?;

                if let Some(ident) = opt_ident {
                    ast.set("ident", ident.name.as_str().get())?;
                }
            },
            UseTreeKind::Nested(use_trees) => {
                let trees = use_trees
                    .into_iter()
                    .map(|(tree, _)| tree.into_lua_ast(ctx, lua_ctx))
                    .collect::<LuaResult<Vec<_>>>();

                ast.set("kind", "Nested")?;
                ast.set("trees", lua_ctx.create_sequence_from(trees?.into_iter())?)?;
            },
            UseTreeKind::Glob => ast.set("kind", "Glob")?,
        }

        Ok(ast)
    }
}
