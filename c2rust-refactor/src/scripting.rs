use std::cell::RefCell;
use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, Read};
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;

use derive_more::{From, TryInto};
use rlua::prelude::{LuaContext, LuaError, LuaFunction, LuaResult, LuaTable};
use rlua::{Lua, UserData, UserDataMethods};
use rustc_interface::interface;
use slotmap::{new_key_type, SlotMap};
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

use crate::ast_manip::fn_edit::mut_visit_fns;
use crate::command::{self, CommandState, RefactorState};
use crate::driver::{self, Phase};
use crate::file_io::{OutputMode, RealFileIO};
use crate::matcher::{self, mut_visit_match_with, Bindings, MatchCtxt, Pattern, Subst, TryMatch};
use crate::RefactorCtxt;
use crate::scripting::utils::iter_to_lua_array;

pub mod ast_visitor;
pub mod utils;

use ast_visitor::MergeLuaAst;

/// Refactoring module
// @module Refactor

/// Global refactoring state
// @field refactor RefactorState object

pub fn run_lua_file(
    script_path: &Path,
    config: interface::Config,
    registry: command::Registry,
    rewrite_modes: Vec<OutputMode>,
) -> io::Result<()> {
    let mut file = File::open(script_path)?;
    let mut script = vec![];
    file.read_to_end(&mut script)?;
    let io = Arc::new(RealFileIO::new(rewrite_modes));

    driver::run_refactoring(config, registry, io, HashSet::new(), |state| {
        let lua = Lua::new();
        lua.context(|lua_ctx| {
            lua_ctx.scope(|scope| {
                let refactor = scope.create_nonstatic_userdata(state)?;
                lua_ctx.globals().set("refactor", refactor)?;

                lua_ctx.load(&script).exec()
            })
        })
    })
    .unwrap_or_else(|e| panic!("User script failed: {:#?}", e));

    Ok(())
}

trait IntoLuaAst<'lua> {
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
impl<'lua> IntoLuaAst<'lua> for ast::Stmt {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        ast.set("type", "Stmt")?;
        match self.node {
            ast::StmtKind::Local(l) => {
                ast.set("kind", "Local")?;
                let ast::Local { pat, ty, init, .. } = l.into_inner();
                ast.set("pat", ctx.intern(pat))?;
                if let Some(ty) = ty {
                    ast.set("ty", ctx.intern(ty))?;
                }
                if let Some(init) = init {
                    ast.set("init", init.into_lua_ast(ctx, lua_ctx)?)?;
                }
            }
            ast::StmtKind::Item(i) => {
                ast.set("kind", "Item")?;
                ast.set("item", ctx.intern(i))?;
            }
            ast::StmtKind::Semi(e) => {
                ast.set("kind", "Semi")?;
                ast.set("expr", e.into_lua_ast(ctx, lua_ctx)?)?;
            }
            ast::StmtKind::Expr(e) => {
                ast.set("kind", "Expr")?;
                ast.set("expr", e.into_lua_ast(ctx, lua_ctx)?)?;
            }
            ast::StmtKind::Mac(_) => {
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
impl<'lua> IntoLuaAst<'lua> for P<ast::Expr> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        ast.set("type", "Expr")?;
        ast.set("id", self.id.as_u32())?;

        self.and_then(|expr| {
            match expr.node {
                ExprKind::Lit(l) => {
                    ast.set("kind", "Lit")?;
                    match l.node {
                        ast::LitKind::Str(s, _) => ast.set("value", s.to_string())?,
                        ast::LitKind::Int(i, _) => ast.set("value", i)?,
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

                },
                ExprKind::Array(values) => {
                    let vals: LuaResult<Vec<_>> = values
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Array")?;
                    ast.set("values", iter_to_lua_array(vals?.into_iter(), lua_ctx)?)?;
                },
                ExprKind::Call(path, args) => {
                   let args: LuaResult<Vec<_>> = args
                        .into_iter()
                        .map(|v| v.into_lua_ast(ctx, lua_ctx))
                        .collect();

                    ast.set("kind", "Call")?;
                    ast.set("path", path.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("args", iter_to_lua_array(args?.into_iter(), lua_ctx)?)?;
                },
                ExprKind::MethodCall(_, _) => {
                    ast.set("kind", "MethodCall")?;

                },
                ExprKind::Tup(_) => {
                    ast.set("kind", "Tup")?;

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
                ExprKind::Cast(_, _) => {
                    ast.set("kind", "Cast")?;

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
                },
                ExprKind::While(cond, _, _) => {
                    ast.set("kind", "While")?;
                    ast.set("cond", cond.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::WhileLet(_pats, expr, block, opt_label) => {
                    ast.set("kind", "WhileLet")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

                    if let Some(label) = opt_label {
                        ast.set("label", label.ident.name.as_str().get())?;
                    }
                },
                ExprKind::ForLoop(_pat, expr, block, opt_label) => {
                    ast.set("kind", "ForLoop")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;

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
                ExprKind::Match(expr, _arms) => {
                    ast.set("kind", "Match")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ExprKind::Closure(capture_by, _is_async, movability, fn_decl, expr, _span) => {
                    ast.set("kind", "Closure")?;
                    ast.set("fn_decl", fn_decl.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("capture_by", match capture_by {
                        ast::CaptureBy::Ref => "Ref",
                        ast::CaptureBy::Value => "Value",
                    })?;
                    ast.set("movability", match movability {
                        ast::Movability::Movable => "Movable",
                        ast::Movability::Static => "Static",
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
                ExprKind::Field(..) => {
                    ast.set("kind", "Field")?;
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
                        ast::RangeLimits::HalfOpen => "HalfOpen",
                        ast::RangeLimits::Closed => "Closed",
                    })?;
                },
                ExprKind::Path(_, path) => {
                    let segments = path
                        .segments
                        .into_iter()
                        .map(|s| s.ident.to_string());

                    ast.set("kind", "Path")?;
                    ast.set("segments", iter_to_lua_array(segments, lua_ctx)?)?;
                },
                ExprKind::AddrOf(mutability, expr) => {
                    ast.set("kind", "AddrOf")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("mutability", match mutability {
                        ast::Mutability::Immutable => "Immutable",
                        ast::Mutability::Mutable => "Mutable",
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
                ExprKind::InlineAsm(..) => {
                    ast.set("kind", "InlineAsm")?;

                },
                ExprKind::Mac(..) => {
                    ast.set("kind", "Mac")?;

                },
                ExprKind::Struct(..) => {
                    ast.set("kind", "Struct")?;

                },
                ExprKind::Repeat(expr, _anon_const) => {
                    ast.set("kind", "Repeat")?;
                    ast.set("expr", expr.into_lua_ast(ctx, lua_ctx)?)?;
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
                ExprKind::Err => {
                    ast.set("kind", "Err")?;
                },
            }

            Ok(ast)
        })
    }
}

/// Refactoring context
// @type RefactorState
#[allow(unused_doc_comments)]
impl UserData for RefactorState {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Run a builtin refactoring command
        // @function run_command
        // @tparam string name Command to run
        // @tparam {string,...} args List of arguments for the command
        methods.add_method_mut(
            "run_command",
            |_lua_ctx, this, (name, args): (String, Vec<String>)| {
                this.load_crate();
                let res = this.run(&name, &args).map_err(|e| LuaError::external(e));
                this.save_crate();
                res
            },
        );

        /// Run a custom refactoring transformation
        // @function transform
        // @tparam function(TransformCtxt,LuaAstNode) callback Transformation function called with a fresh @{TransformCtxt} and the crate to be transformed.
        methods.add_method_mut("transform", |lua_ctx, this, callback: LuaFunction| {
            this.load_crate();
            this.transform_crate(Phase::Phase2, |st, cx| {
                let transform = TransformCtxt::new(st, cx);
                let res: LuaResult<ast::Crate> = lua_ctx.scope(|scope| {
                    let krate = transform.intern(st.krate().clone());
                    let transform_data = scope.create_nonstatic_userdata(transform.clone())?;
                    let krate: LuaAstNode =
                        callback.call::<_, LuaAstNode>((transform_data, krate))?;
                    Ok(ast::Crate::try_from(transform.remove_ast(krate)).unwrap())
                });
                let new_krate = res.unwrap_or_else(|e| panic!("Could not run transform: {:#?}", e));
                *st.krate_mut() = new_krate;
            })
            .map_err(|e| LuaError::external(format!("Failed to run compiler: {:#?}", e)))?;
            this.save_crate();
            Ok(())
        });
    }
}

#[derive(Clone, Debug, From, TryInto)]
enum RustAstNode {
    Crate(ast::Crate),
    Expr(P<ast::Expr>),
    Pat(P<ast::Pat>),
    Ty(P<ast::Ty>),
    Stmts(Vec<ast::Stmt>),
    Stmt(ast::Stmt),
    Item(P<ast::Item>),
}

impl TryMatch for RustAstNode {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        match self {
            RustAstNode::Crate(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Expr(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Pat(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Ty(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Stmts(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Stmt(x) => mcx.try_match(x, target.try_into().unwrap()),
            RustAstNode::Item(x) => mcx.try_match(x, target.try_into().unwrap()),
        }
    }
}

// These impls should get auto-derived, see
// https://github.com/JelteF/derive_more/issues/69
impl<'a> TryFrom<&'a RustAstNode> for &'a ast::Crate {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Crate(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Crate can be converted to &Crate"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a P<ast::Expr> {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Expr(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Expr can be converted to &P<Expr>"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a P<ast::Pat> {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Pat(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Pat can be converted to &P<Pat>"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a P<ast::Ty> {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Ty(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Ty can be converted to &P<Ty>"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a Vec<ast::Stmt> {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Stmts(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Stmts can be converted to &Vec<Stmt>"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a ast::Stmt {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Stmt(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Stmt can be converted to &Stmt"),
        }
    }
}

impl<'a> TryFrom<&'a RustAstNode> for &'a P<ast::Item> {
    type Error = &'static str;
    fn try_from(value: &'a RustAstNode) -> Result<Self, Self::Error> {
        match value {
            RustAstNode::Item(ref x) => Ok(x),
            _ => Err("Only &RustAstNode::Item can be converted to &Item"),
        }
    }
}

impl Subst for RustAstNode {
    fn subst(self, st: &CommandState, cx: &RefactorCtxt, bindings: &Bindings) -> Self {
        match self {
            RustAstNode::Crate(_) => panic!("Can't subst Crates"),
            RustAstNode::Expr(x) => RustAstNode::Expr(x.subst(st, cx, bindings)),
            RustAstNode::Pat(x) => RustAstNode::Pat(x.subst(st, cx, bindings)),
            RustAstNode::Ty(x) => RustAstNode::Ty(x.subst(st, cx, bindings)),
            RustAstNode::Stmts(x) => RustAstNode::Stmts(x.subst(st, cx, bindings)),
            RustAstNode::Stmt(x) => RustAstNode::Stmt(x.subst(st, cx, bindings)),
            RustAstNode::Item(x) => RustAstNode::Item(x.subst(st, cx, bindings)),
        }
    }
}

new_key_type! { struct LuaAstNode; }

/// AST node handle
// @type LuaAstNode
impl UserData for LuaAstNode {}

#[derive(Clone)]
struct ScriptingMatchCtxt<'a, 'tcx: 'a> {
    mcx: MatchCtxt<'a, 'tcx>,
    transform: TransformCtxt<'a, 'tcx>,
}

impl<'a, 'tcx> ScriptingMatchCtxt<'a, 'tcx> {
    fn new(transform: TransformCtxt<'a, 'tcx>) -> Self {
        Self {
            mcx: MatchCtxt::new(transform.st, transform.cx),
            transform,
        }
    }

    fn wrap(transform: TransformCtxt<'a, 'tcx>, mcx: MatchCtxt<'a, 'tcx>) -> Self {
        Self { mcx, transform }
    }

    fn fold_with<'lua, P, V>(
        &self,
        lua_ctx: LuaContext<'lua>,
        pattern: P,
        krate: &mut ast::Crate,
        callback: LuaFunction<'lua>,
    ) where
        P: Pattern<V>,
        V: TryFrom<RustAstNode> + Into<RustAstNode> + Clone,
        <V as TryFrom<RustAstNode>>::Error: Debug,
    {
        mut_visit_match_with(self.mcx.clone(), pattern, krate, |x, mcx| {
            let orig_node = self.transform.intern(x.clone());
            let mcx = ScriptingMatchCtxt::wrap(self.transform.clone(), mcx);
            let new_node = lua_ctx
                .scope(|scope| {
                    let mcx = scope.create_nonstatic_userdata(mcx)?;
                    callback.call::<_, LuaAstNode>((orig_node, mcx))
                })
                .unwrap_or_else(|e| {
                    panic!("Could not execute callback in match:fold_with {:#?}", e)
                });
            *x = self.transform.remove_ast(new_node).try_into().unwrap();
        })
    }
}

impl<'a, 'tcx> Deref for ScriptingMatchCtxt<'a, 'tcx> {
    type Target = MatchCtxt<'a, 'tcx>;
    fn deref(&self) -> &MatchCtxt<'a, 'tcx> {
        &self.mcx
    }
}

impl<'a, 'tcx> DerefMut for ScriptingMatchCtxt<'a, 'tcx> {
    fn deref_mut(&mut self) -> &mut MatchCtxt<'a, 'tcx> {
        &mut self.mcx
    }
}

/// A match context
// @type MatchCtxt
#[allow(unused_doc_comments)]
impl<'a, 'tcx> UserData for ScriptingMatchCtxt<'a, 'tcx> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Parse statements and add them to this MatchCtxt
        // @function parse_stmts
        // @tparam string pat Pattern to parse
        // @treturn LuaAstNode The parsed statements
        methods.add_method_mut("parse_stmts", |_lua_ctx, this, pat: String| {
            let stmts = this.parse_stmts(&pat);
            Ok(this.transform.intern(stmts))
        });

        /// Parse an expression and add it to this MatchCtxt
        // @function parse_expr
        // @tparam string pat Pattern to parse
        // @treturn LuaAstNode The parsed expression
        methods.add_method_mut("parse_expr", |_lua_ctx, this, pat: String| {
            let expr = this.parse_expr(&pat);
            Ok(this.transform.intern(expr))
        });

        /// Find matches of `pattern` within `crate` and rewrite using `callback`
        // @function fold_with
        // @tparam LuaAstNode needle Pattern to search for
        // @tparam LuaAstNode crate Crate to fold over
        // @tparam function(LuaAstNode,MatchCtxt) callback Function called for each match. Takes the matching node and a new @{MatchCtxt} for that match.
        methods.add_method(
            "fold_with",
            |lua_ctx, this, (needle, krate, f): (LuaAstNode, LuaAstNode, LuaFunction)| {
                let mut krate = ast::Crate::try_from(this.transform.remove_ast(krate)).unwrap();
                match this.transform.remove_ast(needle).clone() {
                    RustAstNode::Expr(pattern) => this.fold_with(lua_ctx, pattern, &mut krate, f),
                    RustAstNode::Ty(pattern) => this.fold_with(lua_ctx, pattern, &mut krate, f),
                    RustAstNode::Stmts(pattern) => this.fold_with(lua_ctx, pattern, &mut krate, f),
                    _ => return Err(LuaError::external("Unexpected Ast node type")),
                }
                Ok(this.transform.intern(krate))
            },
        );

        /// Get matched binding for an expression variable
        // @function get_expr
        // @tparam string Expression variable pattern
        // @treturn LuaAstNode Expression matched by this binding
        methods.add_method("get_expr", |_lua_ctx, this, pattern: String| {
            Ok(this.transform.intern(
                this.bindings
                    .get::<_, P<ast::Expr>>(pattern)
                    .unwrap()
                    .clone(),
            ))
        });

        /// Get matched binding for a statement variable
        // @function get_stmt
        // @tparam string Statement variable pattern
        // @treturn LuaAstNode Statement matched by this binding
        methods.add_method("get_stmt", |_lua_ctx, this, pattern: String| {
            Ok(this
                .transform
                .intern(this.bindings.get::<_, ast::Stmt>(pattern).unwrap().clone()))
        });

        /// Attempt to match `target` against `pat`, updating bindings if matched.
        // @function try_match
        // @tparam LuaAstNode pat AST (potentially with variable bindings) to match with
        // @tparam LuaAstNode target AST to match against
        // @treturn bool true if match was successful
        methods.add_method_mut(
            "try_match",
            |_lua_ctx, this, (pat, target): (LuaAstNode, LuaAstNode)| {
                let pat = this.transform.clone_ast(pat);
                let target = this.transform.clone_ast(target);
                Ok(this.try_match(&pat, &target).is_ok())
            },
        );

        /// Substitute the currently matched AST node with a new AST node
        // @function subst
        // @tparam LuaAstNode replacement New AST node to replace the currently matched AST. May include variable bindings if these bindings were matched by the search pattern.
        // @treturn LuaAstNode New AST node with variable bindings replaced by their matched values
        methods.add_method("subst", |_lua_ctx, this, node: LuaAstNode| {
            let node = this.transform.clone_ast(node);
            let new_node = node.subst(this.transform.st, this.transform.cx, &this.bindings);

            Ok(this.transform.intern(new_node))
        });
    }
}

#[derive(Clone)]
struct TransformCtxt<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    nodes: Rc<RefCell<SlotMap<LuaAstNode, RustAstNode>>>,
}

impl<'a, 'tcx> TransformCtxt<'a, 'tcx> {
    fn new(st: &'a CommandState, cx: &'a RefactorCtxt<'a, 'tcx>) -> Self {
        Self {
            st,
            cx,
            nodes: Rc::new(RefCell::new(SlotMap::with_key())),
        }
    }

    fn clone_ast(&self, node: LuaAstNode) -> RustAstNode {
        self.nodes.borrow().get(node).unwrap().clone()
    }

    fn remove_ast(&self, node: LuaAstNode) -> RustAstNode {
        self.nodes.borrow_mut().remove(node).unwrap()
    }

    fn intern<T>(&self, v: T) -> LuaAstNode
    where
        T: Into<RustAstNode>,
    {
        self.nodes.borrow_mut().insert(v.into())
    }

    fn get_lua_ast<'lua>(
        &self,
        lua_ctx: LuaContext<'lua>,
        node: LuaAstNode,
    ) -> LuaResult<LuaTable<'lua>> {
        match self.clone_ast(node) {
            RustAstNode::Stmt(s) => s.into_lua_ast(self, lua_ctx),
            RustAstNode::Expr(e) => e.into_lua_ast(self, lua_ctx),
            _ => Err(LuaError::external(
                "get_ast not implemented for this type of RustAstNode",
            )),
        }
    }
}

/// Transformation context
// @type TransformCtxt
#[allow(unused_doc_comments)]
impl<'a, 'tcx> UserData for TransformCtxt<'a, 'tcx> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Replace matching statements using given callback
        // @function replace_stmts_with
        // @tparam string needle Statements pattern to search for, may include variable bindings
        // @tparam function(LuaAstNode,MatchCtxt) callback Function called for each match. Takes the matching node and a new @{MatchCtxt} for that match. See @{MatchCtxt:fold_with}
        methods.add_method_mut(
            "replace_stmts_with",
            |_lua_ctx, this, (pat, f): (String, LuaFunction)| {
                this.st.map_krate(|krate| {
                    let mut mcx = MatchCtxt::new(this.st, this.cx);
                    let pat = mcx.parse_stmts(&pat);
                    mut_visit_match_with(mcx, pat, krate, |pat, _mcx| {
                        let i = f.call::<_, LuaAstNode>(this.intern(pat.clone())).unwrap();
                        *pat = this
                            .nodes
                            .borrow_mut()
                            .remove(i)
                            .unwrap()
                            .try_into()
                            .unwrap();
                    })
                });
                Ok(())
            },
        );

        /// Replace matching expressions using given callback
        // @function replace_expr_with
        // @tparam string needle Expression pattern to search for, may include variable bindings
        // @tparam function(LuaAstNode,MatchCtxt) callback Function called for each match. Takes the matching node and a new @{MatchCtxt} for that match. See @{MatchCtxt:fold_with}
        methods.add_method_mut(
            "replace_expr_with",
            |_lua_ctx, this, (pat, f): (String, LuaFunction)| {
                this.st.map_krate(|krate| {
                    let mut mcx = MatchCtxt::new(this.st, this.cx);
                    let pat = mcx.parse_expr(&pat);
                    mut_visit_match_with(mcx, pat, krate, |pat, _mcx| {
                        let i = f.call::<_, LuaAstNode>(this.intern(pat.clone())).unwrap();
                        *pat = this
                            .nodes
                            .borrow_mut()
                            .remove(i)
                            .unwrap()
                            .try_into()
                            .unwrap();
                    })
                });
                Ok(())
            },
        );

        /// Create a new, empty @{MatchCtxt}
        // @function match
        // @treturn MatchCtxt New match context
        methods.add_method("match", |lua_ctx, this, f: LuaFunction| {
            let init_mcx = ScriptingMatchCtxt::new(this.clone());
            lua_ctx.scope(|scope| {
                let init_mcx = scope.create_nonstatic_userdata(init_mcx)?;
                f.call::<_, LuaAstNode>(init_mcx)
            })
        });

        methods.add_method("debug", |_lua_ctx, this, ()| {
            eprintln!("intern table: {:?}", this.nodes);
            Ok(())
        });

        /// Retrieve a Lua version of an AST node
        // @function get_ast
        // @tparam LuaAstNode node AST node handle
        // @return Struct representation of this AST node. Valid return types are @{Stmt}, and @{Expr}.
        methods.add_method("get_ast", |lua_ctx, this, node: LuaAstNode| {
            this.get_lua_ast(lua_ctx, node)
        });

        /// Visits all function like items
        // @function visit_fn_like
        // @tparam function() callback Function called for each function like item.
        methods.add_method_mut("visit_fn_like", |lua_ctx, this, (krate, callback): (LuaAstNode, LuaFunction)| {
            let mut krate = ast::Crate::try_from(this.remove_ast(krate)).expect("Did not find crate input");
            let mut found_err = Ok(());

            mut_visit_fns(&mut krate, |fn_like| {
                if found_err.is_err() {
                    return;
                }

                // REVIEW: Maybe this can be cleaned up by doing this in
                // a result returning function?
                let lua_fn_like = match fn_like.clone().into_lua_ast(this, lua_ctx) {
                    Ok(lfl) => lfl,
                    Err(e) => {
                        found_err = Err(e);

                        return
                    },
                };

                let ret_lua_fn_like: LuaTable = match callback.call(lua_fn_like) {
                    Ok(lfl) => lfl,
                    Err(e) => {
                        found_err = Err(e);

                        return
                    },
                };

                match fn_like.merge_lua_ast(ret_lua_fn_like) {
                    Ok(()) => (),
                    Err(e) => {
                        found_err = Err(e);

                        return
                    },
                };
            });

            found_err.map(|_| this.intern(krate))
        });
    }
}
