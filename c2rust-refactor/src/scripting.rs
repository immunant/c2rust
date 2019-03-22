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
use syntax::ast;
use syntax::ptr::P;

use crate::command::{self, CommandState, RefactorState};
use crate::driver::{self, Phase};
use crate::file_io::{ArcFileIO, OutputMode, RealFileIO};
use crate::matcher::{self, mut_visit_match_with, Bindings, MatchCtxt, Pattern, Subst, TryMatch};
use crate::RefactorCtxt;

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

    driver::run_compiler(config, Some(Box::new(ArcFileIO(io.clone()))), |compiler| {
        let lua = Lua::new();
        lua.context(|lua_ctx| {
            lua_ctx.scope(|scope| {
                let state = RefactorState::new(compiler, registry, io, HashSet::new());
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
    fn into_lua_ast(self, ctx: &TransformCtxt, ast: &mut LuaTable<'lua>) -> LuaResult<()>;
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
    fn into_lua_ast(self, ctx: &TransformCtxt, ast: &mut LuaTable<'lua>) -> LuaResult<()> {
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
                    ast.set("init", ctx.intern(init))?;
                }
            }
            ast::StmtKind::Item(i) => {
                ast.set("kind", "Item")?;
                ast.set("item", ctx.intern(i))?;
            }
            ast::StmtKind::Semi(e) => {
                ast.set("kind", "Semi")?;
                ast.set("expr", ctx.intern(e))?;
            }
            ast::StmtKind::Expr(e) => {
                ast.set("kind", "Expr")?;
                ast.set("expr", ctx.intern(e))?;
            }
            ast::StmtKind::Mac(_) => {
                return Err(LuaError::external("StmtKind::Mac is not yet implemented"));
            }
        }

        Ok(())
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
    fn into_lua_ast(self, _ctx: &TransformCtxt, ast: &mut LuaTable<'lua>) -> LuaResult<()> {
        ast.set("type", "Expr")?;
        self.and_then(|expr| {
            match expr.node {
                ast::ExprKind::Lit(l) => {
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
                }
                _ => {
                    return Err(LuaError::external(format!(
                        "{:?} is not yet implemented",
                        expr.node
                    )));
                }
            }

            Ok(())
        })
    }
}

/// Refactoring context
// @type RefactorState
#[allow(unused_doc_comments)]
impl<'a> UserData for RefactorState<'a> {
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
                    let krate: LuaAstNode = callback.call::<_, LuaAstNode>((transform_data, krate))?;
                    Ok(ast::Crate::try_from(transform.remove_ast(krate)).unwrap())
                });
                let new_krate = res.unwrap_or_else(|e| panic!("Could not run transform: {:#?}", e));
                *st.krate_mut() = new_krate;
            }).map_err(|e| LuaError::external(format!("Failed to run compiler: {:#?}", e)))?;
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
        let mut ast = lua_ctx.create_table()?;
        match self.clone_ast(node) {
            RustAstNode::Stmt(s) => s.into_lua_ast(self, &mut ast)?,
            RustAstNode::Expr(e) => e.into_lua_ast(self, &mut ast)?,
            _ => {
                return Err(LuaError::external(
                    "get_ast not implemented for this type of RustAstNode",
                ));
            }
        }
        Ok(ast)
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
                        *pat = this.nodes
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
                        *pat = this.nodes
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
    }
}
