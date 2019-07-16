use std::collections::HashSet;
use std::fmt;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::sync::Arc;

use rlua::prelude::{LuaContext, LuaError, LuaFunction, LuaResult, LuaString, LuaTable, LuaValue};
use rlua::{AnyUserData, FromLua, Lua, UserData, UserDataMethods};
use rustc_interface::interface;
use syntax::ast::{self, DUMMY_NODE_ID};
use syntax::mut_visit::MutVisitor;
use syntax::ptr::P;

use c2rust_ast_builder::mk;
use crate::ast_manip::MutVisit;
use crate::ast_manip::fn_edit::{mut_visit_fns, FnLike};
use crate::command::{self, CommandState, RefactorState};
use crate::driver::{self, Phase};
use crate::file_io::{OutputMode, RealFileIO};
use crate::matcher::{mut_visit_match_with, MatchCtxt, Pattern, Subst, TryMatch};
use crate::path_edit::fold_resolved_paths_with_id;
use crate::RefactorCtxt;

pub mod ast_visitor;
pub mod into_lua_ast;
pub mod merge_lua_ast;
mod to_lua_ast_node;

use ast_visitor::{LuaAstVisitor, LuaAstVisitorNew};
use into_lua_ast::IntoLuaAst;
use merge_lua_ast::MergeLuaAst;
use to_lua_ast_node::LuaAstNode;
use to_lua_ast_node::{ToLuaExt, ToLuaScoped};

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
            // Add the script's current directory to the lua path so that local
            // files can be imported
            let package: LuaTable = lua_ctx.globals().get("package")?;
            let mut path: String = package.get("path")?;

            let parent_path = script_path.parent().unwrap_or(&Path::new("./"));

            path.push_str(";");
            path.push_str(parent_path.to_str().expect("Did not find UTF-8 path"));
            path.push_str("/?.lua");

            package.set("path", path)?;

            lua_ctx.globals().set("package", package)?;

            // Load the script into the created scope
            lua_ctx.scope(|scope| {
                let refactor = scope.create_nonstatic_userdata(state)?;
                lua_ctx.globals().set("refactor", refactor)?;

                lua_ctx.load(&script).exec()
            })
        })
    })
    .unwrap_or_else(|e| panic!("User script failed: {}", DisplayLuaError(e)));

    Ok(())
}

struct DisplayLuaError(LuaError);

impl fmt::Display for DisplayLuaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            LuaError::SyntaxError{message, ..} => write!(f, "Syntax error while parsing lua: {}", message),
            LuaError::RuntimeError(e) => write!(f, "Runtime error during lua execution: {}", e),
            e => e.fmt(f),
        }
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
        // @tparam function(TransformCtxt) callback Transformation function called with a fresh @{TransformCtxt}. This @{TransformCtxt} can operate on the crate to implement transformations.
        methods.add_method_mut("transform", |lua_ctx, this, (callback, phase): (LuaFunction, Option<u8>)| {
            let phase = match phase {
                Some(1) => Phase::Phase1,
                Some(2) => Phase::Phase2,
                Some(3) | None => Phase::Phase3,
                _ => return Err(LuaError::external("Phase must be nil, 1, 2, or 3")),
            };
            this.load_crate();
            this.transform_crate(phase, |st, cx| {
                enter_transform(st, cx, |transform| {
                    let res: LuaResult<()> = lua_ctx.scope(|scope| {
                        let transform_data = scope.create_nonstatic_userdata(transform.clone())?;
                        callback.call(transform_data)?;
                        Ok(())
                    });
                    res.unwrap_or_else(|e| {
                        match e {
                            LuaError::CallbackError { traceback, cause } => {
                                panic!("Could not run transform due to {:#?} at:\n{}", cause, traceback);
                            }
                            _ => panic!("Could not run transform due to {:#?}", e),
                        }
                    });
                });
            })
            .map_err(|e| LuaError::external(format!("Failed to run compiler: {:#?}", e)))?;
            this.save_crate();
            Ok(())
        });
    }
}

// Dispatch to a monomorphized method taking a LuaAstNode<T> as the first parameter.
// The macro take the `object.method` to dispatch to, the AnyUserData node to
// dispatch on, a tuple containing additional args, and a list of AST node types
// that should be accepted in braces.
// example: dispatch!(this.fold_with, node, (args...), {P<ast::Expr>, P<ast::Ty>, Vec<ast::Stmt>})
macro_rules! dispatch {
    (
        $this: ident.$method: ident,
        $node: ident,
        $params: tt,
        {$($tys: ty),+}
        $(,)*
    ) => {
        $(
            if let Ok($node) = $node.borrow::<LuaAstNode<$tys>>() {
                dispatch!(@call $this.$method(&*$node, $params))
            } else
        )*
            {
                panic!("Could not match node type from Lua")
            }
    };

    (
        $this: ident.$method: ident<$generic: ty>,
        $node: ident,
        $params: tt,
        {$($tys: ty),+}
        $(,)*
    ) => {
        $(
            if let Ok($node) = $node.borrow::<LuaAstNode<$tys>>() {
                dispatch!(@call $this.$method<$generic>(&*$node, $params))
            } else
        )*
            {
                panic!("Could not match node type from Lua")
            }
    };

    (@call $this:ident.$method:ident ($node: expr, ($($arg:expr),*))) => {
        $this.$method($node, $($arg),*)
    };
    (@call $this:ident.$method:ident<$generic: ty> ($node: expr, ($($arg:expr),*))) => {
        $this.$method::<$generic, _>($node, $($arg),*)
    };
}

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

    fn new_subcontext(&self, mcx: MatchCtxt<'a, 'tcx>) -> Self {
        Self {
            mcx,
            transform: self.transform.clone(),
        }
    }

    fn fold_with<'lua, P, V>(
        &mut self,
        pattern: &LuaAstNode<P>,
        krate: &mut ast::Crate,
        callback: LuaFunction<'lua>,
        lua_ctx: LuaContext<'lua>,
    )
        where P: Pattern<V> + Clone,
              V: 'static + ToLuaScoped + Clone,
              LuaAstNode<P>: 'static + UserData,
              LuaAstNode<V>: 'static + UserData,
    {
        let pattern = pattern.borrow().clone();
        mut_visit_match_with(self.mcx.clone(), pattern, krate, |x, mcx| {
            let mcx = self.new_subcontext(mcx);
            let new_node: LuaAstNode<V> = lua_ctx
                .scope(|scope| {
                    let node = x.clone().to_lua_scoped(lua_ctx, scope)?;
                    let mcx = scope.create_nonstatic_userdata(mcx)?;
                    callback.call((node, mcx))
                })
                .unwrap_or_else(|e| {
                    panic!("Could not execute callback in match:fold_with {:#?}", e)
                });
            // TODO: This shouldn't have to be a clone, but we seem to have a
            // spurious copy held by Lua.
            *x = new_node.borrow().clone();
        })
    }

    fn try_match<'lua, T>(&mut self, pat: &LuaAstNode<T>, target: AnyUserData<'lua>) -> LuaResult<bool>
        where T: TryMatch,
              LuaAstNode<T>: 'static + UserData,
    {
        let target = match target.borrow::<LuaAstNode<T>>() {
            Ok(t) => t,
            Err(_) => return Ok(false), // target was not the same type of node as pat
        };
        let res = self.mcx.try_match(&*pat.borrow(), &*target.borrow()).is_ok();
        // TODO: Return the matcher error to Lua instead of a bool
        Ok(res)
    }

    fn subst<'lua, T>(&self, node: &LuaAstNode<T>, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaValue<'lua>>
        where T: Subst + Clone + ToLuaExt,
    {
        node.borrow()
            .clone()
            .subst(self.transform.st, self.transform.cx, &self.mcx.bindings)
            .to_lua(lua_ctx)
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
        methods.add_method_mut("parse_stmts", |lua_ctx, this, pat: String| {
            let stmts = this.mcx.parse_stmts(&pat);
            stmts.to_lua(lua_ctx)
        });

        /// Parse an expression and add it to this MatchCtxt
        // @function parse_expr
        // @tparam string pat Pattern to parse
        // @treturn LuaAstNode The parsed expression
        methods.add_method_mut("parse_expr", |lua_ctx, this, pat: String| {
            let expr = this.mcx.parse_expr(&pat);
            expr.to_lua(lua_ctx)
        });

        /// Find matches of `pattern` and rewrite using `callback`
        // @function fold_with
        // @tparam LuaAstNode needle Pattern to search for
        // @tparam function(LuaAstNode,MatchCtxt) callback Function called for each match. Takes the matching node and a new @{MatchCtxt} for that match.
        methods.add_method_mut(
            "fold_with",
            |lua_ctx, this, (needle, callback): (AnyUserData, LuaFunction)| {
                this.transform.st.map_krate(|krate| {
                    dispatch!(
                        this.fold_with,
                        needle,
                        (krate, callback, lua_ctx),
                        {P<ast::Expr>, P<ast::Ty>, Vec<ast::Stmt>},
                    )
                });
                Ok(())
            },
        );

        /// Get matched binding for an expression variable
        // @function get_expr
        // @tparam string pattern Expression variable pattern
        // @treturn LuaAstNode Expression matched by this binding
        methods.add_method_mut("get_expr", |lua_ctx, this, pattern: String| {
            this.mcx.bindings
                .get::<_, P<ast::Expr>>(pattern)
                .unwrap()
                .clone()
                .to_lua(lua_ctx)
        });

        /// Get matched binding for a statement variable
        // @function get_stmt
        // @tparam string pattern Statement variable pattern
        // @treturn LuaAstNode Statement matched by this binding
        methods.add_method_mut("get_stmt", |lua_ctx, this, pattern: String| {
            this.mcx.bindings.get::<_, ast::Stmt>(pattern).unwrap().clone().to_lua(lua_ctx)
        });

        /// Attempt to match `target` against `pat`, updating bindings if matched.
        // @function try_match
        // @tparam LuaAstNode pat AST (potentially with variable bindings) to match with
        // @tparam LuaAstNode target AST to match against
        // @treturn bool true if match was successful
        methods.add_method_mut(
            "try_match",
            |_lua_ctx, this, (pat, target): (AnyUserData, AnyUserData)| {
                dispatch!(this.try_match, pat, (target), {P<ast::Expr>})
            },
        );

        /// Substitute the currently matched AST node with a new AST node
        // @function subst
        // @tparam LuaAstNode replacement New AST node to replace the currently matched AST. May include variable bindings if these bindings were matched by the search pattern.
        // @treturn LuaAstNode New AST node with variable bindings replaced by their matched values
        methods.add_method_mut("subst", |lua_ctx, this, replacement: AnyUserData| {
            dispatch!(this.subst, replacement, (lua_ctx), {P<ast::Expr>, Vec<ast::Stmt>, ast::Stmt})
        });
    }
}

#[derive(Clone)]
pub(crate) struct TransformCtxt<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
}

fn enter_transform<'a, 'tcx, F, R>(st: &'a CommandState, cx: &'a RefactorCtxt<'a, 'tcx>, f: F) -> R
    where F: Fn(TransformCtxt) -> R
{
    let ctx = TransformCtxt {
        st,
        cx,
    };

    f(ctx)
}

impl<'a, 'tcx> TransformCtxt<'a, 'tcx> {
    fn fold_paths<'lua, T>(&self, node: &LuaAstNode<T>, callback: LuaFunction<'lua>, lua_ctx: LuaContext<'lua>) -> LuaResult<()>
        where T: MutVisit,
              LuaAstNode<T>: 'static + UserData + Clone,
    {
        node.map(|node| {
            fold_resolved_paths_with_id(node, self.cx, |id, qself, path, def| {
                let (qself, path): (Option<LuaAstNode<ast::QSelf>>, LuaAstNode<ast::Path>) = lua_ctx
                    .scope(|scope| {
                        let qself = qself.map(|q| q.to_lua_scoped(lua_ctx, scope).unwrap());
                        let path = path.to_lua_scoped(lua_ctx, scope).unwrap();
                        let def = def.to_lua_scoped(lua_ctx, scope).unwrap();
                        callback.call((
                            id.to_lua(lua_ctx).unwrap(),
                            qself,
                            path,
                            def,
                        ))
                    })
                    .unwrap_or_else(|e| panic!("Lua callback failed in visit_paths: {}", DisplayLuaError(e)));
                (qself.map(|x| x.into_inner()), path.into_inner())
            });
        });
        Ok(())
    }

    fn create_use_tree<'lua>(
        &self,
        lua_ctx: LuaContext<'lua>,
        lua_tree: LuaTable<'lua>,
        ident: Option<ast::Ident>
    ) -> LuaResult<ast::UseTree> {
        let mut trees: Vec<ast::UseTree> = vec![];
        for pair in lua_tree.pairs::<LuaValue, LuaValue>() {
            let (key, value) = pair?;
            match key {
                LuaValue::Integer(_) => {
                    let ident_str = LuaString::from_lua(value, lua_ctx)?;
                    // TODO: handle renames
                    trees.push(mk().use_tree(
                        ident_str.to_str()?,
                        ast::UseTreeKind::Simple(None, DUMMY_NODE_ID, DUMMY_NODE_ID),
                    ));
                }

                LuaValue::String(s) => match value {
                    LuaValue::String(ref glob) if glob.to_str()? == "*" => {
                        trees.push(mk().use_tree(s.to_str()?, ast::UseTreeKind::Glob));
                    }

                    LuaValue::Table(items) => {
                        trees.push(self.create_use_tree(
                            lua_ctx,
                            items,
                            Some(ast::Ident::from_str(s.to_str()?)),
                        )?);
                    }

                    _ => return Err(LuaError::FromLuaConversionError {
                        from: "UseTree table value",
                        to: "ast::UseTree",
                        message: Some("UseTree table keys must be \"*\" or a table".to_string()),
                    })
                },

                _ => return Err(LuaError::FromLuaConversionError {
                    from: "UseTree table key",
                    to: "ast::UseTree",
                    message: Some("UseTree table keys must be integer or string".to_string()),
                }),
            }
        }

        if trees.len() == 1 {
            let mut use_tree = trees.pop().unwrap();
            if let Some(ident) = ident {
                use_tree.prefix.segments.insert(0, ast::PathSegment::from_ident(ident));
            }
            return Ok(use_tree);
        }
        let prefix = ast::Path::from_ident(ident.unwrap_or_else(|| ast::Ident::from_str("")));
        Ok(mk().use_tree(prefix, ast::UseTreeKind::Nested(
            trees.into_iter().map(|u| (u, DUMMY_NODE_ID)).collect()
        )))
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
        methods.add_method(
            "replace_stmts_with",
            |lua_ctx, this, (pat, callback): (String, LuaFunction)| {
                this.st.map_krate(|krate| {
                    let mut smcx = ScriptingMatchCtxt::new(this.clone());
                    let pat = smcx.mcx.parse_stmts(&pat);
                    smcx.fold_with(&LuaAstNode::new(pat), krate, callback, lua_ctx);
                    Ok(())
                })
            },
        );

        /// Replace matching expressions using given callback
        // @function replace_expr_with
        // @tparam string needle Expression pattern to search for, may include variable bindings
        // @tparam function(LuaAstNode,MatchCtxt) callback Function called for each match. Takes the matching node and a new @{MatchCtxt} for that match. See @{MatchCtxt:fold_with}
        methods.add_method(
            "replace_expr_with",
            |lua_ctx, this, (needle, callback): (String, LuaFunction)| {
                this.st.map_krate(|krate| {
                    let mut smcx = ScriptingMatchCtxt::new(this.clone());
                    let needle = smcx.mcx.parse_expr(&needle);
                    smcx.fold_with(&LuaAstNode::new(needle), krate, callback, lua_ctx);
                    Ok(())
                })
            },
        );

        /// Create a new, empty @{MatchCtxt}
        // @function match
        // @tparam function(MatchCtxt) callback Function called with the new match context
        methods.add_method("match", |lua_ctx, this, callback: LuaFunction| {
            let init_mcx = ScriptingMatchCtxt::new(this.clone());
            lua_ctx.scope(|scope| {
                let init_mcx = scope.create_nonstatic_userdata(init_mcx)?;
                callback.call::<_, ()>(init_mcx)
            })
        });

        /// Visits an entire crate via a lua object's methods
        // @function visit_crate
        // @tparam Visitor object Visitor whose methods will be called during the traversal
        methods.add_method(
            "visit_crate",
            |lua_ctx, this, visitor_obj: LuaTable| {
                this.st.map_krate(|krate| {
                    let lua_crate = krate.clone().into_lua_ast(this, lua_ctx)?;

                    let visitor = LuaAstVisitor::new(visitor_obj);

                    visitor.visit_crate(lua_crate.clone())?;
                    visitor.finish()?;

                    krate.merge_lua_ast(lua_crate)
                })
            },
        );

        /// Visits an entire crate via a lua object's methods
        // @function visit_crate
        // @tparam Visitor object Visitor whose methods will be called during the traversal
        methods.add_method(
            "visit_crate_new",
            |lua_ctx, this, visitor_obj: LuaTable| {
                this.st.map_krate(|krate: &mut ast::Crate| {
                    let mut visitor = LuaAstVisitorNew::new(this.clone(), lua_ctx, visitor_obj);

                    visitor.visit_crate(krate);
                    Ok(())
                })
            },
        );

        /// Visits every fn like via a lua object's methods
        // @function visit_fn_like
        // @tparam Visitor object Visitor whose methods will be called during the traversal
        methods.add_method(
            "visit_fn_like_new",
            |lua_ctx, this, callback: LuaFunction| {
                this.st.map_krate(|krate| {
                    let mut result = Ok(());

                    let wrapper = |fn_like: &mut FnLike| -> LuaResult<()> {
                        let node = LuaAstNode::new(fn_like.clone());
                        lua_ctx.scope(|scope| {
                            let param = scope.create_static_userdata(node)?;

                            callback.call(param)
                        })
                    };

                    mut_visit_fns(krate, |mut fn_like| {
                        if result.is_err() {
                            return;
                        }

                        match wrapper(&mut fn_like) {
                            Ok(()) => (),
                            Err(e) => {
                                result = Err(e);
                                return;
                            }
                        };
                    });

                    result
                })
            },
        );

        /// Visits every fn like via a lua object's methods
        // @function visit_fn_like
        // @tparam Visitor object Visitor whose methods will be called during the traversal
        methods.add_method(
            "visit_fn_like",
            |lua_ctx, this, visitor_obj: LuaTable| {
                this.st.map_krate(|krate| {
                    let mut result = Ok(());
                    let visitor = LuaAstVisitor::new(visitor_obj);

                    // Here we actually call the visitor visit methods recursively, as
                    // well as the lua finish method once complete.
                    let wrapper = |fn_like: &mut FnLike| -> LuaResult<()> {
                        let lua_fn_like = fn_like.clone().into_lua_ast(this, lua_ctx)?;

                        visitor.visit_fn_like(lua_fn_like.clone())?;
                        visitor.finish()?;

                        fn_like.merge_lua_ast(lua_fn_like)
                    };

                    mut_visit_fns(krate, |mut fn_like| {
                        if result.is_err() {
                            return;
                        }

                        match wrapper(&mut fn_like) {
                            Ok(()) => (),
                            Err(e) => {
                                result = Err(e);
                                return;
                            }
                        };
                    });

                    result
                })
            },
        );

        /// Rewrite all paths in a crate
        // @function visit_paths
        // @tparam LuaAstNode node AST node to visit. Valid node types: {P<Item>}.
        // @tparam function(NodeId, QSelf, Path, Def) callback Function called for each path. Can modify QSelf and/or Path to rewrite the path.
        methods.add_method("visit_paths", |lua_ctx, this, (node, callback): (AnyUserData<'lua>, LuaFunction<'lua>)| {
            dispatch!(this.fold_paths, node, (callback, lua_ctx), {P<ast::Item>})
        });

        /// Create a new use item
        // @function create_use
        // @tparam tab Tree of paths to convert into a UseTree. Each leaf of the tree corresponds to a UseTree and is an array of items to import. Each of these items may be a string (the item name), an array with two values: the item name and an identifier to rebind the item name to (i.e. `a as b`), or the literal string '*' indicating a glob import from that module. The prefix for each UseTree is the sequence of keys in the map along the path to that leaf array.
        // @treturn LuaAstNode New use item node
        methods.add_method("create_use", |lua_ctx, this, (tree, pub_vis): (LuaTable, bool)| {
            let use_tree = this.create_use_tree(lua_ctx, tree, None)?;
            let mut builder = mk();
            if pub_vis { builder = builder.pub_(); }
            Ok(builder.use_item(use_tree).to_lua(lua_ctx))
        });

        methods.add_method("get_use_def", |lua_ctx, this, id: u32| {
            this.cx.resolve_use_id(ast::NodeId::from_u32(id)).res.to_lua(lua_ctx)
        });
    }
}
