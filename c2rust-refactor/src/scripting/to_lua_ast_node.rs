use std::cell::{Ref, RefCell, RefMut};
use std::mem::swap;
use std::ops::DerefMut;
use std::sync::Arc;

use rustc::hir::def::Res;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::mut_visit::*;
use syntax::source_map::{DUMMY_SP, dummy_spanned};
use syntax_pos::Span;

use rlua::{Context, Error, Function, Result, Scope, ToLua, UserData, UserDataMethods, Value};
use rlua::prelude::LuaString;

use crate::ast_manip::{util, visit_nodes, AstName, AstNode, WalkAst};
use crate::ast_manip::fn_edit::{FnLike, FnKind};
use super::DisplayLuaError;

/// Refactoring module
// @module Refactor


pub(crate) trait ToLuaExt {
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>>;
}

pub(crate) trait ToLuaScoped {
    fn to_lua_scoped<'lua, 'scope>(self, lua: Context<'lua>, scope: &Scope<'lua, 'scope>) -> Result<Value<'lua>>;
}

impl<T> ToLuaExt for T
    where T: Sized,
          LuaAstNode<T>: 'static + UserData + Send,
{
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        lua.create_userdata(LuaAstNode::new(self))?.to_lua(lua)
    }
}

impl<T> ToLuaScoped for T
    where T: 'static + Sized,
          LuaAstNode<T>: UserData,
{
    fn to_lua_scoped<'lua, 'scope>(self, lua: Context<'lua>, scope: &Scope<'lua, 'scope>) -> Result<Value<'lua>> {
        scope.create_static_userdata(LuaAstNode::new(self)).and_then(|v| v.to_lua(lua))
    }
}

/// Holds a rustc AST node that can be passed back and forth to Lua as a scoped,
/// static userdata. Implement UserData for LuaAstNode<T> to support an AST node
/// T.
#[derive(Clone)]
pub(crate) struct LuaAstNode<T> (Arc<RefCell<T>>);

impl<T> LuaAstNode<T> {
    pub fn new(item: T) -> Self {
        Self(Arc::new(RefCell::new(item)))
    }

    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }

    pub fn map<F>(&self, f: F)
        where F: Fn(&mut T)
    {
        f(self.0.borrow_mut().deref_mut());
    }
}

impl<T> LuaAstNode<T>
    where T: Clone
{
    // TODO: make sure we aren't leaking LuaAstNodes into Lua, never to be freed
    pub fn into_inner(self) -> T {
        match Arc::try_unwrap(self.0) {
            Ok(cell) => cell.into_inner(),
            Err(arc) => arc.borrow().clone(),
        }
    }
}

impl<T> LuaAstNode<T>
    where T: WalkAst
{
    pub fn walk<V: MutVisitor>(&self, visitor: &mut V) {
        self.0.borrow_mut().walk(visitor);
    }
}

/// Item AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ItemAstNode
unsafe impl Send for LuaAstNode<P<Item>> {}
#[allow(unused_doc_comments)]
impl UserData for LuaAstNode<P<Item>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().node.ast_name())
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.0.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.0.borrow().ident.to_lua(lua_ctx)
        });

        methods.add_method("set_ident", |_lua_ctx, this, ident: LuaString| {
            this.0.borrow_mut().ident = Ident::from_str(ident.to_str()?);
            Ok(())
        });

        methods.add_method("get_vis", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().vis.ast_name())
        });

        /// Visit statements
        // @function visit_stmts
        // @tparam function(LuaAstNode) callback Function to call when visiting each statement
        methods.add_method("visit_stmts", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &Stmt| {
                callback.call::<_, ()>(node.clone().to_lua(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_stmts: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("visit_items", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &Item| {
                callback.call::<_, ()>(P(node.clone()).to_lua(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_items: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("visit_foreign_items", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &ForeignItem| {
                callback.call::<_, ()>(P(node.clone()).to_lua(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_foreign_items: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.0.borrow().node.clone() {
                ItemKind::Use(e) => Ok(e.to_lua(lua_ctx)),
                node => Err(Error::external(format!("Item node {:?} not implemented yet", node))),
            }
        });
    }
}

/// ForeignItem AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ForeignItemAstNode
unsafe impl Send for LuaAstNode<P<ForeignItem>> {}
impl UserData for LuaAstNode<P<ForeignItem>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().node.ast_name())
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.0.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.0.borrow().ident.to_lua(lua_ctx)
        });

        methods.add_method("set_ident", |_lua_ctx, this, ident: LuaString| {
            this.0.borrow_mut().ident = Ident::from_str(ident.to_str()?);
            Ok(())
        });
    }
}

/// QSelf AST node handle
//
// @type QSelfAstNode
impl UserData for LuaAstNode<QSelf> {}


/// Path AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type PathAstNode
unsafe impl Send for LuaAstNode<Path> {}
impl UserData for LuaAstNode<Path> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_span", |lua_ctx, this, ()| {
            this.0.borrow().span.to_lua(lua_ctx)
        });
        methods.add_method("has_generic_args", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().segments.iter().any(|s| s.args.is_some()))
        });
        methods.add_method("get_segments", |lua_ctx, this, ()| {
            this.0
                .borrow()
                .segments
                .iter()
                .map(|s| s.ident.to_lua(lua_ctx))
                .collect::<Result<Vec<_>>>()
        });
        methods.add_method("set_segments", |_lua_ctx, this, new_segments: Vec<LuaString>| {
            let has_generic_args = this.0.borrow().segments.iter().any(|s| s.args.is_some());
            if has_generic_args {
                Err(Error::external("One or more path segments have generic args, cannot set segments as strings"))
            } else {
                this.0.borrow_mut().segments = new_segments.into_iter().map(|new_seg| {
                    Ok(PathSegment::from_ident(Ident::from_str(new_seg.to_str()?)))
                }).collect::<Result<Vec<_>>>()?;
                Ok(())
            }
        });
        methods.add_method("map_segments", |lua_ctx, this, callback: Function| {
            let new_segments = lua_ctx.scope(|scope| {
                let segments = this.0.borrow().segments.iter().map(|s| scope.create_static_userdata(LuaAstNode::new(s.clone())).unwrap()).collect::<Vec<_>>().to_lua(lua_ctx);
                callback.call::<_, Vec<LuaAstNode<PathSegment>>>(segments)
            }).unwrap();
            this.0.borrow_mut().segments = new_segments.into_iter().map(|s| s.into_inner()).collect();
            Ok(())
        });

        methods.add_method("set_generic_angled_arg_tys", |_lua_ctx, this, (idx, tys): (usize, Vec<LuaAstNode<P<Ty>>>)| {
            let generic_arg = GenericArgs::AngleBracketed(AngleBracketedArgs {
                span: DUMMY_SP,
                args: tys.iter().map(|ty| GenericArg::Type(ty.borrow().clone())).collect(),
                constraints: Vec::new(),
            });

            // Using 1-offset idx like lua does
            this.borrow_mut().segments[idx - 1].args = Some(P(generic_arg));

            Ok(())
        });
    }
}

impl UserData for LuaAstNode<PathSegment> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.0.borrow().ident.to_lua(lua_ctx)
        });
    }
}


/// Def result AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type DefAstNode
unsafe impl Send for LuaAstNode<Res> {}
impl UserData for LuaAstNode<Res> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_namespace", |_lua_ctx, this, ()| {
            Ok(util::namespace(&*this.0.borrow()).map(|namespace| namespace.descr()))
        });
    }
}


impl ToLuaExt for NodeId {
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.as_u32().to_lua(lua)
    }
}

impl ToLuaExt for Ident {
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.as_str().get().to_lua(lua)
    }
}

struct SpanData(syntax_pos::SpanData);

impl UserData for SpanData {}

impl ToLuaExt for Span {
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        lua.create_userdata(SpanData(self.data())).unwrap().to_lua(lua)
    }
}


/// Expr AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ExprAstNode
unsafe impl Send for LuaAstNode<P<Expr>> {}
impl UserData for LuaAstNode<P<Expr>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.borrow().node.ast_name())
        });

        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.borrow().node.clone() {
                ExprKind::Lit(x) => x.to_lua(lua_ctx),
                node => Err(Error::external(format!("Expr node {:?} not implemented yet", node))),
            }
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            Ok(this.borrow().id.to_lua(lua_ctx))
        });

        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            match &this.borrow().node {
                ExprKind::Cast(_, ty)
                | ExprKind::Type(_, ty) => Ok(LuaAstNode::new(ty.clone())),
                e => unimplemented!("LuaAstNode<P<Expr>>:get_ty() for {}", e.ast_name()),
            }
        });

        methods.add_method("get_path", |_lua_ctx, this, ()| {
            match &this.borrow().node {
                ExprKind::Path(_, path)
                | ExprKind::Struct(path, ..) => Ok(LuaAstNode::new(path.clone())),
                e => unimplemented!("LuaAstNode<P<Expr>>:get_path() for {}", e.ast_name()),
            }
        });

        methods.add_method("get_exprs", |_lua_ctx, this, ()| {
            match &this.borrow().node {
                ExprKind::Cast(expr, _)
                | ExprKind::Field(expr, _)
                | ExprKind::Unary(_, expr) => Ok(vec![LuaAstNode::new(expr.clone())]),
                ExprKind::MethodCall(_, exprs) => Ok(exprs.iter().map(|e| LuaAstNode::new(e.clone())).collect()),
                ExprKind::Assign(lhs, rhs) => Ok(vec![LuaAstNode::new(lhs.clone()), LuaAstNode::new(rhs.clone())]),
                ExprKind::Call(func, params) => {
                    let mut exprs = Vec::with_capacity(params.len() + 1);

                    exprs.push(LuaAstNode::new(func.clone()));

                    for param in params {
                        exprs.push(LuaAstNode::new(param.clone()));
                    }

                    Ok(exprs)
                },
                e => unimplemented!("LuaAstNode<P<Expr>>:get_exprs() for {}", e.ast_name()),
            }
        });

        methods.add_method("set_exprs", |_lua_ctx, this, exprs: Vec<LuaAstNode<P<Expr>>>| {
            match &mut this.borrow_mut().node {
                ExprKind::Field(expr, _)
                | ExprKind::Unary(_, expr) => *expr = exprs[0].borrow().clone(),
                ExprKind::MethodCall(_, exprs) => *exprs = exprs.iter().map(|e| e.clone()).collect(),
                ExprKind::Call(func, params) => {
                    *func = exprs[0].borrow().clone();
                    *params = exprs.iter().skip(1).map(|e| e.borrow().clone()).collect()
                },
                ExprKind::Assign(lhs, rhs)
                | ExprKind::Binary(_, lhs, rhs) => {
                    *lhs = exprs[0].borrow().clone();
                    *rhs = exprs[1].borrow().clone();
                },
                ExprKind::Cast(expr, _) => *expr = exprs[0].borrow().clone(),
                e => unimplemented!("LuaAstNode<P<Expr>>:set_exprs() for {}", e.ast_name()),
            }

            Ok(())
        });

        methods.add_method("get_op", |_lua_ctx, this, ()| {
            match &this.borrow().node {
                ExprKind::Unary(op, _) => Ok(Some(op.ast_name())),
                // ExprKind::Binary(op, ..) |
                // ExprKind::AssignOp(op, ..) => Ok(op.ast_name()),
                _ => Ok(None),
            }
        });

        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });

        methods.add_method("get_method_name", |lua_ctx, this, ()| {
            if let ExprKind::MethodCall(path_seg, ..) = &this.borrow().node {
                return path_seg.ident.as_str().get().to_lua(lua_ctx).map(|s| Some(s))
            }

            Ok(None)
        });

        methods.add_method("set_method_name", |_lua_ctx, this, name: LuaString| {
            if let ExprKind::MethodCall(path_seg, ..) = &mut this.borrow_mut().node {
                path_seg.ident = Ident::from_str(name.to_str()?);
            }

            Ok(())
        });

        methods.add_method("get_lit", |_lua_ctx, this, ()| {
            if let ExprKind::Lit(lit) = &this.borrow().node {
                return Ok(Some(LuaAstNode::new(lit.clone())))
            }

            Ok(None)
        });

        methods.add_method("to_lit", |_lua_ctx, this, lit: LuaAstNode<Lit>| {
            let lit = lit.borrow().clone();

            this.borrow_mut().node = ExprKind::Lit(lit);

            Ok(())
        });

        methods.add_method("to_index", |_lua_ctx, this, (indexed, indexee): (LuaAstNode<P<Expr>>, LuaAstNode<P<Expr>>)| {
            let indexed = indexed.borrow().clone();
            let indexee = indexee.borrow().clone();

            this.borrow_mut().node = ExprKind::Index(indexed, indexee);

            Ok(())
        });

        methods.add_method("to_binary", |_lua_ctx, this, (op, lhs, rhs): (LuaString, LuaAstNode<P<Expr>>, LuaAstNode<P<Expr>>)| {
            let op = match op.to_str()? {
                "Add" => BinOpKind::Add,
                "Div" => BinOpKind::Div,
                _ => unimplemented!("BinOpKind parsing from string"),
            };
            let lhs = lhs.borrow().clone();
            let rhs = rhs.borrow().clone();

            this.borrow_mut().node = ExprKind::Binary(dummy_spanned(op), lhs, rhs);

            Ok(())
        });

        methods.add_method("to_call", |_lua_ctx, this, exprs: Vec<LuaAstNode<P<Expr>>>| {
            let func = exprs[0].borrow().clone();
            let params = exprs.iter().skip(1).map(|lan| lan.borrow().clone()).collect();

            this.borrow_mut().node = ExprKind::Call(func, params);

            Ok(())
        });

        methods.add_method("to_field", |_lua_ctx, this, (expr, ident): (LuaAstNode<P<Expr>>, LuaString)| {
            let expr = expr.borrow().clone();

            this.borrow_mut().node = ExprKind::Field(expr, Ident::from_str(ident.to_str()?));

            Ok(())
        });

        methods.add_method("to_ident_path", |_lua_ctx, this, path: LuaString| {
            let path = Path::from_ident(Ident::from_str(path.to_str()?));

            this.borrow_mut().node = ExprKind::Path(None, path);

            Ok(())
        });

        methods.add_method("to_path", |_lua_ctx, this, path: LuaAstNode<Path>| {
            this.borrow_mut().node = ExprKind::Path(None, path.borrow().clone());

            Ok(())
        });
    }
}


/// Ty AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type TyAstNode
unsafe impl Send for LuaAstNode<P<Ty>> {}
impl UserData for LuaAstNode<P<Ty>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });

        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.borrow().node.ast_name())
        });

        methods.add_method("get_mut_ty", |_lua_ctx, this, ()| {
            match &this.borrow().node {
                TyKind::Ptr(mut_ty) |
                TyKind::Rptr(_, mut_ty) => Ok(Some(LuaAstNode::new(mut_ty.clone()))),
                _ => Ok(None),
            }
        });

        methods.add_method("to_rptr", |_lua_ctx, this, (lt, mut_ty): (Option<LuaString>, LuaAstNode<MutTy>)| {
            let lt = lt.map(|lt| {
                let lt_str = lt.to_str()?;
                let mut lt_string = String::with_capacity(lt_str.len() + 1);

                lt_string.push('\'');
                lt_string.push_str(lt_str);

                Ok(Lifetime {
                    id: DUMMY_NODE_ID,
                    ident: Ident::from_str(&lt_string),
                })
            }).transpose()?;

            this.borrow_mut().node = TyKind::Rptr(lt, mut_ty.borrow().clone());

            Ok(())
        });

        methods.add_method("wrap_in_slice", |_lua_ctx, this, ()| {
            let mut ty = this.borrow_mut();
            let mut placeholder = TyKind::Err;

            swap(&mut placeholder, &mut ty.node);

            ty.node = TyKind::Slice(P(Ty {
                id: DUMMY_NODE_ID,
                node: placeholder,
                span: DUMMY_SP,
            }));

            Ok(())
        });

        methods.add_method("wrap_as_generic_angle_arg", |_lua_ctx, this, name: LuaString| {
            let mut ty = this.borrow_mut();
            let mut placeholder = TyKind::Err;

            swap(&mut placeholder, &mut ty.node);

            let arg = GenericArg::Type(P(Ty {
                id: DUMMY_NODE_ID,
                node: placeholder,
                span: DUMMY_SP,
            }));
            let args = GenericArgs::AngleBracketed(AngleBracketedArgs {
                span: DUMMY_SP,
                args: vec![arg],
                constraints: Vec::new(),
            });
            let path_segment = PathSegment {
                ident: Ident::from_str(name.to_str()?),
                id: DUMMY_NODE_ID,
                args: Some(P(args)),
            };
            let path = Path {
                span: DUMMY_SP,
                segments: vec![path_segment],
            };

            ty.node = TyKind::Path(None, path);

            Ok(())
        });
    }
}

unsafe impl Send for LuaAstNode<Vec<Stmt>> {}
impl UserData for LuaAstNode<Vec<Stmt>> {}


/// MutTy AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type MutTyAstNode
unsafe impl Send for LuaAstNode<MutTy> {}
impl UserData for LuaAstNode<MutTy> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            Ok(LuaAstNode::new(this.0.borrow().ty.clone()))
        });

        methods.add_method("set_ty", |_lua_ctx, this, ty: LuaAstNode<P<Ty>>| {
            this.0.borrow_mut().ty = ty.0.borrow().clone();

            Ok(())
        });
    }
}

/// Stmt AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type StmtAstNode
unsafe impl Send for LuaAstNode<Stmt> {}
impl UserData for LuaAstNode<Stmt> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().node.ast_name())
        });
        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.0.borrow().node.clone() {
                StmtKind::Expr(e) | StmtKind::Semi(e) => e.to_lua(lua_ctx),
                StmtKind::Local(l) => l.to_lua(lua_ctx),
                StmtKind::Item(i) => i.to_lua(lua_ctx),
                StmtKind::Mac(_) => Err(Error::external(format!("Mac stmts aren't implemented yet"))),
            }
        });
    }
}


/// Pat AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type PatAstNode
unsafe impl Send for LuaAstNode<P<Pat>> {}
impl UserData for LuaAstNode<P<Pat>> {}


/// Crate AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type CrateAstNode
unsafe impl Send for LuaAstNode<Crate> {}
impl UserData for LuaAstNode<Crate> {}


/// Local AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type LocalAstNode
unsafe impl Send for LuaAstNode<P<Local>> {}
impl UserData for LuaAstNode<P<Local>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            Ok(this.borrow().ty.as_ref().map(|ty| LuaAstNode::new(ty.clone())))
        });

        methods.add_method("set_ty", |_lua_ctx, this, ty: Option<LuaAstNode<P<Ty>>>| {
            this.borrow_mut().ty = ty.map(|ty| ty.borrow().clone());

            Ok(())
        });

        methods.add_method("get_init", |_lua_ctx, this, ()| {
            Ok(this.borrow().init.as_ref().map(|init| LuaAstNode::new(init.clone())))
        });

        methods.add_method("set_init", |_lua_ctx, this, init: Option<LuaAstNode<P<Expr>>>| {
            this.borrow_mut().init = init.map(|init| init.borrow().clone());

            Ok(())
        });

        methods.add_method("get_pat_id", |lua_ctx, this, ()| {
            Ok(this.borrow().pat.id.to_lua(lua_ctx))
        });
    }
}


/// Lit AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type LitAstNode
unsafe impl Send for LuaAstNode<Lit> {}
impl UserData for LuaAstNode<Lit> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_value", |lua_ctx, this, ()| {
            match this.borrow().node {
                LitKind::Str(s, _) => {
                    s.to_string().to_lua(lua_ctx)
                }
                LitKind::Int(i, _suffix) => i.to_lua(lua_ctx),
                LitKind::Bool(b) => b.to_lua(lua_ctx),
                ref node => {
                    return Err(Error::external(format!(
                        "{:?} is not yet implemented",
                        node
                    )));
                }
            }
        });

        methods.add_method("strip_suffix", |_lua_ctx, this, ()| {
            let mut lit = this.borrow_mut();

            if let LitKind::Int(_, ref mut suffix) = lit.node {
                *suffix = LitIntType::Unsuffixed
            }

            lit.token.suffix = None;

            Ok(())
        });

        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });
    }
}


/// Mod AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ModAstNode
unsafe impl Send for LuaAstNode<Mod> {}
impl UserData for LuaAstNode<Mod> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("num_items", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().items.len())
        });

        methods.add_method_mut("insert_item", |_lua_ctx, this, (index, item): (usize, LuaAstNode<P<Item>>)| {
            this.0.borrow_mut().items.insert(index, item.borrow().clone());
            Ok(())
        });

        methods.add_method("get_items", |lua_ctx, this, ()| {
            this.0
                .borrow()
                .items
                .iter()
                .map(|item| item.clone().to_lua(lua_ctx))
                .collect::<Result<Vec<_>>>()
        });

        methods.add_method_mut("drain_items", |lua_ctx, this, ()| {
            this.0
                .borrow_mut()
                .items
                .drain(..)
                .map(|item| item.to_lua(lua_ctx))
                .collect::<Result<Vec<_>>>()
        });
    }
}


/// UseTree AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type UseTreeAstNode
unsafe impl Send for LuaAstNode<P<UseTree>> {}
impl UserData for LuaAstNode<P<UseTree>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().kind.ast_name())
        });

        methods.add_method("get_prefix", |lua_ctx, this, ()| {
            this.0.borrow().prefix.clone().to_lua(lua_ctx)
        });

        methods.add_method("get_rename", |_lua_ctx, this, ()| {
            match this.0.borrow().kind {
                UseTreeKind::Simple(Some(rename), _, _) => Ok(Some(rename.to_string())),
                _ => Ok(None),
            }
        });

        methods.add_method("get_nested", |lua_ctx, this, ()| {
            match &this.0.borrow().kind {
                UseTreeKind::Nested(trees) => Ok(Some(
                    trees.clone()
                        .into_iter()
                        .map(|(tree, id)| Ok(vec![P(tree).to_lua(lua_ctx)?, id.to_lua(lua_ctx)?]))
                        .collect::<Result<Vec<_>>>()?
                )),
                _ => Ok(None),
            }
        });
    }
}

impl ToLuaExt for AstNode {
    fn to_lua<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        match self {
            AstNode::Crate(x) => x.to_lua(lua),
            AstNode::Expr(x) => x.to_lua(lua),
            AstNode::Pat(x) => x.to_lua(lua),
            AstNode::Ty(x) => x.to_lua(lua),
            AstNode::Stmts(x) => x.to_lua(lua),
            AstNode::Stmt(x) => x.to_lua(lua),
            AstNode::Item(x) => x.to_lua(lua),
        }
    }
}

/// FnLike AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type FnLikeAstNode
unsafe impl Send for LuaAstNode<P<FnLike>> {}
impl UserData for LuaAstNode<FnLike> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |lua_ctx, this, ()| {
            Ok(this.borrow().kind.to_lua(lua_ctx))
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.borrow().ident.to_lua(lua_ctx)
        });

        methods.add_method("has_block", |lua_ctx, this, ()| {
            this.borrow().block.is_some().to_lua(lua_ctx)
        });
    }
}

impl ToLuaExt for FnKind {
    fn to_lua<'lua>(self, ctx: Context<'lua>) -> Result<Value<'lua>> {
        match self {
            FnKind::Normal => "Normal",
            FnKind::ImplMethod => "ImplMethod",
            FnKind::TraitMethod => "TraitMethod",
            FnKind::Foreign => "Foreign",
        }.to_lua(ctx)
    }
}

/// FnDecl AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type FnDeclAstNode
unsafe impl Send for LuaAstNode<P<FnDecl>> {}
impl UserData for LuaAstNode<P<FnDecl>> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_args", |_lua_ctx, this, ()| {
            Ok(this
                .borrow()
                .inputs
                .iter()
                .map(|arg| LuaAstNode::new(arg.clone()))
                .collect::<Vec<_>>())
        });
    }
}

/// Arg AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ArgAstNode
unsafe impl Send for LuaAstNode<Arg> {}
impl UserData for LuaAstNode<Arg> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            Ok(LuaAstNode::new(this.borrow().ty.clone()))
        });

        methods.add_method("set_ty", |_lua_ctx, this, ty: LuaAstNode<P<Ty>>| {
            this.borrow_mut().ty = ty.borrow().clone();

            Ok(())
        });

        methods.add_method("get_pat_id", |lua_ctx, this, ()| {
            Ok(this.borrow().pat.id.to_lua(lua_ctx))
        });

        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });
    }
}

/// FnHeader AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type FnHeaderAstNode
unsafe impl Send for LuaAstNode<FnHeader> {}
impl UserData for LuaAstNode<FnHeader> {}

/// StructField AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type StructFieldAstNode
unsafe impl Send for LuaAstNode<StructField> {}
impl UserData for LuaAstNode<StructField> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            Ok(LuaAstNode::new(this.borrow().ty.clone()))
        });

        methods.add_method("set_ty", |_lua_ctx, this, ty: LuaAstNode<P<Ty>>| {
            this.borrow_mut().ty = ty.borrow().clone();

            Ok(())
        });

        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });
    }
}

/// ItemKind AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type ItemKindAstNode
unsafe impl Send for LuaAstNode<ItemKind> {}
impl UserData for LuaAstNode<ItemKind> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.borrow().ast_name())
        });

        methods.add_method("add_lifetime", |_lua_ctx, this, string: LuaString| {
            let lt_str = string.to_str()?;
            let mut lt_string = String::with_capacity(lt_str.len() + 1);

            lt_string.push('\'');
            lt_string.push_str(lt_str);

            let generic_param = GenericParam {
                id: DUMMY_NODE_ID,
                ident: Ident::from_str(&lt_string),
                attrs: Default::default(),
                bounds: Vec::new(),
                kind: GenericParamKind::Lifetime,
            };

            if let ItemKind::Struct(_, generics) = &mut *this.borrow_mut() {
                let diff = |p: &GenericParam| {
                    if let GenericParamKind::Lifetime = p.kind {
                        p.ident == generic_param.ident
                    } else {
                        false
                    }
                };

                if generics.params.iter().any(diff) {
                    return Ok(());
                }

                generics.params.push(generic_param);
            }

            Ok(())
        });

        methods.add_method("get_field_ids", |_lua_ctx, this, ()| {
            if let ItemKind::Struct(variant_data, _) = &*this.borrow() {
                return Ok(Some(variant_data
                    .fields()
                    .iter()
                    .map(|f| f.id.as_u32())
                    .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });

        methods.add_method("print", |_lua_ctx, this, ()| {
            println!("{:?}", this.borrow());

            Ok(())
        });
    }
}
