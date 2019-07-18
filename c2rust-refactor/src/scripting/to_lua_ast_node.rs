use std::cell::{Ref, RefCell};
use std::mem::swap;
use std::ops::DerefMut;
use std::sync::Arc;

use rustc::hir::def::Res;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::mut_visit::*;
use syntax::source_map::DUMMY_SP;
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
            Ok(this.0.borrow().node.ast_name())
        });

        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.0.borrow().node.clone() {
                ExprKind::Lit(x) => x.to_lua(lua_ctx),
                node => Err(Error::external(format!("Expr node {:?} not implemented yet", node))),
            }
        })
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
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.0.borrow().node.ast_name())
        });

        methods.add_method("get_mut_ty", |_lua_ctx, this, ()| {
            match &this.0.borrow().node {
                TyKind::Ptr(mut_ty) |
                TyKind::Rptr(_, mut_ty) => Ok(Some(LuaAstNode::new(mut_ty.clone()))),
                _ => Ok(None),
            }
        });

        methods.add_method("to_rptr", |_lua_ctx, this, (_lt, mut_ty): (Option<LuaString>, LuaAstNode<MutTy>)| {
            // TODO: Support explicit lifetimes

            this.0.borrow_mut().node = TyKind::Rptr(None, mut_ty.0.borrow().clone());

            Ok(())
        });

        methods.add_method("wrap_in_slice", |_lua_ctx, this, ()| {
            let mut ty = this.0.borrow_mut();
            let mut placeholder = TyKind::Err;

            swap(&mut placeholder, &mut ty.node);

            ty.node = TyKind::Slice(P(Ty {
                id: DUMMY_NODE_ID,
                node: placeholder,
                span: DUMMY_SP,
            }));

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
impl UserData for LuaAstNode<P<Local>> {}


/// Lit AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type LitAstNode
unsafe impl Send for LuaAstNode<Lit> {}
impl UserData for LuaAstNode<Lit> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_value", |lua_ctx, this, ()| {
            match this.0.borrow().node {
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
            Ok(this.0.borrow().kind.to_lua(lua_ctx))
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.0.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.0.borrow().ident.to_lua(lua_ctx)
        });

        methods.add_method("has_block", |lua_ctx, this, ()| {
            this.0.borrow().block.is_some().to_lua(lua_ctx)
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
            Ok(this.0
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
            this.0.borrow().id.to_lua(lua_ctx)
        });

        methods.add_method("get_ty", |_lua_ctx, this, ()| {
            Ok(LuaAstNode::new(this.0.borrow().ty.clone()))
        });

        methods.add_method("set_ty", |_lua_ctx, this, ty: LuaAstNode<P<Ty>>| {
            this.0.borrow_mut().ty = ty.0.borrow().clone();

            Ok(())
        });
    }
}
