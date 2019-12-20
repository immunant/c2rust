use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Debug;
use std::mem::swap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::Arc;

use c2rust_ast_builder::mk;
use rustc::hir::def::Res;
use rustc::hir::HirId;
use rustc_target::spec::abi::{Abi, lookup as lookup_abi};
use syntax::ast::*;
use syntax::ptr::P;
use syntax::mut_visit::*;
use syntax::token::{BinOpToken, DelimToken, Nonterminal, Token, TokenKind};
use syntax::token::{Lit as TokenLit, LitKind as TokenLitKind};
use syntax::source_map::{DUMMY_SP, Spanned, dummy_spanned};
use syntax::symbol::{Symbol, sym};
use syntax::tokenstream::{DelimSpan, TokenStream, TokenTree};
use syntax::ThinVec;
use syntax_pos::{Span, SyntaxContext};

use rlua::{Context, Error, FromLua, Function, MetaMethod, Result, Scope, ToLua, UserData, UserDataMethods, Value};
use rlua::prelude::{LuaString, LuaTable};

use crate::ast_manip::{util, visit_nodes, AstName, AstNode, WalkAst};
use crate::ast_manip::fn_edit::{FnLike, FnKind};
use super::DisplayLuaError;

/// Refactoring module
// @module Refactor

fn find_subexpr<'a, C>(expr: &'a mut P<Expr>, cmp: &C) -> Result<Option<&'a mut P<Expr>>>
where
    C: Fn(&P<Expr>) -> bool,
{
    if cmp(expr) {
        return Ok(Some(expr));
    }

    match &mut expr.kind {
        ExprKind::Box(expr)
        | ExprKind::Unary(_, expr)
        | ExprKind::Cast(expr, _)
        | ExprKind::Type(expr, _)
        | ExprKind::While(expr, _, _)
        | ExprKind::ForLoop(_, expr, _, _)
        | ExprKind::Match(expr, _)
        | ExprKind::Closure(_, _, _, _, expr, _)
        | ExprKind::Await(expr)
        | ExprKind::Field(expr, _)
        | ExprKind::AddrOf(_, _, expr)
        | ExprKind::Repeat(expr, _)
        | ExprKind::Paren(expr)
        | ExprKind::Try(expr) => {
            if let Some(e) = find_subexpr(expr, cmp)? {
                return Ok(Some(e));
            };
        },
        ExprKind::Array(exprs)
        | ExprKind::MethodCall(_, exprs)
        | ExprKind::Tup(exprs) => {
            for expr in exprs.iter_mut() {
                if let Some(e) = find_subexpr(expr, cmp)? {
                    return Ok(Some(e));
                };
            }
        },
        ExprKind::Call(expr, exprs) => {
            if let Some(e) = find_subexpr(expr, cmp)? {
                return Ok(Some(e));
            };

            for expr in exprs.iter_mut() {
                if let Some(e) = find_subexpr(expr, cmp)? {
                    return Ok(Some(e));
                };
            }
        },
        ExprKind::Binary(_, expr1, expr2)
        | ExprKind::Assign(expr1, expr2)
        | ExprKind::AssignOp(_, expr1, expr2)
        | ExprKind::Index(expr1, expr2) => {
            if let Some(e) = find_subexpr(expr1, cmp)? {
                return Ok(Some(e));
            };

            if let Some(e) = find_subexpr(expr2, cmp)? {
                return Ok(Some(e));
            };
        },
        ExprKind::If(expr, _, opt_expr) => {
            if let Some(e) = find_subexpr(expr, cmp)? {
                return Ok(Some(e));
            };

            if let Some(expr) = opt_expr {
                if let Some(e) = find_subexpr(expr, cmp)? {
                    return Ok(Some(e));
                };
            }
        },
        _ => (),
    };

    Ok(None)
}

pub(crate) trait ToLuaExt {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>>;
}

pub(crate) trait ToLuaAstNode {
    fn to_lua_ast_node<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>>;
}

pub(crate) trait ToLuaScoped {
    fn to_lua_scoped<'lua, 'scope>(self, lua: Context<'lua>, scope: &Scope<'lua, 'scope>) -> Result<Value<'lua>>;
}

// Marker trait for types that are safe to always pass to Lua as `LuaAstNode`s.
// For now, everything implements it except for `Vec<Stmt>`. This lets
// us send `Vec<Stmt>` values as Lua tables using `to_lua_ext` and
// as `LuaAstNode`s using `to_lua_ast_node`
pub(crate) trait LuaAstNodeSafe {}

impl<T> ToLuaExt for T
    where T: Sized + ToLuaAstNode,
          LuaAstNode<T>: 'static + UserData + Send + LuaAstNodeSafe,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua_ast_node(lua)
    }
}

impl<T> ToLuaExt for &T
    where T: Sized + ToLuaExt + Clone,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        (*self).clone().to_lua_ext(lua)
    }
}

impl<T> ToLuaExt for Rc<T>
    where T: Sized + ToLuaExt + Clone,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        (*self).clone().to_lua_ext(lua)
    }
}

impl<T> ToLuaExt for Option<T>
    where T: Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        if let Some(x) = self {
            x.to_lua_ext(lua)
        } else {
            Ok(Value::Nil)
        }
    }
}

impl<T> ToLuaExt for Vec<T>
    where T: Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        let vec = self.into_iter()
            .map(|x| x.to_lua_ext(lua))
            .collect::<Result<Vec<_>>>()?;
        vec.to_lua(lua)
    }
}

impl<T> ToLuaExt for ThinVec<T>
    where T: Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        let v: Vec<T> = self.into();
        v.to_lua_ext(lua)
    }
}

impl<A, B> ToLuaExt for (A, B)
    where A: Sized + ToLuaExt,
          B: Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        let (a, b) = self;
        vec![a.to_lua_ext(lua)?, b.to_lua_ext(lua)?].to_lua(lua)
    }
}

impl<A, B, C> ToLuaExt for (A, B, C)
    where A: Sized + ToLuaExt,
          B: Sized + ToLuaExt,
          C: Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        let (a, b, c) = self;
        vec![a.to_lua_ext(lua)?, b.to_lua_ext(lua)?, c.to_lua_ext(lua)?].to_lua(lua)
    }
}

impl<A, B, C> ToLuaExt for P<(A, B, C)>
    where A: 'static + Sized + ToLuaExt,
          B: 'static + Sized + ToLuaExt,
          C: 'static + Sized + ToLuaExt,
{
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        let (a, b, c) = self.into_inner();
        vec![a.to_lua_ext(lua)?, b.to_lua_ext(lua)?, c.to_lua_ext(lua)?].to_lua(lua)
    }
}


// Manual `ToLuaExt` implementation for leaf types
impl ToLuaExt for bool {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua(lua)
    }
}

impl ToLuaExt for u8 {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua(lua)
    }
}

impl ToLuaExt for u16 {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua(lua)
    }
}

impl ToLuaExt for u128 {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua(lua)
    }
}

impl ToLuaExt for usize {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_lua(lua)
    }
}

impl ToLuaExt for char {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.to_string().to_lua(lua)
    }
}

impl ToLuaExt for NodeId {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.as_u32().to_lua(lua)
    }
}

impl ToLuaExt for Symbol {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.as_str().to_lua(lua)
    }
}

impl ToLuaExt for AttrId {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.0.to_lua(lua)
    }
}

impl ToLuaExt for Abi {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        self.name().to_lua(lua)
    }
}


// The other Lua traits
impl<T> ToLuaAstNode for T
    where T: Sized,
          LuaAstNode<T>: 'static + UserData + Send,
{
    fn to_lua_ast_node<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
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


// FromLua traits
/// Converts from a Lua `Value` directly into an AST node of type `T`.
pub(crate) trait FromLuaExt: Sized {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self>;
}

/// Converts from a Lua `Value` into a `LuaAstNode<T>` object.
pub(crate) trait FromLuaAstNode: Sized {
    fn from_lua_ast_node<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self>;
}

/// Internal trait that converts a Lua `Table` into an AST node.
pub(crate) trait FromLuaTable: Sized {
    fn from_lua_table<'lua>(table: LuaTable<'lua>, lua: Context<'lua>) -> Result<Self>;
}

/// Internal trait that tries to convert a string into an enum object.
pub(crate) trait TryFromString: Sized {
    fn try_from_string(s: &str) -> Option<Self>;
}

/// Default implementation for `TryFromString`. Most things don't convert.
impl<T> TryFromString for T {
    default fn try_from_string(_str: &str) -> Option<Self> {
        None
    }
}

impl<T> FromLuaExt for T
    where T: 'static + Sized + Clone + FromLuaTable + TryFromString,
          LuaAstNode<T>: UserData + LuaAstNodeSafe,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        match value {
            Value::UserData(ud) => {
                let node = &*ud.borrow::<LuaAstNode<T>>()?;
                let node = node.borrow().clone();
                Ok(node)
            }

            Value::Table(t) => FromLuaTable::from_lua_table(t, lua),

            Value::String(s) => TryFromString::try_from_string(s.to_str()?).ok_or_else(|| {
                Error::FromLuaConversionError {
                    from: "String",
                    to: std::any::type_name::<T>(),
                    message: None,
                }
            }),

            Value::Nil => Err(Error::FromLuaConversionError {
                from: "Nil",
                to: std::any::type_name::<T>(),
                message: None,
            }),

            _ => Err(Error::FromLuaConversionError {
                // FIXME: we should get this from `value.type_name()`,
                // but that method is currently private, see
                // https://github.com/kyren/rlua/issues/58
                from: "Value",
                to: std::any::type_name::<T>(),
                message: None,
            })
        }
    }
}

impl<T> FromLuaAstNode for LuaAstNode<T>
    where T: 'static + Sized + FromLuaTable + TryFromString,
          LuaAstNode<T>: UserData + Clone,
{
    fn from_lua_ast_node<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        match value {
            Value::UserData(ud) => {
                let node = &*ud.borrow::<LuaAstNode<T>>()?;
                Ok(node.clone())
            }

            Value::Table(t) => {
                let node = FromLuaTable::from_lua_table(t, lua)?;
                Ok(LuaAstNode::new(node))
            }

            Value::String(s) => TryFromString::try_from_string(s.to_str()?).ok_or_else(|| {
                Error::FromLuaConversionError {
                    from: "String",
                    to: std::any::type_name::<T>(),
                    message: None,
                }
            }).map(LuaAstNode::new),

            Value::Nil => Err(Error::FromLuaConversionError {
                from: "Nil",
                to: std::any::type_name::<T>(),
                message: None,
            }),

            _ => Err(Error::FromLuaConversionError {
                from: "Value",
                to: std::any::type_name::<T>(),
                message: None,
            })
        }
    }
}

impl<T> FromLuaExt for Rc<T>
    where T: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let x: T = FromLuaExt::from_lua_ext(value, lua)?;
        Ok(Rc::new(x))
    }
}

impl<T> FromLuaExt for Option<T>
    where T: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        if let Value::Nil = value {
            Ok(None)
        } else {
            FromLuaExt::from_lua_ext(value, lua).map(Some)
        }
    }
}

impl<T> FromLuaExt for Vec<T>
    where T: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let v: Vec<Value> = FromLua::from_lua(value, lua)?;
        v.into_iter()
            .map(|v| FromLuaExt::from_lua_ext(v, lua))
            .collect::<Result<Vec<_>>>()
    }
}

impl<T> FromLuaExt for ThinVec<T>
    where T: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        if let Value::Nil = value {
            return Ok(ThinVec::new());
        }
        let v: Vec<T> = FromLuaExt::from_lua_ext(value, lua)?;
        Ok(v.into())
    }
}

impl<A, B> FromLuaExt for (A, B)
    where A: FromLuaExt,
          B: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let err_fn = || Error::FromLuaConversionError {
            from: "Value",
            to: "Tuple",
            message: Some("tuple table must have at least 2 elements".to_string()),
        };

        let mut v: Vec<Value> = FromLua::from_lua(value, lua)?;
        let b = v.pop().ok_or_else(err_fn)?;
        let a = v.pop().ok_or_else(err_fn)?;
        Ok((FromLuaExt::from_lua_ext(a, lua)?,
            FromLuaExt::from_lua_ext(b, lua)?,
        ))
    }
}

impl<A, B, C> FromLuaExt for (A, B, C)
    where A: FromLuaExt,
          B: FromLuaExt,
          C: FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let err_fn = || Error::FromLuaConversionError {
            from: "Value",
            to: "Tuple",
            message: Some("tuple table must have at least 3 elements".to_string()),
        };

        let mut v: Vec<Value> = FromLua::from_lua(value, lua)?;
        let c = v.pop().ok_or_else(err_fn)?;
        let b = v.pop().ok_or_else(err_fn)?;
        let a = v.pop().ok_or_else(err_fn)?;
        Ok((FromLuaExt::from_lua_ext(a, lua)?,
            FromLuaExt::from_lua_ext(b, lua)?,
            FromLuaExt::from_lua_ext(c, lua)?,
        ))
    }
}

impl<A, B, C> FromLuaExt for P<(A, B, C)>
    where A: 'static + FromLuaExt,
          B: 'static + FromLuaExt,
          C: 'static + FromLuaExt,
{
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let err_fn = || Error::FromLuaConversionError {
            from: "Value",
            to: "Tuple",
            message: Some("tuple table must have at least 3 elements".to_string()),
        };

        let mut v: Vec<Value> = FromLua::from_lua(value, lua)?;
        let c = v.pop().ok_or_else(err_fn)?;
        let b = v.pop().ok_or_else(err_fn)?;
        let a = v.pop().ok_or_else(err_fn)?;
        Ok(P((FromLuaExt::from_lua_ext(a, lua)?,
              FromLuaExt::from_lua_ext(b, lua)?,
              FromLuaExt::from_lua_ext(c, lua)?,
        )))
    }
}

impl FromLuaExt for bool {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        FromLua::from_lua(value, lua)
    }
}

impl FromLuaExt for u8 {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        FromLua::from_lua(value, lua)
    }
}

impl FromLuaExt for u16 {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        FromLua::from_lua(value, lua)
    }
}

impl FromLuaExt for u128 {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        FromLua::from_lua(value, lua)
    }
}

impl FromLuaExt for usize {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        FromLua::from_lua(value, lua)
    }
}

impl FromLuaExt for char {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let s: String = FromLua::from_lua(value, lua)?;
        s.chars().next().ok_or_else(|| Error::FromLuaConversionError {
            from: "String",
            to: "char",
            message: Some(format!("invalid single-character string: {}", s)),
        })
    }
}

impl FromLuaExt for NodeId {
    fn from_lua_ext<'lua>(value: Value<'lua>, _lua: Context<'lua>) -> Result<Self> {
        match value {
            Value::Integer(x) => Ok(NodeId::from_u32(x as u32)),

            Value::Nil => Ok(DUMMY_NODE_ID),

            _ => Err(Error::FromLuaConversionError {
                from: "Value",
                to: "NodeId",
                message: None,
            })
        }
    }
}

impl FromLuaExt for Symbol {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let s: String = FromLua::from_lua(value, lua)?;
        Ok(Symbol::intern(&s))
    }
}

impl FromLuaExt for AttrId {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let id: usize = FromLua::from_lua(value, lua)?;
        Ok(AttrId(id))
    }
}

impl FromLuaExt for Abi {
    fn from_lua_ext<'lua>(value: Value<'lua>, lua: Context<'lua>) -> Result<Self> {
        let abi: String = FromLua::from_lua(value, lua)?;
        lookup_abi(&abi).ok_or_else(|| Error::FromLuaConversionError {
            from: "String",
            to: "Abi",
            message: Some(format!("unknown ABI: {}", abi)),
        })
    }
}

/// Holds a rustc AST node that can be passed back and forth to Lua as a scoped,
/// static userdata. Implement AddMoreMethods for LuaAstNode<T> to support an AST node
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
        self.borrow_mut().walk(visitor);
    }
}

pub(crate) trait AddMoreMethods: UserData {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M);
}

impl<T: UserData> AddMoreMethods for T {
    default fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(_methods: &mut M) {}
}

// Helper functions for lua_ast_node_gen.inc.rs
fn from_lua_kind_error<T>(expected: &'static str, actual: &str) -> Result<T> {
    Err(Error::FromLuaConversionError {
        from: "Table",
        to: expected,
        message: Some(format!("expected kind {}, got {}", expected, actual)),
    })
}

fn from_lua_prepend_field<T>(field: &'static str, res: Result<T>) -> Result<T> {
    match res {
        Err(Error::FromLuaConversionError { from, to, message: None }) => {
            Err(Error::FromLuaConversionError {
                from,
                to,
                message: Some(format!("field '{}' in '{}'", field,
                                      std::any::type_name::<T>())),
            })
        }

        Err(Error::FromLuaConversionError { from, to, message: Some(msg) }) => {
            Err(Error::FromLuaConversionError {
                from,
                to,
                message: Some(format!("field '{}' in '{}': {}", field,
                                      std::any::type_name::<T>(), msg)),
            })
        }

        _ => res
    }
}

include!(concat!(env!("OUT_DIR"), "/lua_ast_node_gen.inc.rs"));

unsafe impl<T> Send for LuaAstNode<Spanned<T>> {}
impl<T> LuaAstNodeSafe for LuaAstNode<Spanned<T>> {}
impl<T> UserData for LuaAstNode<Spanned<T>>
    where T: ToLuaExt + FromLuaExt + Clone + Debug,
{
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_node", |lua_ctx, this, ()| {
          Ok(this.borrow().node.clone().to_lua_ext(lua_ctx))
        });
        methods.add_method("set_node", |lua_ctx, this, (value,)| {
          this.borrow_mut().node = FromLuaExt::from_lua_ext(value, lua_ctx)?;
          Ok(())
        });
        methods.add_method("get_span", |lua_ctx, this, ()| {
          Ok(this.borrow().span.clone().to_lua_ext(lua_ctx))
        });
        methods.add_method("set_span", |lua_ctx, this, (value,)| {
          this.borrow_mut().span = FromLuaExt::from_lua_ext(value, lua_ctx)?;
          Ok(())
        });
        methods.add_meta_method(
          MetaMethod::ToString,
          |_lua_ctx, this, ()| Ok(format!("{:?}", this.borrow())),
        );
    }
}

impl<T> FromLuaTable for Spanned<T>
    where T: 'static + Clone + FromLuaTable,
          LuaAstNode<T>: UserData + LuaAstNodeSafe,
{
  fn from_lua_table<'lua>(table: LuaTable<'lua>, lua_ctx: Context<'lua>) -> Result<Self> {
    Ok(Spanned::<T> {
      node: FromLuaExt::from_lua_ext(table.get::<_, Value>("node")?, lua_ctx)?,
      span: FromLuaExt::from_lua_ext(table.get::<_, Value>("span")?, lua_ctx)?,
    })
  }
}

#[allow(unused_doc_comments)]
impl AddMoreMethods for LuaAstNode<P<Item>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        /// Visit statements
        // @function visit_stmts
        // @tparam function(LuaAstNode) callback Function to call when visiting each statement
        methods.add_method("visit_stmts", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &Stmt| {
                callback.call::<_, ()>(node.clone().to_lua_ext(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_stmts: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("visit_items", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &Item| {
                callback.call::<_, ()>(P(node.clone()).to_lua_ext(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_items: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("visit_foreign_items", |lua_ctx, this, callback: Function| {
            visit_nodes(&**this.borrow(), |node: &ForeignItem| {
                callback.call::<_, ()>(node.clone().to_lua_ext(lua_ctx))
                .unwrap_or_else(|e| panic!("Lua callback failed in visit_foreign_items: {}", DisplayLuaError(e)));
            });
            Ok(())
        });

        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.borrow().kind.clone() {
                ItemKind::Use(e) => Ok(e.to_lua_ext(lua_ctx)),
                node => Err(Error::external(format!("Item node {:?} not implemented yet", node))),
            }
        });

        methods.add_method("get_fields", |_lua_ctx, this, ()| {
            if let ItemKind::Struct(variant_data, _) = &this.borrow().kind {
                return Ok(Some(variant_data
                    .fields()
                    .iter()
                    .map(|f| LuaAstNode::new(f.clone()))
                    .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });

        methods.add_method("get_arg_ids", |_lua_ctx, this, ()| {
            if let ItemKind::Fn(sig, ..) = &this.borrow().kind {
                return Ok(Some(sig
                               .decl
                               .inputs
                               .iter()
                               .map(|a| a.id.as_u32())
                               .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });

        methods.add_method("get_args", |_lua_ctx, this, ()| {
            if let ItemKind::Fn(sig, ..) = &this.borrow().kind {
                return Ok(Some(sig.decl
                    .inputs
                    .iter()
                    .map(|a| LuaAstNode::new(a.clone()))
                    .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });

        methods.add_method("set_args", |lua_ctx, this, args: Vec<Value>| {
            if let ItemKind::Fn(sig, ..) = &mut this.borrow_mut().kind {
                sig.decl.inputs = args.into_iter()
                    .map(|a| FromLuaExt::from_lua_ext(a, lua_ctx))
                    .collect::<Result<Vec<_>>>()?;
            }

            Ok(())
        });

        methods.add_method("add_lifetime", |_lua_ctx, this, string: LuaString| {
            let lt_str = string.to_str()?;

            add_item_lifetime(&mut this.borrow_mut().kind, lt_str);

            Ok(())
        });

        methods.add_method("get_trait_ref", |_lua_ctx, this, ()| {
            if let ItemKind::Impl(_, _, _, _, opt_trait_ref, ..) = &this.borrow().kind {
                return Ok(opt_trait_ref.as_ref().map(|tr| LuaAstNode::new(tr.path.clone())));
            }

            Ok(None)
        });

        // TODO: This needs to be tested when the bitfields derive is present
        methods.add_method("remove_copy_derive", |_lua_ctx, this, ()| {
            let attrs = &mut this.borrow_mut().attrs;

            let opt_idx = attrs.iter().position(|a| a.check_name(Symbol::intern("rustc_copy_clone_marker")));

            if let Some(idx) = opt_idx {
                attrs.remove(idx);
            }

            attrs.push(mk().call_attr("derive", vec!["Clone"]).into_attrs().remove(0));

            Ok(())
        });
    }
}

impl AddMoreMethods for LuaAstNode<ForeignItem> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_args", |_lua_ctx, this, ()| {
            if let ForeignItemKind::Fn(decl, ..) = &this.borrow().kind {
                return Ok(Some(decl
                    .inputs
                    .iter()
                    .map(|a| LuaAstNode::new(a.clone()))
                    .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });
    }
}

impl AddMoreMethods for LuaAstNode<QSelf> {}


impl AddMoreMethods for LuaAstNode<Path> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("has_generic_args", |_lua_ctx, this, ()| {
            Ok(this.borrow().segments.iter().any(|s| s.args.is_some()))
        });
        methods.add_method("set_segments", |_lua_ctx, this, new_segments: Vec<LuaString>| {
            let has_generic_args = this.borrow().segments.iter().any(|s| s.args.is_some());
            if has_generic_args {
                Err(Error::external("One or more path segments have generic args, cannot set segments as strings"))
            } else {
                this.borrow_mut().segments = new_segments.into_iter().map(|new_seg| {
                    Ok(PathSegment::from_ident(Ident::from_str(new_seg.to_str()?)))
                }).collect::<Result<Vec<_>>>()?;
                Ok(())
            }
        });
        methods.add_method("map_segments", |lua_ctx, this, callback: Function| {
            let new_segments = lua_ctx.scope(|scope| {
                let segments = this.borrow().segments.iter().map(|s| scope.create_static_userdata(LuaAstNode::new(s.clone())).unwrap()).collect::<Vec<_>>().to_lua(lua_ctx);
                callback.call(segments)
            }).unwrap();
            this.borrow_mut().segments = FromLuaExt::from_lua_ext(new_segments, lua_ctx)?;
            Ok(())
        });

        methods.add_method("set_generic_angled_arg_tys", |lua_ctx, this, (idx, tys): (usize, Vec<Value>)| {
            let args = tys.into_iter()
                .map(|ty| FromLuaExt::from_lua_ext(ty, lua_ctx))
                .map(|ty| ty.map(GenericArg::Type))
                .collect::<Result<Vec<_>>>()?;
            let generic_arg = GenericArgs::AngleBracketed(AngleBracketedArgs {
                span: DUMMY_SP,
                args,
                constraints: Vec::new(),
            });

            // Using 1-offset idx like lua does
            this.borrow_mut().segments[idx - 1].args = Some(P(generic_arg));

            Ok(())
        });
    }
}
// FIXME: implement the above for `P<Path>`; it's non-trivial
// because `UserDataMethods` is bound to `LuaAstNode<Path>`,
// but we need to implement it for `LuaAstNode<P<Path>>`

impl AddMoreMethods for LuaAstNode<PathSegment> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(_methods: &mut M) {
    }
}


/// Def result AST node handle
//
// This object is NOT thread-safe. Do not use an object of this class from a
// thread that did not acquire it.
// @type DefAstNode
unsafe impl Send for LuaAstNode<Res> {}
impl LuaAstNodeSafe for LuaAstNode<Res> {}
impl UserData for LuaAstNode<Res> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_namespace", |_lua_ctx, this, ()| {
            Ok(util::namespace(&*this.borrow()).map(|namespace| namespace.descr()))
        });
        methods.add_meta_method(
          MetaMethod::ToString,
          |_lua_ctx, this, ()| Ok(format!("{:?}", this.borrow())),
        );
    }
}

#[derive(Clone)]
struct SpanData(syntax_pos::SpanData);

impl UserData for SpanData {}

impl ToLuaExt for Span {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        lua.create_userdata(SpanData(self.data()))?.to_lua(lua)
    }
}

impl FromLuaExt for Span {
    fn from_lua_ext<'lua>(value: Value<'lua>, _lua: Context<'lua>) -> Result<Self> {
        match value {
            Value::UserData(ud) => {
                let sd = ud.borrow::<SpanData>()?;
                Ok(sd.0.with_ctxt(sd.0.ctxt))
            }

            Value::Nil => Ok(DUMMY_SP),

            _ => Err(Error::FromLuaConversionError {
                from: "Value",
                to: "Span",
                message: None,
            })
        }
    }
}

fn get_iter_next<T, I: Iterator<Item = T>>(iter: &mut I) -> Result<T> {
    iter.next().ok_or_else(|| {
        Error::FromLuaConversionError {
            from: "Table",
            to: "Vec",
            message: Some("not enough elements in table".to_string()),
        }
    })
}

impl AddMoreMethods for LuaAstNode<P<Expr>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.borrow().kind.clone() {
                ExprKind::Lit(x) => x.to_lua_ext(lua_ctx),
                node => Err(Error::external(format!("Expr node {:?} not implemented yet", node))),
            }
        });

        methods.add_method("get_ty", |lua_ctx, this, ()| {
            match &this.borrow().kind {
                ExprKind::Cast(_, ty)
                | ExprKind::Type(_, ty) => ty.to_lua_ext(lua_ctx),
                e => unimplemented!("LuaAstNode<P<Expr>>:get_ty() for {}", e.ast_name()),
            }
        });

        methods.add_method("set_ty", |lua_ctx, this, lty: Value| {
            match &mut this.borrow_mut().kind {
                ExprKind::Cast(_, ty)
                | ExprKind::Type(_, ty) => *ty = FromLuaExt::from_lua_ext(lty, lua_ctx)?,
                e => unimplemented!("LuaAstNode<P<Expr>>:set_ty() for {}", e.ast_name()),
            }

            Ok(())
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            match &this.borrow().kind {
                ExprKind::Field(_, ident) => ident.to_lua_ext(lua_ctx).map(Some),
                _ => Ok(None),
            }
        });

        methods.add_method("get_path", |_lua_ctx, this, ()| {
            match &this.borrow().kind {
                ExprKind::Path(_, path)
                | ExprKind::Struct(path, ..) => Ok(Some(LuaAstNode::new(path.clone()))),
                _ => Ok(None),
            }
        });

        methods.add_method("get_exprs", |lua_ctx, this, ()| {
            match &this.borrow().kind {
                ExprKind::Cast(expr, _)
                | ExprKind::Field(expr, _)
                | ExprKind::Unary(_, expr) => vec![expr.clone()],
                ExprKind::MethodCall(_, exprs) => exprs.clone(),
                ExprKind::Assign(lhs, rhs)
                | ExprKind::AssignOp(_, lhs, rhs)
                | ExprKind::Binary(_, lhs, rhs) => vec![lhs.clone(), rhs.clone()],
                ExprKind::Call(func, params) => {
                    let mut exprs = Vec::with_capacity(params.len() + 1);

                    exprs.push(func.clone());

                    for param in params {
                        exprs.push(param.clone());
                    }

                    exprs
                },
                _ => Vec::new(),
            }.to_lua_ext(lua_ctx)
        });

        methods.add_method("set_exprs", |lua_ctx, this, exprs: Vec<Value>| {
            let mut exprs = exprs.into_iter()
                .map(|e| FromLuaExt::from_lua_ext(e, lua_ctx));
            match &mut this.borrow_mut().kind {
                ExprKind::Field(expr, _)
                | ExprKind::Unary(_, expr)
                | ExprKind::Cast(expr, _)
                | ExprKind::AddrOf(_, _, expr) => *expr = get_iter_next(&mut exprs)??,

                ExprKind::MethodCall(_, old_exprs) => {
                    let mut exprs = exprs.collect::<Result<Vec<_>>>()?;
                    std::mem::swap(old_exprs, &mut exprs);
                }

                ExprKind::Call(func, params) => {
                    *func = get_iter_next(&mut exprs)??;
                    *params = exprs.collect::<Result<Vec<_>>>()?;
                },

                ExprKind::Assign(lhs, rhs)
                | ExprKind::AssignOp(_, lhs, rhs)
                | ExprKind::Binary(_, lhs, rhs) => {
                    *lhs = get_iter_next(&mut exprs)??;
                    *rhs = get_iter_next(&mut exprs)??;
                },

                e => unimplemented!("LuaAstNode<P<Expr>>:set_exprs() for {}", e.ast_name()),
            }

            Ok(())
        });

        methods.add_method("get_op", |_lua_ctx, this, ()| {
            match &this.borrow().kind {
                ExprKind::Unary(op, _) => Ok(Some(op.ast_name())),
                ExprKind::Binary(op, ..) |
                ExprKind::AssignOp(op, ..) => Ok(Some(op.ast_name())),
                _ => Ok(None),
            }
        });

        methods.add_method("get_method_name", |lua_ctx, this, ()| {
            if let ExprKind::MethodCall(path_seg, ..) = &this.borrow().kind {
                return path_seg.ident.as_str().to_lua(lua_ctx).map(Some)
            }

            Ok(None)
        });

        methods.add_method("set_method_name", |_lua_ctx, this, name: LuaString| {
            if let ExprKind::MethodCall(path_seg, ..) = &mut this.borrow_mut().kind {
                path_seg.ident = Ident::from_str(name.to_str()?);
            }

            Ok(())
        });

        methods.add_method("to_lit", |lua_ctx, this, lit: Value| {
            let lit = FromLuaExt::from_lua_ext(lit, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Lit(lit);

            Ok(())
        });

        methods.add_method("to_index", |lua_ctx, this, (indexed, indexee): (Value, Value)| {
            let indexed = FromLuaExt::from_lua_ext(indexed, lua_ctx)?;
            let indexee = FromLuaExt::from_lua_ext(indexee, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Index(indexed, indexee);

            Ok(())
        });

        methods.add_method("to_range", |lua_ctx, this, (lhs, rhs): (Value, Value)| {
            let lhs = FromLuaExt::from_lua_ext(lhs, lua_ctx)?;
            let rhs = FromLuaExt::from_lua_ext(rhs, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Range(lhs, rhs, RangeLimits::HalfOpen);

            Ok(())
        });

        methods.add_method("to_binary", |lua_ctx, this, (op, lhs, rhs): (LuaString, Value, Value)| {
            let op = match op.to_str()? {
                "Add" => BinOpKind::Add,
                "Div" => BinOpKind::Div,
                _ => unimplemented!("BinOpKind parsing from string"),
            };
            let lhs = FromLuaExt::from_lua_ext(lhs, lua_ctx)?;
            let rhs = FromLuaExt::from_lua_ext(rhs, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Binary(dummy_spanned(op), lhs, rhs);

            Ok(())
        });

        methods.add_method("to_unary", |lua_ctx, this, (op, expr): (LuaString, Value)| {
            let op = match op.to_str()? {
                "Deref" => UnOp::Deref,
                _ => unimplemented!("UnOp parsing from string"),
            };
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Unary(op, expr);

            Ok(())
        });

        methods.add_method("to_call", |lua_ctx, this, exprs: Vec<Value>| {
            let mut expr_iter = exprs.into_iter();
            let func = FromLuaExt::from_lua_ext(get_iter_next(&mut expr_iter)?, lua_ctx)?;
            let params = expr_iter
                .map(|lan| FromLuaExt::from_lua_ext(lan, lua_ctx))
                .collect::<Result<Vec<_>>>()?;

            this.borrow_mut().kind = ExprKind::Call(func, params);

            Ok(())
        });

        methods.add_method("to_cast", |lua_ctx, this, (expr, ty): (Value, Value)| {
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;
            let ty = FromLuaExt::from_lua_ext(ty, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Cast(expr, ty);

            Ok(())
        });

        methods.add_method("to_addr_of", |lua_ctx, this, (expr, mutable): (Value, bool)| {
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;
            let mutability = if mutable {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            };

            this.borrow_mut().kind = ExprKind::AddrOf(BorrowKind::Ref, mutability, expr);

            Ok(())
        });

        methods.add_method(
            "to_block",
            |lua_ctx, this, (stmts, label, is_safe): (Vec<Value>, Option<LuaString>, bool)|
        {
            let label = match label {
                Some(s) => Some(Label { ident: Ident::from_str(s.to_str()?) }),
                None => None,
            };
            let rules = if is_safe {
                BlockCheckMode::Default
            } else {
                BlockCheckMode::Unsafe(UnsafeSource::UserProvided)
            };
            let stmts = stmts.into_iter()
                .map(|s| FromLuaExt::from_lua_ext(s, lua_ctx))
                .collect::<Result<Vec<_>>>()?;
            let block = Block {
                stmts,
                id: DUMMY_NODE_ID,
                rules,
                span: DUMMY_SP,
            };

            this.borrow_mut().kind = ExprKind::Block(P(block), label);

            Ok(())
        });

        methods.add_method("to_method_call", |lua_ctx, this, (segment, exprs): (LuaString, Vec<Value>)| {
            let segment = PathSegment::from_ident(Ident::from_str(segment.to_str()?));
            let exprs = exprs.into_iter()
                .map(|e| FromLuaExt::from_lua_ext(e, lua_ctx))
                .collect::<Result<Vec<_>>>()?;

            this.borrow_mut().kind = ExprKind::MethodCall(segment, exprs);

            Ok(())
        });

        methods.add_method("to_closure", |lua_ctx, this, (params, expr): (Vec<LuaString>, Value)| {
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;
            let inputs: Result<_> = params.into_iter().map(|s| Ok(Param {
                attrs: ThinVec::new(),
                ty: P(Ty {
                    id: DUMMY_NODE_ID,
                    kind: TyKind::Infer,
                    span: DUMMY_SP,
                }),
                pat: P(Pat {
                    id: DUMMY_NODE_ID,
                    kind: PatKind::Ident(
                        BindingMode::ByValue(Mutability::Immutable),
                        Ident::from_str(s.to_str()?),
                        None,
                    ),
                    span: DUMMY_SP,
                }),
                id: DUMMY_NODE_ID,
                span: DUMMY_SP,
                is_placeholder: true,
            })).collect();
            let fn_decl = P(FnDecl {
                inputs: inputs?,
                output: FunctionRetTy::Default(DUMMY_SP),
            });

            this.borrow_mut().kind = ExprKind::Closure(
                CaptureBy::Ref,
                IsAsync::NotAsync,
                Movability::Movable,
                fn_decl,
                expr,
                DUMMY_SP,
            );

            Ok(())
        });

        methods.add_method("to_field", |lua_ctx, this, (expr, ident): (Value, LuaString)| {
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;

            this.borrow_mut().kind = ExprKind::Field(expr, Ident::from_str(ident.to_str()?));

            Ok(())
        });

        methods.add_method("to_ident_path", |_lua_ctx, this, path: LuaString| {
            let path = Path::from_ident(Ident::from_str(path.to_str()?));

            this.borrow_mut().kind = ExprKind::Path(None, path);

            Ok(())
        });

        methods.add_method("to_path", |lua_ctx, this, path: Value| {
            let path = FromLuaExt::from_lua_ext(path, lua_ctx)?;
            this.borrow_mut().kind = ExprKind::Path(None, path);

            Ok(())
        });

        methods.add_method("to_bool_lit", |_lua_ctx, this, b: bool| {
            let lit = Lit {
                token: TokenLit {
                    kind: TokenLitKind::Bool,
                    symbol: Symbol::intern(&format!("{}", b)),
                    suffix: None,
                },
                kind: LitKind::Bool(b),
                span: DUMMY_SP,
            };

            this.borrow_mut().kind = ExprKind::Lit(lit);

            Ok(())
        });

        methods.add_method("find_subexpr", |lua_ctx, this, id: u32| {
            let expr = &mut *this.borrow_mut();
            let node_id = NodeId::from_u32(id);
            let opt_expr = find_subexpr(expr, &|sub_expr| sub_expr.id == node_id)?;

            opt_expr.cloned().to_lua_ext(lua_ctx)
        });

        methods.add_method("filtermap_subexprs", |lua_ctx, this, (filter, map): (Function, Function)| {
            struct LuaFilterMapExpr<'lua> {
                filter: Function<'lua>,
                map: Function<'lua>,
                lua_ctx: Context<'lua>,
            }

            impl<'lua> MutVisitor for LuaFilterMapExpr<'lua> {
                fn visit_expr(&mut self, x: &mut P<Expr>) {
                    let is_end = self.filter
                        .call::<_, bool>(x.kind.ast_name())
                        .expect("Failed to call filter");

                    if is_end {
                        let new_x = self.map
                            .call(LuaAstNode::new(x.clone()))
                            .expect("Failed to call map");
                        *x = FromLuaExt::from_lua_ext(new_x, self.lua_ctx)
                            .expect("Failed to convert Lua value");
                    } else {
                        noop_visit_expr(x, self);
                    }
                }
            }

            let mut visitor = LuaFilterMapExpr {
                filter,
                map,
                lua_ctx,
            };

            visitor.visit_expr(&mut this.borrow_mut());

            Ok(())
        });
    }
}


impl AddMoreMethods for LuaAstNode<P<Ty>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_tys", |lua_ctx, this, ()| {
            match &this.borrow().kind {
                TyKind::Slice(ty)
                | TyKind::Array(ty, _) => {
                    vec![ty.clone()].to_lua_ext(lua_ctx)
                },
                | TyKind::Tup(tys) => tys.clone().to_lua_ext(lua_ctx),
                e => unimplemented!("LuaAstNode<P<Ty>>:get_tys() for {}", e.ast_name()),
            }
        });

        methods.add_method("set_tys", |lua_ctx, this, ltys: Vec<Value>| {
            let mut ltys = ltys.into_iter()
                .map(|lty| FromLuaExt::from_lua_ext(lty, lua_ctx))
                .collect::<Result<Vec<P<Ty>>>>()?;
            match &mut this.borrow_mut().kind {
                TyKind::Slice(ty)
                | TyKind::Array(ty, _) => *ty = ltys[0].clone(),
                | TyKind::Tup(tys) => {
                    std::mem::swap(tys, &mut ltys);
                },
                e => unimplemented!("LuaAstNode<P<Ty>>:set_tys() for {}", e.ast_name()),
            }

            Ok(())
        });

        methods.add_method("get_mut_ty", |_lua_ctx, this, ()| {
            match &this.borrow().kind {
                TyKind::Ptr(mut_ty) |
                TyKind::Rptr(_, mut_ty) => Ok(Some(LuaAstNode::new(mut_ty.clone()))),
                _ => Ok(None),
            }
        });

        methods.add_method("set_mut_ty", |lua_ctx, this, mt: Value| {
            match &mut this.borrow_mut().kind {
                TyKind::Ptr(mut_ty) |
                TyKind::Rptr(_, mut_ty) => {
                    *mut_ty = FromLuaExt::from_lua_ext(mt, lua_ctx)?;

                    Ok(())
                },
                _ => Ok(()),
            }
        });

        methods.add_method("get_path", |_lua_ctx, this, ()| {
            let path = match &this.borrow().kind {
                TyKind::Path(_, path) => path.clone(),
                _ => return Ok(None),
            };

            Ok(Some(LuaAstNode::new(path)))
        });

        methods.add_method("to_simple_path", |_lua_ctx, this, path: LuaString| {
            let path = Path::from_ident(Ident::from_str(path.to_str()?));

            this.borrow_mut().kind = TyKind::Path(None, path);

            Ok(())
        });

        methods.add_method("to_rptr", |lua_ctx, this, (lt, mut_ty): (Option<LuaString>, Value)| {
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

            let mut_ty = FromLuaExt::from_lua_ext(mut_ty, lua_ctx)?;
            this.borrow_mut().kind = TyKind::Rptr(lt, mut_ty);

            Ok(())
        });

        methods.add_method("wrap_in_slice", |_lua_ctx, this, ()| {
            let mut ty = this.borrow_mut();
            let mut placeholder = TyKind::Err;

            swap(&mut placeholder, &mut ty.kind);

            ty.kind = TyKind::Slice(P(Ty {
                id: DUMMY_NODE_ID,
                kind: placeholder,
                span: DUMMY_SP,
            }));

            Ok(())
        });

        methods.add_method("wrap_as_generic_angle_arg", |_lua_ctx, this, name: LuaString| {
            let mut ty = this.borrow_mut();
            let mut placeholder = TyKind::Err;

            swap(&mut placeholder, &mut ty.kind);

            let arg = GenericArg::Type(P(Ty {
                id: DUMMY_NODE_ID,
                kind: placeholder,
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

            ty.kind = TyKind::Path(None, path);

            Ok(())
        });

        methods.add_method("add_lifetime", |_lua_ctx, this, lifetime: LuaString| {
            if let TyKind::Path(_, path) = &mut this.borrow_mut().kind {
                let lt_str = lifetime.to_str()?;
                let mut lt_string = String::with_capacity(lt_str.len() + 1);

                lt_string.push('\'');
                lt_string.push_str(lt_str);

                let segments_len = path.segments.len();
                let mut segment = &mut path.segments[segments_len - 1];
                let arg = GenericArg::Lifetime(Lifetime {
                    id: DUMMY_NODE_ID,
                    ident: Ident::from_str(&lt_string),
                });

                if let Some(generic_args) = &mut segment.args {
                    if let GenericArgs::AngleBracketed(args) = &mut **generic_args {
                        args.args.push(arg);
                    }
                } else {
                    let args = GenericArgs::AngleBracketed(AngleBracketedArgs {
                        span: DUMMY_SP,
                        args: vec![arg],
                        constraints: Vec::new(),
                    });

                    segment.args = Some(P(args));
                }
            }

            Ok(())
        });

        methods.add_method("map_ptr_root", |lua_ctx, this, func: Function| {
            let ty = &mut *this.borrow_mut();

            fn apply_callback<'lua>(
                lua_ctx: Context<'lua>,
                ty: &mut P<Ty>,
                callback: Function<'lua>,
            ) -> Result<()> {
                match &mut ty.kind {
                    TyKind::Rptr(_, mut_ty)
                    | TyKind::Ptr(mut_ty) => return apply_callback(lua_ctx, &mut mut_ty.ty, callback),
                    _ => {
                        let ty_clone = ty.clone();
                        let new_ty = callback.call(LuaAstNode::new(ty_clone))?;

                        *ty = FromLuaExt::from_lua_ext(new_ty, lua_ctx)?;
                    },
                }

                Ok(())
            }

            apply_callback(lua_ctx, ty, func)
        });
    }
}

unsafe impl Send for LuaAstNode<Vec<Stmt>> {}
impl UserData for LuaAstNode<Vec<Stmt>> {}

impl FromLuaTable for Vec<Stmt> {
    fn from_lua_table<'lua>(table: LuaTable<'lua>, lua: Context<'lua>) -> Result<Self> {
        let v: Vec<Value> = FromLua::from_lua(Value::Table(table), lua)?;
        v.into_iter()
            .map(|v| FromLuaExt::from_lua_ext(v, lua))
            .collect::<Result<Vec<_>>>()
    }
}

impl AddMoreMethods for LuaAstNode<MutTy> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("set_ty", |lua_ctx, this, ty: Value| {
            let ty = FromLuaExt::from_lua_ext(ty, lua_ctx)?;
            this.borrow_mut().ty = ty;

            Ok(())
        });

        methods.add_method("set_mutable", |_lua_ctx, this, mutable: bool| {
            this.borrow_mut().mutbl = if mutable {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            };

            Ok(())
        });
    }
}

impl AddMoreMethods for LuaAstNode<Stmt> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_node", |lua_ctx, this, ()| {
            match this.borrow().kind.clone() {
                StmtKind::Expr(e) | StmtKind::Semi(e) => e.to_lua_ext(lua_ctx),
                StmtKind::Local(l) => l.to_lua_ext(lua_ctx),
                StmtKind::Item(i) => i.to_lua_ext(lua_ctx),
                StmtKind::Mac(_) => Err(Error::external(String::from("Mac stmts aren't implemented yet"))),
            }
        });

        methods.add_method("to_expr", |lua_ctx, this, (expr, is_semi): (Value, bool)| {
            let expr = FromLuaExt::from_lua_ext(expr, lua_ctx)?;

            this.borrow_mut().kind = if is_semi {
                StmtKind::Semi(expr)
            } else {
                StmtKind::Expr(expr)
            };

            Ok(())
        });
    }
}


impl AddMoreMethods for LuaAstNode<P<Pat>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_ident", |lua_ctx, this, ()| {
            if let PatKind::Ident(_, ident, _) = this.borrow().kind {
                ident.to_lua_ext(lua_ctx).map(Some)
            } else {
                Ok(None)
            }
        });
    }
}


impl AddMoreMethods for LuaAstNode<Crate> {}


impl AddMoreMethods for LuaAstNode<P<Local>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("set_ty", |lua_ctx, this, ty: Value| {
            let ty = FromLuaExt::from_lua_ext(ty, lua_ctx)?;
            this.borrow_mut().ty = ty;

            Ok(())
        });

        methods.add_method("set_init", |lua_ctx, this, init: Value| {
            let init = FromLuaExt::from_lua_ext(init, lua_ctx)?;
            this.borrow_mut().init = init;

            Ok(())
        });

        methods.add_method("get_pat", |_lua_ctx, this, ()| {
            Ok(LuaAstNode::new(this.borrow().pat.clone()))
        });

        methods.add_method("get_pat_id", |lua_ctx, this, ()| {
            Ok(this.borrow().pat.id.to_lua_ext(lua_ctx))
        });

        methods.add_method("get_attrs", |_lua_ctx, this, ()| {
            Ok(this.borrow()
                .attrs
                .iter()
                .map(|attr| LuaAstNode::new(attr.clone()))
                .collect::<Vec<_>>())
        });
    }
}


impl AddMoreMethods for LuaAstNode<Lit> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.borrow().kind.ast_name())
        });

        methods.add_method("get_value", |lua_ctx, this, ()| {
            match this.borrow().kind {
                LitKind::Str(s, _) => {
                    s.to_string().to_lua(lua_ctx)
                }
                LitKind::Int(i, _suffix) => i.to_lua_ext(lua_ctx),
                LitKind::Bool(b) => b.to_lua_ext(lua_ctx),
                LitKind::Char(c) => c.to_lua_ext(lua_ctx),
                ref node => {
                    Err(Error::external(format!(
                        "{:?} is not yet implemented",
                        node
                    )))
                }
            }
        });

        methods.add_method("replace_suffix", |_lua_ctx, this, lua_suffix: LuaString| {
            let mut lit = this.borrow_mut();

            match lit.kind {
                LitKind::Int(int, _) => {
                    let suffix = lua_suffix.to_str()?;
                    match suffix {
                        "" => {
                            lit.kind = LitKind::Int(int, LitIntType::Unsuffixed);
                            lit.token.suffix = None;
                            return Ok(());
                        }
                        "f32" | "f64" => {
                            let (float_ty, suffix_sym) = if suffix == "f32" {
                                (FloatTy::F32, sym::f32)
                            } else {
                                (FloatTy::F64, sym::f64)
                            };

                            let int_sym = Symbol::intern(&int.to_string());
                            lit.kind = LitKind::Float(int_sym, LitFloatType::Suffixed(float_ty));
                            lit.token = TokenLit {
                                kind: TokenLitKind::Float,
                                symbol: int_sym,
                                suffix: Some(suffix_sym),
                            };
                            return Ok(());
                        }
                        _ => {}
                    }

                    macro_rules! impl_suffix_int_match {
                        ($([$suffix:ident, $outer:path, $inner:path]),*) => {
                            match suffix {
                                $(stringify!($suffix) => {
                                    lit.kind = LitKind::Int(int, $outer($inner));
                                    lit.token.suffix = Some(sym::$suffix);
                                    return Ok(());
                                })*
                                _ => {}
                            }
                        }
                    }
                    impl_suffix_int_match!(
                        [usize, LitIntType::Unsigned, UintTy::Usize],
                        [u8,    LitIntType::Unsigned, UintTy::U8],
                        [u16,   LitIntType::Unsigned, UintTy::U16],
                        [u32,   LitIntType::Unsigned, UintTy::U32],
                        [u64,   LitIntType::Unsigned, UintTy::U64],
                        [u128,  LitIntType::Unsigned, UintTy::U128],
                        [isize, LitIntType::Signed,    IntTy::Isize],
                        [i8,    LitIntType::Signed,    IntTy::I8],
                        [i16,   LitIntType::Signed,    IntTy::I16],
                        [i32,   LitIntType::Signed,    IntTy::I32],
                        [i64,   LitIntType::Signed,    IntTy::I64],
                        [i128,  LitIntType::Signed,    IntTy::I128]
                    );

                    Err(Error::external(format!(
                        "{} literal suffix is not yet implemented",
                        suffix
                    )))
                }

                _ => Err(Error::external(format!(
                        "LuaAstNode<Lit>::replace_suffix() is not yet implemented for {}",
                        lit.ast_name())))
            }
        });

        methods.add_method("strip_suffix", |_lua_ctx, this, ()| {
            let mut lit = this.borrow_mut();

            if let LitKind::Int(_, ref mut suffix) = lit.kind {
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


impl AddMoreMethods for LuaAstNode<Mod> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("num_items", |_lua_ctx, this, ()| {
            Ok(this.borrow().items.len())
        });

        methods.add_method_mut("insert_item", |lua_ctx, this, (index, item): (usize, Value)| {
            let item = FromLuaExt::from_lua_ext(item, lua_ctx)?;
            this.borrow_mut().items.insert(index, item);
            Ok(())
        });

        methods.add_method_mut("drain_items", |lua_ctx, this, ()| {
            this.borrow_mut()
                .items
                .drain(..)
                .map(|item| item.to_lua_ext(lua_ctx))
                .collect::<Result<Vec<_>>>()
        });
    }
}


impl AddMoreMethods for LuaAstNode<P<UseTree>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_rename", |_lua_ctx, this, ()| {
            match this.borrow().kind {
                UseTreeKind::Simple(Some(rename), _, _) => Ok(Some(rename.to_string())),
                _ => Ok(None),
            }
        });

        methods.add_method("get_nested", |lua_ctx, this, ()| {
            match &this.borrow().kind {
                UseTreeKind::Nested(trees) => Ok(Some(
                    trees.clone()
                        .into_iter()
                        .map(|(tree, id)| Ok(vec![P(tree).to_lua_ext(lua_ctx)?, id.to_lua_ext(lua_ctx)?]))
                        .collect::<Result<Vec<_>>>()?
                )),
                _ => Ok(None),
            }
        });
    }
}

impl ToLuaExt for AstNode {
    fn to_lua_ext<'lua>(self, lua: Context<'lua>) -> Result<Value<'lua>> {
        match self {
            AstNode::Crate(x) => x.to_lua_ext(lua),
            AstNode::Expr(x) => x.to_lua_ext(lua),
            AstNode::Pat(x) => x.to_lua_ext(lua),
            AstNode::Ty(x) => x.to_lua_ext(lua),
            AstNode::Stmts(x) => x.to_lua_ext(lua),
            AstNode::Stmt(x) => x.to_lua_ext(lua),
            AstNode::Item(x) => x.to_lua_ext(lua),
        }
    }
}

impl UserData for LuaAstNode<FnLike> {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_kind", |_lua_ctx, this, ()| {
            Ok(this.borrow().kind.ast_name())
        });

        methods.add_method("get_id", |lua_ctx, this, ()| {
            this.borrow().id.to_lua_ext(lua_ctx)
        });

        methods.add_method("get_ident", |lua_ctx, this, ()| {
            this.borrow().ident.to_lua_ext(lua_ctx)
        });

        methods.add_method("has_block", |lua_ctx, this, ()| {
            this.borrow().block.is_some().to_lua_ext(lua_ctx)
        });
    }
}

impl ToLuaExt for FnKind {
    fn to_lua_ext<'lua>(self, ctx: Context<'lua>) -> Result<Value<'lua>> {
        match self {
            FnKind::Normal => "Normal",
            FnKind::ImplMethod => "ImplMethod",
            FnKind::TraitMethod => "TraitMethod",
            FnKind::Foreign => "Foreign",
        }.to_lua(ctx)
    }
}

impl AddMoreMethods for LuaAstNode<P<FnDecl>> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_args", |_lua_ctx, this, ()| {
            this.borrow().inputs.clone().to_lua_ext(_lua_ctx)
        });
    }
}

impl AddMoreMethods for LuaAstNode<Param> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_pat_id", |lua_ctx, this, ()| {
            Ok(this.borrow().pat.id.to_lua_ext(lua_ctx))
        });

        methods.add_method("set_binding", |_lua_ctx, this, binding_str: LuaString| {
            if let PatKind::Ident(binding, ..) = &mut this.borrow_mut().pat.kind {
                *binding = match binding_str.to_str()? {
                    "ByRefMut" => BindingMode::ByRef(Mutability::Mutable),
                    "ByRefImmut" => BindingMode::ByRef(Mutability::Immutable),
                    "ByValMut" => BindingMode::ByValue(Mutability::Mutable),
                    "ByValImmut" => BindingMode::ByValue(Mutability::Immutable),
                    _ => panic!("Unknown binding kind"),
                };
            }

            Ok(())
        });

        //methods.add_method("get_attrs", |_lua_ctx, this, ()| {
        //    Ok(this
        //       .borrow()
        //       .attrs
        //       .iter()
        //       .map(|attr| LuaAstNode::new(attr.clone()))
        //       .collect::<Vec<_>>())
        //});
    }
}

impl AddMoreMethods for LuaAstNode<FnHeader> {}

impl AddMoreMethods for LuaAstNode<StructField> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("get_attrs", |_lua_ctx, this, ()| {
            Ok(this.borrow()
                .attrs
                .iter()
                .map(|attr| LuaAstNode::new(attr.clone()))
                .collect::<Vec<_>>())
        });
    }
}

impl AddMoreMethods for LuaAstNode<ItemKind> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("add_lifetime", |_lua_ctx, this, string: LuaString| {
            let lt_str = string.to_str()?;

            add_item_lifetime(&mut this.borrow_mut(), lt_str);

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

        methods.add_method("get_arg_ids", |_lua_ctx, this, ()| {
            if let ItemKind::Fn(sig, ..) = &*this.borrow() {
                return Ok(Some(sig.decl
                    .inputs
                    .iter()
                    .map(|a| a.id.as_u32())
                    .collect::<Vec<_>>()
                ));
            }

            Ok(None)
        });
    }
}

impl AddMoreMethods for LuaAstNode<Attribute> {
    fn add_more_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("ident", |lua_ctx, this, ()| {
            if let Some(ident) = this.borrow().ident() {
                Ok(Some(ident.to_lua_ext(lua_ctx)?))
            } else {
                Ok(None)
            }
        });
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct LuaHirId(pub HirId);

impl UserData for LuaHirId {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Eq, |_lua_ctx, this, rhs: LuaHirId| {
            Ok(*this == rhs)
        });

        methods.add_meta_method(MetaMethod::ToString, |_lua_ctx, this, ()| {
            Ok(format!("HirId {{ owner: {}, local_id: {} }}", this.0.owner.as_u32(), this.0.local_id.as_u32()))
        });
    }
}

fn add_item_lifetime(item_kind: &mut ItemKind, lt_name: &str) {
    let mut lt_string = String::with_capacity(lt_name.len() + 1);

    lt_string.push('\'');
    lt_string.push_str(lt_name);

    let generic_param = GenericParam {
        id: DUMMY_NODE_ID,
        ident: Ident::from_str(&lt_string),
        attrs: Default::default(),
        bounds: Vec::new(),
        kind: GenericParamKind::Lifetime,
        is_placeholder: false,
    };

    if let ItemKind::Struct(_, generics)
            | ItemKind::Fn(_, generics, _) = item_kind {
        let diff = |p: &GenericParam| {
            if let GenericParamKind::Lifetime = p.kind {
                p.ident == generic_param.ident
            } else {
                false
            }
        };

        if generics.params.iter().any(diff) {
            return;
        }

        generics.params.push(generic_param);
    }
}
