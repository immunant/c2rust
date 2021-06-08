use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, *};
use syntax::ptr::P;

use rlua::prelude::{LuaContext, LuaFunction, LuaResult, LuaTable, LuaUserData, LuaValue};

use crate::ast_manip::{WalkAst};
use super::DisplayLuaError;
use super::to_lua_ast_node::{FromLuaAstNode, FromLuaExt, LuaAstNode, ToLuaScoped};

macro_rules! call_lua_visitor_method {
    ($obj: expr , $method: ident ($($params: expr),*)) => {
        let opt_visit_method: Option<LuaFunction> = $obj.get(stringify!($method))?;

        if let Some(visit_method) = opt_visit_method {
            let proceed = visit_method.call::<_, bool>(($obj.clone(), $($params.clone()),*))?;

            if !proceed {
                return Ok(());
            }
        }
    };
}

pub(crate) struct LuaAstVisitor<'lua> {
    visitor: LuaTable<'lua>
}

impl<'lua> LuaAstVisitor<'lua> {
    pub fn new(visitor: LuaTable<'lua>) -> Self {
        LuaAstVisitor {
            visitor,
        }
    }

    pub fn visit_crate(&self, lua_crate: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_crate(lua_crate));

        self.visit_mod(lua_crate.get("module")?)?;

        Ok(())
    }

    pub fn visit_mod(&self, module: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_mod(module));

        let items: LuaTable = module.get("items")?;

        for item in items.sequence_values::<LuaTable>() {
            self.visit_item(item?)?;
        }

        Ok(())
    }

    pub fn visit_impl(&self, imp: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_impl(imp));

        let items: LuaTable = imp.get("items")?;

        for item in items.sequence_values::<LuaTable>() {
            let item = item?;
            let kind: String = item.get("kind")?;

            match kind.as_str() {
                "ImplMethod" => { self.visit_fn_like(item)?; },
                ref e => unimplemented!("visit_impl: Impl kind: {:?}", e),
            }
        }

        Ok(())
    }

    pub fn visit_item(&self, item: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_item(item));

        match item.get::<_, String>("kind")?.as_str() {
            "Fn" => { self.visit_fn_like(item)?; },
            "Impl" => { self.visit_impl(item)?; },
            ref e =>
                warn!("visit_item: Found unsupported item kind: {:?}", e),
        }

        Ok(())
    }

    pub fn visit_expr(&self, expr: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_expr(expr));

        match expr.get::<_, String>("kind")?.as_str() {
            "Box" => {
                let boxed = expr.get("expr")?;

                self.visit_expr(boxed)?;
            },
            "AssignOp"
            | "Binary"
            | "Assign" => {
                let lhs = expr.get("lhs")?;
                let rhs = expr.get("rhs")?;

                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
            },
            "Array" => {
                let values: LuaTable = expr.get("values")?;

                for val in values.sequence_values::<LuaTable>() {
                    self.visit_expr(val?)?;
                }
            },
            "Path" => {
                // TODO
            },
            "Lit" => {
                // TODO: self.visit_literal(lit)?;
            },
            "InlineAsm" => {
                let inputs: LuaTable = expr.get("inputs")?;
                let outputs: LuaTable = expr.get("outputs")?;

                for input in inputs.sequence_values::<LuaTable>() {
                    let input = input?;
                    let expr = input.get("expr")?;

                    self.visit_expr(expr)?;
                }

                for output in outputs.sequence_values::<LuaTable>() {
                    let output = output?;
                    let expr = output.get("expr")?;

                    self.visit_expr(expr)?;
                }
            },
            "Unary" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?;
            },
            "Call" => {
                let path = expr.get("path")?;
                let args: LuaTable = expr.get("args")?;

                self.visit_expr(path)?;

                for arg in args.sequence_values::<LuaTable>() {
                    self.visit_expr(arg?)?;
                }
            },
            "MethodCall" => {
                let args: LuaTable = expr.get("args")?;

                for arg in args.sequence_values::<LuaTable>() {
                    self.visit_expr(arg?)?;
                }
            },
            "Index" => {
                let indexed = expr.get("indexed")?;
                let index = expr.get("index")?;

                self.visit_expr(indexed)?;
                self.visit_expr(index)?;
            },
            "AddrOf" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?;
            },
            "Try" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?;
            },
            "Match" => {
                let match_expr = expr.get("expr")?;
                let arms: LuaTable = expr.get("arms")?;

                for arm in arms.sequence_values::<LuaTable>() {
                    let arm = arm?;
                    let body = arm.get("body")?;
                    let opt_guard = arm.get("guard")?;

                    self.visit_expr(body)?;

                    if let Some(guard) = opt_guard {
                        self.visit_expr(guard)?;
                    }
                }

                self.visit_expr(match_expr)?;
            },
            "Cast" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?;
            },
            "If" => {
                let cond = expr.get("cond")?;
                let then = expr.get("then")?;
                let opt_else = expr.get("else")?;

                self.visit_expr(cond)?;
                self.visit_block(then)?;

                if let Some(els) = opt_else {
                    self.visit_expr(els)?;
                }
            },
            "Block" => {
                let block = expr.get("block")?;

                self.visit_block(block)?
            },
            "Tup" => {
                let exprs: LuaTable = expr.get("exprs")?;

                for expr in exprs.sequence_values::<LuaTable>() {
                    self.visit_expr(expr?)?;
                }
            },
            "Paren" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?
            },
            "Field" => {
                let expr = expr.get("expr")?;

                self.visit_expr(expr)?
            },
            "Loop" => {
                let block = expr.get("block")?;

                self.visit_block(block)?
            },
            "While" => {
                let block = expr.get("block")?;
                let cond = expr.get("cond")?;

                self.visit_expr(cond)?;
                self.visit_block(block)?
            },
            "Ret" => {
                let opt_val = expr.get("value")?;

                if let Some(value) = opt_val {
                    self.visit_expr(value)?;
                }
            },
            ref e => warn!("visit_expr: Found unsupported expr {}", e),
        }

        Ok(())
    }

    pub fn visit_stmt(&self, stmt: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_stmt(stmt));

        match stmt.get::<_, String>("kind")?.as_str() {
            "Expr"
            | "Semi" => {
                let expr = stmt.get("expr")?;

                self.visit_expr(expr)?;
            },
            "Local" => {
                self.visit_local(stmt)?;
            },
            "Item" => {
                let item = stmt.get("item")?;

                self.visit_item(item)?;
            },
            ref e => warn!("visit_stmt: Unsupported Stmt kind: {}", e),
        }

        Ok(())
    }

    pub fn visit_local(&self, local: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_local(local));

        let opt_init = local.get("init")?;

        if let Some(init) = opt_init {
            self.visit_expr(init)?;
        }

        Ok(())
    }

    pub fn visit_block(&self, block: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_block(block));

        let stmts: LuaTable = block.get("stmts")?;

        for stmt in stmts.sequence_values::<LuaTable>() {
            self.visit_stmt(stmt?)?;
        }

        Ok(())
    }

    pub fn visit_fn_like(&self, fn_like: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_fn_like(fn_like));

        let opt_block = fn_like.get("block")?;

        if let Some(block) = opt_block {
            self.visit_block(block)?;
        }

        Ok(())
    }

    pub fn finish(&self) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,finish());

        Ok(())
    }
}


pub(crate) struct LuaAstVisitorNew<'lua> {
    visitor: LuaTable<'lua>,
    lua_ctx: LuaContext<'lua>,
}

impl<'lua> LuaAstVisitorNew<'lua> {
    pub fn new(lua_ctx: LuaContext<'lua>, visitor: LuaTable<'lua>) -> Self {
        LuaAstVisitorNew { lua_ctx, visitor }
    }

    fn call_visit<T>(&mut self, method: LuaFunction<'lua>, param: &mut T)
        where T: WalkAst + Clone,
              LuaAstNode<T>: 'static + LuaUserData + Clone + FromLuaAstNode,
    {
        let node = LuaAstNode::new(param.clone());
        self.lua_ctx.scope(|scope| {
            let visitor = self.visitor.clone();
            let param = scope.create_static_userdata(node.clone()).unwrap();
            let walk = scope.create_function_mut(|lua_ctx, x: LuaValue| {
                <LuaAstNode<T> as FromLuaAstNode>::from_lua_ast_node(x, lua_ctx)?
                    .walk(self);
                Ok(())
            });
            method.call((visitor, (param, walk)))
                .unwrap_or_else(|e| panic!("Lua visit function failed: {:}", DisplayLuaError(e)))
        });
        *param = node.into_inner();
    }

    fn call_flat_map<T>(&mut self, method: LuaFunction<'lua>, param: T) -> Vec<T>
        where T: WalkAst + ToLuaScoped + FromLuaExt,
              LuaAstNode<T>: LuaUserData + FromLuaAstNode,
    {
        let lua_ctx = self.lua_ctx;
        lua_ctx.scope(|scope| {
            let visitor = self.visitor.clone();
            let param = ToLuaScoped::to_lua_scoped(param, self.lua_ctx, scope).unwrap();
            let walk = scope.create_function_mut(|lua_ctx, x: LuaValue| {
                <LuaAstNode<T> as FromLuaAstNode>::from_lua_ast_node(x, lua_ctx)?
                    .walk(self);
                Ok(())
            });
            method.call((visitor, (param, walk)))
                .and_then(|res| FromLuaExt::from_lua_ext(res, lua_ctx))
                .unwrap_or_else(|e| panic!("Lua visit function failed: {:}", DisplayLuaError(e)))
        })
    }
}

macro_rules! impl_visitors {
    {visit: $visitor:ident, $ty:ty, $noop_visitor:ident} => {
        fn $visitor(&mut self, node: &mut $ty) {
            let visit_method: Option<LuaFunction> = self.visitor.get(stringify!($visitor))
                .expect("Could not get lua visitor function");
            if let Some(method) = visit_method {
                self.call_visit(method, node);
            } else {
                mut_visit::$noop_visitor(node, self)
            }
        }
    };
    {flat_map: $visitor:ident, $ty:ty, $noop_visitor:ident} => {
        fn $visitor(&mut self, node: $ty) -> SmallVec<[$ty; 1]> {
            let visit_method: Option<LuaFunction> = self.visitor.get(stringify!($visitor))
                .expect("Could not get lua visitor function");
            if let Some(method) = visit_method {
                let new_items = self.call_flat_map(method, node);
                SmallVec::from_vec(new_items)
            } else {
                mut_visit::$noop_visitor(node, self)
            }
        }
    };
    {$([$kind:ident: $visitor:ident, $ty:ty, $noop_visitor:ident]),*} => {
        $(impl_visitors!{$kind: $visitor, $ty, $noop_visitor})*
    }
}

impl<'lua> MutVisitor for LuaAstVisitorNew<'lua> {
    impl_visitors!{
        [visit: visit_mod, Mod, noop_visit_mod],
        [visit: visit_expr, P<Expr>, noop_visit_expr],
        [visit: visit_fn_header, FnHeader, noop_visit_fn_header],
        [visit: visit_fn_decl, P<FnDecl>, noop_visit_fn_decl],
        [visit: visit_item_kind, ItemKind, noop_visit_item_kind],
        [visit: visit_ty, P<Ty>, noop_visit_ty],
        [visit: visit_ident, Ident, noop_visit_ident],
        [visit: visit_local, P<Local>, noop_visit_local],
        [flat_map: flat_map_param, Param, noop_flat_map_param],
        [flat_map: flat_map_item, P<Item>, noop_flat_map_item],
        [flat_map: flat_map_foreign_item, ForeignItem, noop_flat_map_foreign_item],
        [flat_map: flat_map_stmt, Stmt, noop_flat_map_stmt],
        [flat_map: flat_map_struct_field, StructField, noop_flat_map_struct_field]
    }

    fn visit_mac(&mut self, mac: &mut Mac) {
        mut_visit::noop_visit_mac(mac, self);
    }
}
