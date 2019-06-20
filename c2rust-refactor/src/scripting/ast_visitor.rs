use smallvec::SmallVec;
use syntax::ast::*;
use syntax::mut_visit::{self, *};
use syntax::ptr::P;

use rlua::{FromLuaMulti};
use rlua::prelude::{LuaContext, LuaFunction, LuaResult, LuaTable, LuaUserData};

use crate::ast_manip::{WalkAst};
use super::TransformCtxt;
use super::to_lua_ast_node::{LuaAstNode};

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


pub(crate) struct LuaAstVisitorNew<'lua, 'a, 'tcx> {
    visitor: LuaTable<'lua>,
    lua_ctx: LuaContext<'lua>,
    _ctx: TransformCtxt<'a, 'tcx>,
}

impl<'lua, 'a, 'tcx> LuaAstVisitorNew<'lua, 'a, 'tcx> {
    pub fn new(ctx: TransformCtxt<'a, 'tcx>, lua_ctx: LuaContext<'lua>, visitor: LuaTable<'lua>) -> Self {
        LuaAstVisitorNew { _ctx: ctx, lua_ctx, visitor }
    }

    fn call_visit<T, R>(&mut self, method: LuaFunction<'lua>, param: T) -> R
        where T: WalkAst,
              LuaAstNode<T>: 'static + LuaUserData + Clone,
              R: FromLuaMulti<'lua>,
    {
        self.lua_ctx.scope(|scope| {
            let visitor = self.visitor.clone();
            let param = scope.create_static_userdata(LuaAstNode::new(param)).unwrap();
            let walk = scope.create_function_mut(|_lua_ctx, x: LuaAstNode<T>| {
                x.walk(self);
                Ok(())
            });
            method.call((visitor, (param, walk)))
                .unwrap_or_else(|e| panic!("Lua visit function failed: {:?}", e))
        })
    }
}

impl<'lua, 'a, 'tcx> MutVisitor for LuaAstVisitorNew<'lua, 'a, 'tcx> {
    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let visit_method: Option<LuaFunction> = self.visitor.get("flat_map_item")
            .expect("Could not get lua visitor function");

        if let Some(method) = visit_method {
            let new_items: Vec<LuaAstNode<P<Item>>> = self.call_visit(method, i);
            let res: SmallVec<[P<Item>; 1]> = new_items.into_iter().map(|i| i.into_inner()).collect();
            res
        } else {
            mut_visit::noop_flat_map_item(i, self)
        }
    }

    fn visit_mac(&mut self, mac: &mut Mac) {
        mut_visit::noop_visit_mac(mac, self);
    }
}
