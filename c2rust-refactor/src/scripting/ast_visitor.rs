use rlua::prelude::{LuaFunction, LuaResult, LuaTable};

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
                let boxed: LuaTable = expr.get("expr")?;

                self.visit_expr(boxed)?;
            },
            "AssignOp"
            | "Binary"
            | "Assign" => {
                let lhs: LuaTable = expr.get("lhs")?;
                let rhs: LuaTable = expr.get("rhs")?;

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
                let expr: LuaTable = expr.get("expr")?;

                self.visit_expr(expr)?;
            },
            "MethodCall" => {
                let params: LuaTable = expr.get("params")?;

                for param in params.sequence_values::<LuaTable>() {
                    self.visit_expr(param?)?;
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
                let expr: LuaTable = stmt.get("expr")?;

                self.visit_expr(expr)?;
            },
            "Local" => {
                self.visit_local(stmt)?;
            },
            "Item" => {
                let item: LuaTable = stmt.get("item")?;

                self.visit_item(item)?;
            },
            ref e => warn!("visit_stmt: Unsupported Stmt kind: {}", e),
        }

        Ok(())
    }

    pub fn visit_local(&self, local: LuaTable<'lua>) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,visit_local(local));

        let opt_init: Option<LuaTable> = local.get("init")?;

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

        let block: LuaTable = fn_like.get("block")?;

        self.visit_block(block)?;

        Ok(())
    }

    pub fn finish(&self) -> LuaResult<()> {
        call_lua_visitor_method!(self.visitor,finish());

        Ok(())
    }
}
