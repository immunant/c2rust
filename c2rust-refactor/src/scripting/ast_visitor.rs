use rlua::prelude::{LuaContext, LuaFunction, LuaResult, LuaTable};
use syntax::ast::{
    Arg, BindingMode, Block, Crate, Expr, ExprKind, FnDecl, ImplItem, ImplItemKind,
    Item, ItemKind, Local, Mod, Mutability::*, NodeId, Pat, PatKind,
    Stmt, StmtKind,
};
use syntax::source_map::symbol::Symbol;
use syntax::ptr::P;

use crate::ast_manip::fn_edit::{FnKind, FnLike};
use crate::scripting::{TransformCtxt, into_lua_ast::IntoLuaAst};

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

pub(crate) struct LuaAstVisitor<'a, 'lua, 'tctx: 'a> {
    ctx: &'a TransformCtxt<'a, 'tctx>,
    lua_ctx: LuaContext<'lua>,
    visitor: LuaTable<'lua>
}

impl<'a, 'lua, 'tctx> LuaAstVisitor<'a, 'lua, 'tctx> {
    pub fn new(visitor: LuaTable<'lua>, ctx: &'a TransformCtxt<'a, 'tctx>, lua_ctx: LuaContext<'lua>) -> Self {
        LuaAstVisitor {
            ctx,
            lua_ctx,
            visitor,
        }
    }

    pub fn visit_crate(&self, krate: &mut Crate) -> LuaResult<()> {
        let lua_krate = krate.clone().into_lua_ast(self.ctx, self.lua_ctx)?;

        call_lua_visitor_method!(self.visitor,visit_crate(lua_krate));

        self.visit_mod(lua_krate.get("module")?)?;
        self.finish()?;

        krate.merge_lua_ast(lua_krate)?;

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
            ref e => {
                warn!("visit_item: Found unsupported item kind: {:?}", e);
            },
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

pub(crate) trait MergeLuaAst {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()>;
}

impl MergeLuaAst for &mut FnLike {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.kind = match table.get::<_, String>("kind")?.as_str() {
            "Normal" => FnKind::Normal,
            "ImplMethod" => FnKind::ImplMethod,
            "TraitMethod" => FnKind::TraitMethod,
            "Foreign" => FnKind::Foreign,
            _ => self.kind,
        };
        self.id = NodeId::from_u32(table.get("id")?);
        self.ident.name = Symbol::intern(&table.get::<_, String>("ident")?);
        self.decl.merge_lua_ast(table.get("decl")?)?;

        // REVIEW: How do we deal with spans if there is no existing block
        // to modify?
        if let Some(ref mut block) = self.block {
            block.merge_lua_ast(table.get("block")?)?;
        }

        // TODO: Attrs

        Ok(())
    }
}

impl MergeLuaAst for &mut P<Block> {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_stmts: LuaTable = table.get("stmts")?;

        // TODO: This may need to be improved if we want to delete or add
        // stmts as it currently expects stmts to be 1-1
        self.stmts.iter_mut().enumerate().map(|(i, stmt)| {
            let stmt_table: LuaTable = lua_stmts.get(i + 1)?;

            stmt.merge_lua_ast(stmt_table)
        }).collect()
    }
}

impl MergeLuaAst for &mut Stmt {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        // REVIEW: How do we deal with modifying to a different type of stmt than
        // the existing one?

        match self.node {
            StmtKind::Local(ref mut l) => l.merge_lua_ast(table)?,
            StmtKind::Item(ref mut i) => i.merge_lua_ast(table.get("item")?)?,
            StmtKind::Expr(ref mut e) |
            StmtKind::Semi(ref mut e) => e.merge_lua_ast(table.get("expr")?)?,
            _ => warn!("MergeLuaAst unimplemented for Macro StmtKind"),
        };

        Ok(())
    }
}

impl MergeLuaAst for &mut P<FnDecl> {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_args: LuaTable = table.get("args")?;

        // TODO: This may need to be improved if we want to delete or add
        // arguments as it currently expects args to be 1-1
        self.inputs.iter_mut().enumerate().map(|(i, arg)| {
            let arg_table: LuaTable = lua_args.get(i + 1)?;

            arg.merge_lua_ast(arg_table)
        }).collect()
    }
}

impl MergeLuaAst for &mut Arg {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.id = NodeId::from_u32(table.get("id")?);
        self.pat.merge_lua_ast(table.get("pat")?)?;

        Ok(())
    }
}

impl MergeLuaAst for &mut P<Pat> {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        // REVIEW: How to allow the type to be changed?
        match self.node {
            PatKind::Ident(ref mut binding, ref mut ident, _) => {
                *binding = match table.get::<_, String>("binding")?.as_str() {
                    "ByRefImmutable" => BindingMode::ByRef(Immutable),
                    "ByRefMutable" => BindingMode::ByRef(Mutable),
                    "ByValueImmutable" => BindingMode::ByValue(Immutable),
                    "ByValueMutable" => BindingMode::ByValue(Mutable),
                    _ => *binding,
                };
                ident.name = Symbol::intern(&table.get::<_, String>("ident")?);
            },
            ref e => warn!("Found {:?}", e),
        }

        Ok(())
    }
}

impl MergeLuaAst for &mut Local {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        // TODO: ty
        let pat: LuaTable = table.get("pat")?;
        let opt_init: Option<LuaTable> = table.get("init")?;

        self.pat.merge_lua_ast(pat)?;

        match &mut self.init {
            Some(existing_init) => {
                match opt_init {
                    Some(init) => existing_init.merge_lua_ast(init)?,
                    None => self.init = None,
                }
            },
            None => {
                if let Some(_) = opt_init {
                    unimplemented!("MergeLuaAst unimplemented local init update");
                }
            },
        }

        Ok(())
    }
}

impl MergeLuaAst for &mut Crate {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.module.merge_lua_ast(table.get("module")?)?;

        Ok(())
    }
}

impl MergeLuaAst for &mut Mod {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_items: LuaTable = table.get("items")?;

        self.inline = table.get("inline")?;

        // TODO: This may need to be improved if we want to delete or add
        // items as it currently expects items to be 1-1
        self.items.iter_mut().enumerate().map(|(i, item)| {
            let item_table: LuaTable = lua_items.get(i + 1)?;

            item.merge_lua_ast(item_table)
        }).collect()
    }
}

impl MergeLuaAst for &mut P<Item> {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.ident.name = Symbol::intern(&table.get::<_, String>("ident")?);

        // REVIEW: How to allow the type to be changed?
        match &mut self.node {
            ItemKind::Fn(fn_decl, _, _, block) => {
                let lua_fn_decl: LuaTable = table.get("decl")?;
                let lua_block: LuaTable = table.get("block")?;

                block.merge_lua_ast(lua_block)?;
                fn_decl.merge_lua_ast(lua_fn_decl)?;
            },
            ItemKind::Impl(.., items) => {
                let lua_items: LuaTable = table.get("items")?;

                // TODO: This may need to be improved if we want to delete or add
                // values as it currently expects values to be 1-1
                let res: LuaResult<Vec<()>> = items.iter_mut().enumerate().map(|(i, item)| {
                    let item_table: LuaTable = lua_items.get(i + 1)?;

                    item.merge_lua_ast(item_table)
                }).collect();

                res?;
            },
            ref e => warn!("MergeLuaAst unimplemented: {:?}", e),
        }

        Ok(())
    }
}

impl MergeLuaAst for &mut P<Expr> {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        match self.node {
            ExprKind::Binary(_, ref mut lhs, ref mut rhs)
            | ExprKind::Assign(ref mut lhs, ref mut rhs)
            | ExprKind::AssignOp(_, ref mut lhs, ref mut rhs) => {
                let lua_lhs: LuaTable = table.get("lhs")?;
                let lua_rhs: LuaTable = table.get("rhs")?;

                lhs.merge_lua_ast(lua_lhs)?;
                rhs.merge_lua_ast(lua_rhs)?;
            },
            ExprKind::Array(ref mut exprs) => {
                let lua_exprs: LuaTable = table.get("values")?;

                // TODO: This may need to be improved if we want to delete or add
                // values as it currently expects values to be 1-1
                let res: LuaResult<Vec<()>> = exprs.iter_mut().enumerate().map(|(i, expr)| {
                    let expr_table: LuaTable = lua_exprs.get(i + 1)?;

                    expr.merge_lua_ast(expr_table)
                }).collect();

                res?;
            },
            ref e => warn!("MergeLuaAst unimplemented: {:?}", e),
        }

        Ok(())
    }
}

impl MergeLuaAst for &mut ImplItem {
    fn merge_lua_ast<'lua>(self, table: LuaTable<'lua>) -> LuaResult<()> {
        self.ident.name = Symbol::intern(&table.get::<_, String>("ident")?);

        match &mut self.node {
            ImplItemKind::Method(sig, block) => {
                let lua_decl: LuaTable = table.get("decl")?;
                let lua_block: LuaTable = table.get("block")?;

                sig.decl.merge_lua_ast(lua_decl)?;
                block.merge_lua_ast(lua_block)?;

                // TODO: generics, attrs, ..
            },
            ref e => unimplemented!("MergeLuaAst for {:?}", e),
        }

        Ok(())
    }
}
