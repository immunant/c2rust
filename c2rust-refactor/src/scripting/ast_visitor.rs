use rlua::prelude::{LuaContext, LuaError, LuaFunction, LuaResult, LuaTable};
use syntax::ast::{
    Arg, BindingMode, Block, Crate, FunctionRetTy, FnDecl, Item, ItemKind, Local,
    Mod, Mutability::*, NodeId, Pat, PatKind, Stmt, StmtKind,
};
use syntax::source_map::symbol::Symbol;
use syntax::ptr::P;

use crate::ast_manip::fn_edit::{FnKind, FnLike};
use crate::scripting::{IntoLuaAst, TransformCtxt, utils::iter_to_lua_array};

pub(crate) struct LuaAstVisitor<'a, 'lua, 'tctx: 'a> {
    ctx: &'a TransformCtxt<'a, 'tctx>,
    lua_ctx: LuaContext<'lua>,
    visitor_obj: LuaTable<'lua>
}

impl<'a, 'lua, 'tctx> LuaAstVisitor<'a, 'lua, 'tctx> {
    pub fn new(visitor_obj: LuaTable<'lua>, ctx: &'a TransformCtxt<'a, 'tctx>, lua_ctx: LuaContext<'lua>) -> Self {
        LuaAstVisitor {
            ctx,
            lua_ctx,
            visitor_obj,
        }
    }

    pub fn visit_crate(&self, krate: &mut Crate) -> LuaResult<bool> {
        let visit_crate: Option<LuaFunction> = self.visitor_obj.get("visit_crate")?;

        let lua_krate = krate.clone().into_lua_ast(self.ctx, self.lua_ctx)?;

        if let Some(visit_method) = visit_crate {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), lua_krate.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        self.visit_module(lua_krate.get("module")?)?;
        self.finish()?;

        krate.merge_lua_ast(lua_krate)?;

        Ok(true)
    }

    pub fn visit_module(&self, module: LuaTable<'lua>) -> LuaResult<bool> {
        let visit_module: Option<LuaFunction> = self.visitor_obj.get("visit_module")?;

        if let Some(visit_method) = visit_module {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), module.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        let items: LuaTable = module.get("items")?;

        for item in items.sequence_values::<LuaTable>() {
            self.visit_item(item?)?;
        }

        Ok(true)
    }

    pub fn visit_item(&self, item: LuaTable<'lua>) -> LuaResult<bool> {
        let visit_item: Option<LuaFunction> = self.visitor_obj.get("visit_item")?;

        if let Some(visit_method) = visit_item {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), item.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        match item.get::<_, String>("kind")?.as_str() {
            "Fn" => {
                let block: LuaTable = item.get("block")?;

                self.visit_block(block)?;
            },
            ref e => {
                warn!("visit_item: Found unsupported item kind: {:?}", e);
            },
        }

        Ok(true)
    }

    pub fn visit_expr(&self, expr: LuaTable<'lua>) -> LuaResult<bool> {
        let visit_expr: Option<LuaFunction> = self.visitor_obj.get("visit_expr")?;

        if let Some(visit_method) = visit_expr {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), expr.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        Ok(true)
    }

    pub fn visit_stmt(&self, stmt: LuaTable<'lua>) -> LuaResult<bool> {
       let visit_stmt: Option<LuaFunction> = self.visitor_obj.get("visit_stmt")?;

        if let Some(visit_method) = visit_stmt {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), stmt.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        match stmt.get::<_, String>("kind")?.as_str() {
            "Expr"
            | "Semi" => {
                let expr: LuaTable = stmt.get("expr")?;

                self.visit_expr(expr)?;
            },
            ref e => warn!("visit_stmt: Unsupported Stmt kind: {}", e),
        }

        Ok(true)
    }

    pub fn visit_block(&self, block: LuaTable<'lua>) -> LuaResult<bool> {
       let visit_block: Option<LuaFunction> = self.visitor_obj.get("visit_block")?;

        if let Some(visit_method) = visit_block {
            let proceed = visit_method.call::<_, bool>((self.visitor_obj.clone(), block.clone()))?;

            if !proceed {
                return Ok(true);
            }
        }

        let stmts: LuaTable = block.get("stmts")?;

        for stmt in stmts.sequence_values::<LuaTable>() {
            self.visit_stmt(stmt?)?;
        }

        Ok(true)
    }

    pub fn finish(&self) -> LuaResult<()> {
        let visit_crate: Option<LuaFunction> = self.visitor_obj.get("finish")?;

        if let Some(visit_method) = visit_crate {
            visit_method.call::<_, ()>(self.visitor_obj.clone())?;
        }

        Ok(())
    }
}

impl<'lua> IntoLuaAst<'lua> for FnLike {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnLike")?;
        ast.set("kind", match self.kind {
            FnKind::Normal => "Normal",
            FnKind::ImplMethod => "ImplMethod",
            FnKind::TraitMethod => "TraitMethod",
            FnKind::Foreign => "Foreign",
        })?;
        ast.set("id", self.id.as_u32())?;
        ast.set("ident", self.ident.as_str().get())?;
        ast.set("decl", self.decl.into_lua_ast(ctx, lua_ctx)?)?;

        let block = self.block
            .map(|b| b.into_lua_ast(ctx, lua_ctx))
            .transpose()?;

        ast.set("block", block)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<FnDecl> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnDecl")?;
        ast.set("c_variadic", self.c_variadic)?;

        self.and_then(|fn_decl| {
            ast.set("return_type", match fn_decl.output {
                FunctionRetTy::Default(_) => None,
                FunctionRetTy::Ty(ty) => Some(ctx.intern(ty)),
            })?;

            let args: LuaResult<Vec<_>> = fn_decl.inputs
                .into_iter()
                .map(|arg| arg.into_lua_ast(ctx, lua_ctx))
                .collect();

            ast.set("args", iter_to_lua_array(args?.into_iter(), lua_ctx)?)?;

            // TODO: self, self kind

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for Arg {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Arg")?;
        ast.set("id", self.id.as_u32())?;
        ast.set("pat", self.pat.into_lua_ast(ctx, lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Block> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Block")?;

        self.and_then(|block| {
            let stmts = block.stmts
                .into_iter()
                .map(|stmt| stmt.into_lua_ast(ctx, lua_ctx))
                .collect::<LuaResult<Vec<_>>>();

            ast.set("stmts", iter_to_lua_array(stmts?.into_iter(), lua_ctx)?)?;

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Pat> {
    fn into_lua_ast(self, _ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Pat")?;
        ast.set("id", self.id.as_u32())?;

        self.and_then(|pat| {
            match pat.node {
                PatKind::Wild => {
                    ast.set("kind", "Wild")?;
                },
                PatKind::Ident(binding, ident, _sub_pattern) => {
                    ast.set("kind", "Ident")?;
                    ast.set("binding", match binding {
                        BindingMode::ByRef(Immutable) => "ByRefImmutable",
                        BindingMode::ByRef(Mutable) => "ByRefMutable",
                        BindingMode::ByValue(Immutable) => "ByValueImmutable",
                        BindingMode::ByValue(Mutable) => "ByValueMutable",
                    })?;
                    ast.set("ident", ident.as_str().get())?;
                },
                _ => return Err(LuaError::external(format!("unimplemented pattern type: {:?}", pat.node))),
            }

            Ok(ast)
        })
    }
}

impl<'lua> IntoLuaAst<'lua> for Crate {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Crate")?;
        ast.set("module", self.module.into_lua_ast(ctx, lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for Mod {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;
        let items = self.items
            .into_iter()
            .map(|item| item.into_lua_ast(ctx, lua_ctx))
            .collect::<LuaResult<Vec<_>>>();

        ast.set("type", "Mod")?;
        ast.set("inline", self.inline)?;
        ast.set("items", iter_to_lua_array(items?.into_iter(), lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Item> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Item")?;

        self.and_then(|item| {
            ast.set("id", item.id.as_u32())?;
            ast.set("ident", item.ident.name.as_str().get())?;

            match item.node {
                ItemKind::ExternCrate(opt_name) => {
                    ast.set("kind", "ExternCrate")?;

                    if let Some(name) = opt_name {
                        ast.set("name", name.as_str().get())?;
                    }
                },
                ItemKind::Mod(module) => {
                    ast.set("kind", "Mod")?;
                    ast.set("mod", module.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ItemKind::Use(_use_tree) => {
                    ast.set("kind", "Use")?;
                    // ast.set("use_tree", use_tree.into_lua_ast(ctx, lua_ctx))?;
                },
                ItemKind::Fn(decl, _header, _generics, block) => {
                    ast.set("kind", "Fn")?;
                    ast.set("decl", decl.into_lua_ast(ctx, lua_ctx)?)?;
                    ast.set("block", block.into_lua_ast(ctx, lua_ctx)?)?;
                },
                ItemKind::Struct(_variant_data, _generics) => {
                    ast.set("kind", "Struct")?;
                },
                ItemKind::Impl(..) => {
                    ast.set("kind", "Impl")?;
                }
                ref e => warn!("Item IntoLuaAst kind: {:?}", e),
            }

            Ok(ast)
        })

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
            _ => warn!("MergeLuaAst::merge_lua_ast unimplemented for non Local StmtKind"),
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
        // REVIEW: How do allow the type to be changed?
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
        // TODO: init expr, ty
        self.pat.merge_lua_ast(table.get("pat")?)?;

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

        // REVIEW: How do allow the type to be changed?
        match self.node {
            ref e => warn!("MergeLuaAst Item ItemKind: {:?}", e),
        }

        Ok(())
    }
}
