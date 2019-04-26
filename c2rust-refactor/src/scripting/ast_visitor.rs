use rlua::prelude::{LuaContext, LuaResult, LuaTable};
use syntax::ast::{Arg, FnDecl, NodeId, PatKind};
use syntax::source_map::symbol::Symbol;
use syntax::ptr::P;

use crate::ast_manip::fn_edit::{FnKind, FnLike};
use crate::scripting::{IntoLuaAst, TransformCtxt, utils::iter_to_lua_array};

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
        ast.set("decl", self.decl.clone().into_lua_ast(ctx, lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<FnDecl> {
    fn into_lua_ast(mut self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnDecl")?;
        ast.set("c_variadic", self.c_variadic)?;
        // ast.set("return_type", self.output)?;

        let args: LuaResult<Vec<_>> = self.inputs
            .drain(..)
            .into_iter()
            .map(|arg| arg.into_lua_ast(ctx, lua_ctx))
            .collect();

        ast.set("args", iter_to_lua_array(args?.into_iter(), lua_ctx)?)?;

        // TODO: self, self kind

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for Arg {
    fn into_lua_ast(self, _ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "Arg")?;
        ast.set("id", self.id.as_u32())?;

        match self.pat.node {
            PatKind::Ident(_, ident, _) => ast.set("ident", ident.as_str().get())?,
            ref e => unreachable!("Found {:?}", e),
        }

        Ok(ast)
    }
}

pub(crate) trait MergeLuaAst {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()>;
}

impl MergeLuaAst for FnLike {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        match table.get::<_, String>("kind")?.as_str() {
            "Normal" => self.kind = FnKind::Normal,
            "ImplMethod" => self.kind = FnKind::ImplMethod,
            "TraitMethod" => self.kind = FnKind::TraitMethod,
            "Foreign" => self.kind = FnKind::Foreign,
            _ => (),
        };
        self.id = NodeId::from_u32(table.get("id")?);
        self.ident.name = Symbol::intern(&table.get::<_, String>("ident")?);
        self.decl.merge_lua_ast(table.get("decl")?)?;

        // TODO: Block
        // TODO: Attrs

        Ok(())
    }
}

impl MergeLuaAst for P<FnDecl> {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        let lua_args: LuaTable = table.get("args")?;

        // TODO: This may need to be improved if we want to delete or add
        // arguments as it currently expects args to be 1-1
        self.inputs.iter_mut().enumerate().map(|(i, arg)| {
            let arg_table: LuaTable = lua_args.get(i + 1)?;

            arg.merge_lua_ast(arg_table)
        }).collect()
    }
}

impl MergeLuaAst for Arg {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
        match self.pat.node {
            PatKind::Ident(_, ref mut ident, _) =>
                ident.name = Symbol::intern(&table.get::<_, String>("ident")?),
            ref e => unimplemented!("Found {:?}", e),
        }

        self.id = NodeId::from_u32(table.get("id")?);

        Ok(())
    }
}
