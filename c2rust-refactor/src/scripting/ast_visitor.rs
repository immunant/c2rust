use rlua::prelude::{LuaContext, LuaResult, LuaTable};
use syntax::ast::{Arg, BindingMode, Block, FunctionRetTy, FnDecl, Mutability::*, NodeId, PatKind};
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
        ast.set("decl", self.decl.into_lua_ast(ctx, lua_ctx)?)?;

        let block = self.block
            .map(|b| b.into_lua_ast(ctx, lua_ctx))
            .transpose()?;

        ast.set("block", block)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<FnDecl> {
    fn into_lua_ast(mut self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnDecl")?;
        ast.set("c_variadic", self.c_variadic)?;

        ast.set("return_type", match &self.output {
            FunctionRetTy::Default(_) => None,
            FunctionRetTy::Ty(ty) => Some(ctx.intern(ty.clone())),
        })?;

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
            PatKind::Ident(binding, ident, _) => {
                ast.set("binding", match binding {
                    BindingMode::ByRef(Immutable) => "ByRefImmutable",
                    BindingMode::ByRef(Mutable) => "ByRefMutable",
                    BindingMode::ByValue(Immutable) => "ByValueImmutable",
                    BindingMode::ByValue(Mutable) => "ByValueMutable",
                })?;

                ast.set("ident", ident.as_str().get())?;

            },
            ref e => unreachable!("Found {:?}", e),
        }

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<Block> {
    fn into_lua_ast(mut self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        let stmts = self.stmts
            .drain(..)
            .into_iter()
            .map(|stmt| stmt.into_lua_ast(ctx, lua_ctx))
            .collect::<LuaResult<Vec<_>>>();

        ast.set("stmts", iter_to_lua_array(stmts?.into_iter(), lua_ctx)?)?;

        Ok(ast)
    }
}

pub(crate) trait MergeLuaAst {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()>;
}

impl MergeLuaAst for FnLike {
    fn merge_lua_ast<'lua>(&mut self, table: LuaTable<'lua>) -> LuaResult<()> {
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
            ref e => unimplemented!("Found {:?}", e),
        }

        self.id = NodeId::from_u32(table.get("id")?);

        Ok(())
    }
}
