use rlua::prelude::{LuaContext, LuaResult, LuaTable};
use rlua::{UserData, UserDataMethods};
use syntax::ast::{Arg, FnDecl, PatKind};
use syntax::source_map::symbol::Symbol;
use syntax::ptr::P;

use crate::ast_manip::fn_edit::{FnKind, FnLike, mut_visit_fns};
use crate::command::CommandState;
use crate::scripting::{IntoLuaAst, TransformCtxt, utils::iter_to_lua_array};

pub struct AstVisitor<'a> {
    st: &'a CommandState,
}

impl<'cs> AstVisitor<'cs> {
    pub fn new(st: &'cs CommandState) -> Self {
        AstVisitor {
            st,
        }
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
        ast.set("decl", self.decl.clone().into_lua_ast(ctx, lua_ctx)?)?;

        Ok(ast)
    }
}

impl<'lua> IntoLuaAst<'lua> for P<FnDecl> {
    fn into_lua_ast(self, ctx: &TransformCtxt, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>> {
        let ast = lua_ctx.create_table()?;

        ast.set("type", "FnDecl")?;
        ast.set("c_variadic", self.c_variadic)?;
        // ast.set("return_type", self.output)?;

        let args: LuaResult<Vec<_>> = self.inputs
            .iter()
            .map(|arg| arg.clone().into_lua_ast(ctx, lua_ctx))
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

        match self.pat.node {
            PatKind::Ident(_, ident, _) => ast.set("ident", ident.as_str().get())?,
            ref e => unreachable!("Found {:?}", e),
        }

        Ok(ast)
    }
}
