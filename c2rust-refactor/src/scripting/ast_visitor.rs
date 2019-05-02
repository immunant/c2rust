use rlua::prelude::{LuaContext, LuaError, LuaResult, LuaTable};
use syntax::ast::{
    Arg, BindingMode, Block, FunctionRetTy, FnDecl, Local, Mutability::*,
    NodeId, Pat, PatKind, Stmt, StmtKind,
};
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
            _ => println!("MergeLuaAst::merge_lua_ast unimplemented for non Local StmtKind"),
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
            ref e => unimplemented!("Found {:?}", e),
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
