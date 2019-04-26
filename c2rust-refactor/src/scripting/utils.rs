use rlua::prelude::{LuaContext, LuaResult, LuaTable, ToLua};

pub(crate) fn iter_to_lua_array<'lua, I, T: 'lua>(iter: I, lua_ctx: LuaContext<'lua>) -> LuaResult<LuaTable<'lua>>
where
    I: Iterator<Item = T>,
    T: ToLua<'lua>,
{
    let table = lua_ctx.create_table()?;

    // Lua arrays start at index 1 but enumerate starts at 0
    for (i, item) in iter.enumerate() {
        table.set(i + 1, item.to_lua(lua_ctx)?)?;
    }

    Ok(table)
}
