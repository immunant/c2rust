local function starts_with(str, start)
    return str:sub(1, #start) == start
end

refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like(crate,
            function(fn_like)
                -- Skip foreign functions - we only want functions with bodies
                if fn_like.kind == "Foreign" then
                    return fn_like
                end

                print("FnLike name: " .. fn_like.ident)

                args = fn_like.decl.args

                used_args = {}

                for _, stmt in ipairs(fn_like.block.stmts) do
                    if stmt.kind == "Local" then
                        print("Found Local")
                    elseif stmt.kind == "Item" then
                        print("Found Item")
                    elseif stmt.kind == "Semi" or stmt.kind == "Expr" then
                        print("Found Semi or Expr of kind: " .. stmt.kind)

                        print("tctx:", transform_ctx)
                        -- lua_expr = transform_ctx:get_ast(stmt.expr)
                        lua_expr = stmt.expr

                        print(lua_expr)
                        print("Expr kind: " .. lua_expr.kind)
                        print("Expr node id: " .. lua_expr.id)
                        lhs = lua_expr.lhs
                        rhs = lua_expr.rhs
                        print("Lhs: " .. lua_expr.lhs.id .. " kind: " .. lua_expr.lhs.kind)
                        print("Rhs: " .. lua_expr.rhs.id .. " kind: " .. lua_expr.rhs.kind)

                        -- TODO: Visitor pattern this on paths?
                        if #lhs.segments == 1 then
                            for i, arg in ipairs(args) do
                                if arg.ident == lhs.segments[1] then
                                    used_args[arg.ident] = true
                                end
                            end
                        end

                        if #rhs.segments == 1 then
                            for i, arg in ipairs(args) do
                                if arg.ident == rhs.segments[1] then
                                    used_args[arg.ident] = true
                                end
                            end
                        end

                        for _, segment in ipairs(lhs.segments) do
                            print("Segment " .. segment)
                        end
                    else
                        print("Unsupported stmt kind: " .. stmt.kind)
                        return fn_like
                    end
                end

                for i, arg in ipairs(args) do
                    if not used_args[arg.ident] and not starts_with(arg.ident, '_') then
                        print("Renaming arg " .. arg.ident)
                        arg.ident = '_' .. arg.ident
                        arg.binding = "ByValueImmutable"
                    end
                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
