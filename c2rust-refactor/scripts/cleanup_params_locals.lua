refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like(crate,
            function(fn_like)
                -- Skip foreign functions - we only want functions with bodies
                if fn_like.kind == "Foreign" then
                    return fn_like
                end

                print("FnLike name: " .. fn_like.ident)

                fn_like.ident = "silly_fn"
                print(fn_like.block.stmts)

                args = fn_like.decl.args

                for i, arg in ipairs(args) do
                    print("Renaming arg", arg.ident)

                    arg.ident = "sh" .. i
                    arg.binding = "ByValueImmutable"
                end

                for _, stmt in ipairs(fn_like.block.stmts) do
                    if stmt.kind == "Local" then
                        print("Found Local")
                    elseif stmt.kind == "Item" then
                        print("Found Item")
                    elseif stmt.kind == "Semi" or stmt.kind == "Expr" then
                        print("Found Semi or Expr of kind: " .. stmt.kind)

                        print("tctx:", transform_ctx)
                        lua_expr = transform_ctx:get_ast(stmt.expr)

                        print(lua_expr)
                        print(lua_expr.kind)
                    else
                        print("Unsupported stmt kind: " .. stmt.kind)
                        return fn_like
                    end

                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
