refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like(
            function(fn_like)
                -- Skip foreign functions - we only want functions with bodies
                if fn_like.kind == "Foreign" then
                    return
                end

                print("FnLike name: " .. fn_like.ident)

                fn_like.ident = "silly_fn"

                args = fn_like.decl.args

                for _, arg in ipairs(args) do
                    print("Renaming arg", arg.ident)

                    arg.ident = "silly_name"
                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
