refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit(
            function(visitor)
                visitor:visit_fn_like(
                    function(fn_like)
                        -- Skip foreign functions - we only want functions with bodies
                        if fn_like:is_foreign() then
                            return
                        end

                        print("Running function visitor! - found function", fn_like:get_ident())
                    end
                )
                return crate
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
