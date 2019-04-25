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

                        print("FnLike name: " .. fn_like:get_name())
                        args = fn_like:get_args()

                        print(args[0]:get_name())

                        -- for arg = 1, #args do
                        --     print("Renaming arg", arg:get_name())

                        --     arg:set_name("silly_name")
                        -- end
                    end
                )
                return crate
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
