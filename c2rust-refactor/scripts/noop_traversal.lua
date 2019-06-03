refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like({}, crate)
    end
)
