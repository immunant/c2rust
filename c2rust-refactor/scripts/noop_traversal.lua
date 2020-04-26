refactor:transform(
    function(transform_ctx)
        return transform_ctx:visit_fn_like({})
    end
)
