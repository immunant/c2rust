local function starts_with(str, start)
    return str:sub(1, #start) == start
end

-- Visitor = {root = nil}

-- function Visitor:new(root)
--     setmetatable({}, self)

--     self.__index = Visitor
--     self.root = root

--     return self
-- end

-- function Visitor:visit_expr(expr)
--     if expr.kind == "Path" then
--         print("Found path!")
--     else
--         print("Found not path!")
--     end

--     return 1
-- end

-- function visit_expr(expr, callback)
--     callback(expr)
--     if expr.kind == "Box" then
--         visit_expr(expr.boxed, callback)
--     elseif expr.kind == "Array" then
--         for _, value in ipairs(expr.values) do
--             visit_expr(value, callback)
--         end
--     elseif expr.kind == "AssignOp" then
--         print(expr.lhs.segments[1] .. " & " .. expr.rhs.segments[1])
--         visit_expr(expr.lhs, callback)
--         visit_expr(expr.rhs, callback)
--     elseif expr.kind == "Path" then

--     else
--         error("Found unsupported expr type " .. expr.kind)
--     end
-- end

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

                        visit_expr(stmt.expr,
                            function(expr)
                                if expr.kind == "Path" and #expr.segments == 1 then
                                    for i, arg in ipairs(args) do
                                        if arg.ident == expr.segments[1] then
                                            used_args[arg.ident] = true
                                        end
                                    end
                                end
                            end
                        )

                    else
                        print("Unsupported stmt kind: " .. stmt.kind)
                    end
                end

                for i, arg in ipairs(args) do
                    if not used_args[arg.ident] then
                        if not starts_with(arg.ident, '_') then
                            print("Renaming arg " .. arg.ident)
                            arg.ident = '_' .. arg.ident
                        end

                        arg.binding = "ByValueImmutable"
                    end
                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
