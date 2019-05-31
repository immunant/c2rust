require "utils"

Visitor = {}

function Visitor.new()
    self = {}
    self.args = {}
    self.locals = {}
    self.variables = {}

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
end

function Visitor:visit_expr(expr)
    debug("Visiting Expr: " .. expr.kind)
    if expr.kind == "Binary" then
        tmp = expr.lhs
        expr.lhs = expr.rhs
        expr.rhs = tmp
    end

    return true
end

refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like(Visitor.new(), crate)
    end
)

print("Finished change_ast_kinds.lua")
