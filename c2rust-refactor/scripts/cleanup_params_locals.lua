function starts_with(str, start)
    return str:sub(1, #start) == start
end

Variable = {}

DEBUG = false

function debug(str)
    if DEBUG then
        print(str)
    end
end

function Variable.new(used, id, locl, binding, ident)
    self = {}
    self.used = used
    self.id = id
    self.locl = locl
    self.binding = binding
    self.ident = ident
    self.shadowed = false

    setmetatable(self, Variable)
    Variable.__index = Variable

    return self
end

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

function Visitor:visit_stmt(stmt)
    if stmt.kind == "Local" then
        debug("Found local in visitor")
        if stmt.pat.kind == "Ident" then
            used = false
            locl = true
            id = stmt.pat.id
            ident = stmt.pat.ident
            binding = "ByValueImmutable"

            -- Find a shadowed variable
            self:find_variable(ident,
                function(var)
                    var.shadowed = true
                end
            )

            self.variables[id] = Variable.new(used, id, locl, binding, ident)

            table.insert(self.locals, stmt)
        else
            debug("Skipping unsupported local type")
        end
    end

    return true
end

function Visitor:visit_expr(expr)
    debug("Visiting Expr: " .. expr.kind)
    if expr.kind == "Path" and #expr.segments == 1 then
        self:find_variable(expr.segments[1],
            function(var)
                var.used = true
            end
        )

    elseif (expr.kind == "Assign" or expr.kind == "AssignOp") and
            expr.lhs.kind == "Path" then
        if #expr.lhs.segments == 1 then
            self:find_variable(expr.lhs.segments[1],
                function(var)
                    var.binding = "ByValueMutable"
                end
            )
        end
    end

    return true
end

function Visitor:find_variable(ident, mutator)
    for _, variable in pairs(self.variables) do
        if variable.ident == ident then
            mutator(variable)
        end
    end
end

function Visitor:visit_fn_like(fn_like)
    -- Skip foreign functions - we only want functions with bodies
    if fn_like.kind == "Foreign" then
        return
    end

    debug("FnLike name: " .. fn_like.ident)

    args = fn_like.decl.args
    stmts = fn_like.block.stmts

    for _, arg in ipairs(args) do
        -- TODO: Pattern might not be an ident
        used = false
        id = arg.id
        locl = false
        binding = "ByValueImmutable"
        ident = arg.pat.ident

        self.variables[id] = Variable.new(used, id, locl, binding, ident)

        table.insert(self.args, arg)

        debug("Arg: " .. ident .. " of id " .. id .. " binding " .. binding)
    end

    return true
end

function update_pattern(pattern, variable)
    -- TODO: Pattern might not be an ident
    debug("variable " .. pattern.ident .. " used: " .. tostring(variable.used))
    if not variable.used then
        pattern.binding = "ByValueImmutable"

        -- If the argument doesn't already have an underscore
        -- prefix, we should add one as it is idomatic rust
        if not starts_with(pattern.ident, '_') then
            pattern.ident = '_' .. pattern.ident
        end
    else
        pattern.binding = variable.binding
    end
end

function Visitor:finish()
    -- Iterate over args
    for _, arg in ipairs(self.args) do
        variable = self.variables[arg.id]

        update_pattern(arg.pat, variable)
    end

    -- Iterate over locals
    for _, stmt in ipairs(self.locals) do
        variable = self.variables[stmt.pat.id]

        update_pattern(stmt.pat, variable)
    end
end

refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_crate(Visitor.new(), crate)
    end
)

print("Finished cleanup_params_locals.lua")
