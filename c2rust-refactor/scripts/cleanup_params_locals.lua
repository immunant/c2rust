require "utils"

Variable = {}

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

function is_simple_expr_path(expr)
    if expr.kind ~= "Path" or #expr.segments ~= 1 then
        return false
    end

    return true
end

function set_by_value_mutable(var)
    var.binding = "ByValueMutable"
end

function set_used(var)
    var.used = true
end

function Visitor:visit_expr(expr, walk)
    debug("Visiting Expr: " .. expr.kind)
    if is_simple_expr_path(expr) then
        self:find_variable(expr.segments[1].ident, set_used)
    elseif expr.kind == "Assign" or expr.kind == "AssignOp" then
        ident = nil

        -- Here we might find a path, ie `a += 1` or an index path, ie `a[1] += 1`
        if is_simple_expr_path(expr.lhs) then
            ident = expr.lhs.segments[1].ident
        elseif expr.lhs.kind == "Index" then
            if is_simple_expr_path(expr.lhs.indexed) then
                ident = expr.lhs.indexed.segments[1].ident
            end
        end

        -- Find the variable and mark it as mutable
        if ident then
            self:find_variable(ident, set_by_value_mutable)
        end
    elseif expr.kind == "InlineAsm" then
        for _, input in ipairs(expr.inputs) do
            if is_simple_expr_path(input.expr) then
                self:find_variable(input.expr.segments[1].ident, set_used)
            end
        end

        for _, output in ipairs(expr.outputs) do
            if is_simple_expr_path(output.expr) then
                self:find_variable(output.expr.segments[1].ident, set_by_value_mutable)
            end
        end
    elseif expr.kind == "MethodCall" then
        -- This may need to be more complex. What if self isn't an ident but say an
        -- index? ie `x[1].as_mut_ptr()` x still needs to be mutable?
        if is_simple_expr_path(expr.args[1]) and expr.caller_is == "ref_mut" then
            self:find_variable(expr.args[1].segments[1].ident, set_by_value_mutable)
        end
    end

    walk(expr)

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

    -- Most trait methods don't have default impls, though they can
    if fn_like.kind == "TraitMethod" and not fn_like.block then
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
    function(transform_ctx)
        return transform_ctx:visit_fn_like(Visitor.new())
    end
)
refactor:save_crate()

print("Finished cleanup_params_locals.lua")
