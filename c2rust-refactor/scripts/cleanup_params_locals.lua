local function starts_with(str, start)
    return str:sub(1, #start) == start
end

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

function Visitor.new(params)
    self = {}
    self.variables = params

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
end

function Visitor:run(ast)
    if not ast then return end

    -- print(ast.type)
    if ast.type == "Block" then
        self:visit_block(ast)

        for _, stmt in ipairs(ast.stmts) do
            self:run(stmt)
        end
    elseif ast.type == "Expr" then
        self:visit_expr(ast)

        if ast.kind == "Box" then
            self:run(ast.boxed)
        elseif ast.kind == "Array" then
            for _, value in ipairs(ast.values) do
                self:run(value)
            end
        elseif ast.kind == "AssignOp"
            or ast.kind == "Binary"
            or ast.kind == "Assign" then
            self:run(ast.lhs)
            self:run(ast.rhs)
        elseif ast.kind == "Path" or ast.kind == "Lit" then

        else
            error("Found unsupported expr type " .. ast.kind)
        end
    elseif ast.type == "Stmt" then
        self:visit_stmt(ast)

        if ast.kind == "Local" then
            print("Found Local")
            self:run(ast.init)
        elseif ast.kind == "Item" then
            print("Found Item")
        elseif ast.kind == "Semi" or ast.kind == "Expr" then
            print("Found Semi or Expr of kind: " .. ast.kind)

            self:run(ast.expr)
        else
            error("Unsupported stmt kind: " .. ast.kind)
        end
    else
        error("Found unsupported ast type " .. ast.type)
    end
end

function Visitor:visit_block(block)
    print("Visiting Block: Noop")
end

function Visitor:visit_stmt(stmt)
    if stmt.kind == "Local" then
        print("Found local in visitor")
        if stmt.pat.kind == "Ident" then
            used = false
            locl = true
            id = stmt.pat.id
            ident = stmt.pat.ident
            binding = "ByValueImmutable"

            -- Find a shadowed variable
            for _, variable in ipairs(self.variables) do
                if variable.ident == ident then
                    variable.shadowed = true
                end
            end

            self.variables[id] = Variable.new(used, id, locl, binding, ident)
        else
            print("Skipping unsupported local type")
        end
    end
end

function Visitor:visit_expr(expr)
    print("Visiting Expr: " .. expr.kind)
    if expr.kind == "Path" and #expr.segments == 1 then
        for _, variable in pairs(self.variables) do
            if variable.ident == expr.segments[1] then
                variable.used = true
            end
        end
    elseif(expr.kind == "Assign" or expr.kind == "AssignOp")
        and expr.lhs.kind == "Path" then
        if #expr.lhs.segments == 1 then
            print("Looping:")

            for _, variable in pairs(self.variables) do
                if variable.ident == expr.lhs.segments[1] then
                    variable.binding = "ByValueMutable"
                end
            end
        end
    end
end

refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:visit_fn_like(crate,
            function(fn_like)
                -- Skip foreign functions - we only want functions with bodies
                if fn_like.kind == "Foreign" then
                    return fn_like
                end

                print("FnLike name: " .. fn_like.ident)

                params = {}
                args = fn_like.decl.args
                stmts = fn_like.block.stmts

                for _, arg in ipairs(args) do
                    -- TODO: Pattern might not be an ident
                    used = false
                    id = arg.id
                    locl = false
                    binding = "ByValueImmutable"
                    ident = arg.pat.ident

                    params[id] = Variable.new(used, id, locl, binding, ident)
                end

                print("Running visitor")
                visitor = Visitor.new(params)
                visitor:run(fn_like.block)
                print("Visitor ran")

                -- TODO: Shadowed variables may cause usage to be misrepresented

                -- Iterate over args
                for _, arg in ipairs(args) do
                    variable = visitor.variables[arg.id]

                    -- TODO: Pattern might not be an ident
                    if not variable.used then
                        arg.pat.binding = "ByValueImmutable"

                        -- If the argument doesn't already have an underscore
                        -- prefix, we should add one as it is idomatic rust
                        if not starts_with(arg.pat.ident, '_') then
                            arg.pat.ident = '_' .. arg.pat.ident
                        end
                    else
                        arg.pat.binding = variable.binding
                    end

                    -- if not used_args[arg.pat.ident] then
                    --     -- If the argument doesn't already have an underscore
                    --     -- prefix, we should add one as it is idomatic rust
                    --     if not starts_with(arg.pat.ident, '_') then
                    --         arg.pat.ident = '_' .. arg.pat.ident
                    --     end

                    --     -- Remove any binding since the param is deemed unused
                    --     arg.pat.binding = "ByValueImmutable"
                    -- elseif not mut_args[arg.pat.ident] then
                    --     arg.pat.binding = "ByValueImmutable"
                    -- end
                end

                -- Iterate over locals
                for _, stmt in ipairs(stmts) do
                    if stmt.kind == "Local" then

                    end
                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
