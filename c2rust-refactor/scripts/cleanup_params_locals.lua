local function starts_with(str, start)
    return str:sub(1, #start) == start
end

Visitor = {}

function Visitor:new()
    setmetatable({}, self)

    self.__index = Visitor
    self.root = root
    self.used_args = {}
    self.used_locals = {}
    self.mut_args = {}

    return self
end

function Visitor:run(ast)
    if not ast then return end

    print(ast.type)
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

function Visitor:visit_stmt(block)
    print("Visiting Stmt: Noop")
end

function Visitor:visit_expr(expr)
    print("Visiting Expr")
    if expr.kind == "Path" and #expr.segments == 1 then
        -- if arg.ident == expr.segments[1] then
        self.used_args[expr.segments[1]] = true
        -- end
    elseif(expr.kind == "Assign"
        or expr.kind == "AssignOp")
        and expr.lhs.kind == "Path" then
        if #expr.lhs.segments == 1 then -- and arg.ident == expr.lhs.segments[1]
            -- print("ARG: " .. arg.ident)
            self.mut_args[expr.lhs.segments[1]] = true
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

                args = fn_like.decl.args

                used_args = {}
                used_locals = {}
                mut_args = {}

                print("Running visitor")
                visitor = Visitor:new()
                visitor:run(fn_like.block)
                print("Visitor ran")

                used_args = visitor.used_args
                used_locals = visitor.used_locals
                mut_args = visitor.mut_args

                -- TODO: Shadowed variables may cause usage to be misrepresented

                -- Iterate over args
                for i, arg in ipairs(args) do
                    if not used_args[arg.ident] then
                        -- If the argument doesn't already have an underscore
                        -- prefix, we should add one as it is idomatic rust
                        if not starts_with(arg.ident, '_') then
                            arg.ident = '_' .. arg.ident
                        end

                        -- Remove any binding since the param is deemed unused
                        arg.binding = "ByValueImmutable"
                    elseif not mut_args[arg.ident] then
                        arg.binding = "ByValueImmutable"
                    end
                end

                return fn_like
            end
        )
    end
)

print("Finished cleanup_params_locals.lua")
