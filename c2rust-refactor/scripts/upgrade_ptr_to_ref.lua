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

function Visitor.new(transform_ctx, node_ids)
    self = {}
    self.tctx = transform_ctx
    self.node_ids = node_ids

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
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
        -- TODO: Pattern might not be an ident (ie could be tuple of ptrs)
        if self.node_ids[arg.id] and arg.ty.kind == "Ptr" then
            print("Found 'em cap: " .. arg.id .. " " .. arg.ty.kind)

            arg.ty.kind = "Rptr"
        end
    end

    return true
end

refactor:transform(
    function(transform_ctx)
        node_ids = Set.new{12}
        return transform_ctx:visit_fn_like(Visitor.new(transform_ctx, node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
