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

refactor:transform(
    function(transform_ctx)
        node_ids = Set.new{12, 21}
        return transform_ctx:visit_fn_like_new(function(fn_like)
            -- Skip foreign functions - we only want functions with bodies
            print("Hello wurld")
            fn_like_kind = fn_like:get_kind()

            if fn_like_kind == "Foreign" then
                return
            end

            -- Most trait methods don't have default impls, though they can
            if fn_like_kind == "TraitMethod" and not fn_like:has_block() then
                return
            end

            debug("FnLike name: " .. fn_like:get_ident())

            args = fn_like:get_decl().args
            stmts = fn_like:get_block().stmts

            for _, arg in ipairs(args) do
                -- TODO: Pattern might not be an ident (ie could be tuple of ptrs)
                if node_ids[arg.id] and arg.ty.kind == "Ptr" then
                    debug("Found node: " .. arg.id)

                    arg.ty.kind = "Rptr"
                end
            end

            return true
        end)
    end
)

print("Finished upgrade_ptr_to_ref.lua")
