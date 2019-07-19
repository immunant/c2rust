-- Take a set of node ids (params) and turn them (if a pointer) into a reference
Variable = {}

function Variable.new(id, locl)
    self = {}
    self.id = id
    self.locl = locl
    self.shadowed = false

    setmetatable(self, Variable)
    Variable.__index = Variable

    return self
end

Visitor = {}

function Visitor.new(tctx, node_id)
   self = {}
   self.tctx = tctx
   self.node_ids = node_ids
   self.vars = {}
   self.param_idx = 0

   setmetatable(self, Visitor)
   Visitor.__index = Visitor

   return self
end

function Visitor:visit_fn_header(header)
    self.param_idx = 0
end

function Visitor:visit_arg(arg)
    arg_id = arg:get_id()

    self.param_idx = self.param_idx + 1

    if self.node_ids[arg_id] then
        arg_ty = arg:get_ty()

        if arg_ty:get_kind() == "Ptr" then
            mut_ty = arg_ty:get_mut_ty()
            -- arg_pat_id = arg:get_pat_id()

            -- NOTE: This is a 1-index of param position, so will
            -- need tweaking to support locals
            self.vars[self.param_idx] = Variable.new(self.param_idx, false)

            if self.node_ids[arg_id] == "ref_slice" then
                pointee_ty = mut_ty:get_ty()
                pointee_ty:wrap_in_slice()
                mut_ty:set_ty(pointee_ty)
            end

            arg_ty:to_rptr(nil, mut_ty)
            arg:set_ty(arg_ty)
        end
    end
end

function Visitor:visit_expr(expr)
    -- (*foo).bar -> (foo).bar (can't remove parens..)
    -- REVIEW: Or MethodCall?
    if expr:get_kind() == "Field" then
        field_expr = expr:get_exprs()[1]

        if field_expr:get_kind() == "Unary" and field_expr:get_op() == "Deref" then
            deref_expr = field_expr:get_exprs()[1]

            if deref_expr:get_kind() == "Path" then
                id = self.tctx:get_expr_path_hrid(deref_expr)

                -- This is a path we're expecting to modify
                if self.vars[id] then
                    expr:set_exprs{deref_expr}
                end
            end
        end
    end
end

refactor:transform(
    function(transform_ctx)
        node_ids = {
            [12] = "ref",
            [21] = "ref",
            [56] = "ref_slice",
            [70] = "ref",
        }
        return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
