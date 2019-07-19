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

function strip_int_suffix(expr)
    local lit = expr:get_lit()

    if lit then
        lit:strip_suffix()
        expr:to_lit(lit)
    end

    return expr
end

function is_int_zero(expr)
    local lit = expr:get_lit()

    return lit and lit:get_value() == 0
end

Visitor = {}

function Visitor.new(tctx, node_id)
   self = {}
   self.tctx = tctx
   self.node_ids = node_ids
   self.vars = {}

   setmetatable(self, Visitor)
   Visitor.__index = Visitor

   return self
end

function Visitor:visit_arg(arg)
    local arg_id = arg:get_id()

    if self.node_ids[arg_id] then
        local arg_ty = arg:get_ty()

        if arg_ty:get_kind() == "Ptr" then
            local mut_ty = arg_ty:get_mut_ty()
            arg_pat_hrid = self.tctx:get_nodeid_hrid(arg:get_pat_id())

            self.vars[arg_pat_hrid] = Variable.new(arg_id, false)

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
    expr_kind = expr:get_kind()

    -- (*foo).bar -> (foo).bar (can't remove parens..)
    -- TODO: Or MethodCall? FnPtr could be a field
    if expr_kind == "Field" then
        local field_expr = expr:get_exprs()[1]

        if field_expr:get_kind() == "Unary" and field_expr:get_op() == "Deref" then
            local derefed_expr = field_expr:get_exprs()[1]

            if derefed_expr:get_kind() == "Path" then
                local id = self.tctx:get_expr_path_hrid(derefed_expr)

                -- This is a path we're expecting to modify
                if self.vars[id] then
                    expr:set_exprs{derefed_expr}
                end
            end
        end
    -- *p.offset(x).offset(y) -> p[x + y]
    elseif expr_kind == "Unary" and expr:get_op() == "Deref" then
        local derefed_exprs = expr:get_exprs()
        local unwrapped_expr = derefed_exprs[1]

        if unwrapped_expr:get_method_name() == "offset" then
            offset_expr = nil

            while true do
                local unwrapped_exprs = unwrapped_expr:get_exprs()
                unwrapped_expr = unwrapped_exprs[1]
                local method_name = unwrapped_expr:get_method_name()

                -- Accumulate offset params
                if not offset_expr then
                    offset_expr = strip_int_suffix(unwrapped_exprs[2])
                elseif not is_int_zero(unwrapped_exprs[2]) then
                    offset_expr:to_binary("Add", strip_int_suffix(unwrapped_exprs[2]), offset_expr)
                end

                if method_name ~= "offset" then
                    break
                end
            end

            -- Should be left with a path, otherwise bail
            local id = self.tctx:get_expr_path_hrid(unwrapped_expr)
            local var = self.vars[id]

            -- We only want to apply this operation if we're converting
            -- a pointer to an array
            if var and self.node_ids[var.id] == "ref_slice" and offset_expr then
                expr:to_index(unwrapped_expr, offset_expr)
            end
        end
    end
end

refactor:transform(
    function(transform_ctx)
        node_ids = {
            [12] = "ref",
            [21] = "ref",
            [63] = "ref",
            [73] = "ref_slice",
        }
        return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
