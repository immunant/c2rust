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

RefCfg = {}

function RefCfg.new(is_slice, lifetime)
   self = {}
   self.is_slice = is_slice
   self.lifetime = lifetime

   setmetatable(self, RefCfg)
   RefCfg.__index = RefCfg

   return self
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

-- Takes a ptr type and returns the newly modified ref type
function ptr_to_ref(ptr_ty, conversion_cfg)
    local mut_ty = ptr_ty:get_mut_ty()

    if conversion_cfg.is_slice then
        local pointee_ty = mut_ty:get_ty()
        pointee_ty:wrap_in_slice()
        mut_ty:set_ty(pointee_ty)
    end

    ptr_ty:to_rptr(conversion_cfg.lifetime, mut_ty)
end

function Visitor:visit_arg(arg)
    local arg_id = arg:get_id()
    local conversion_cfg = self.node_ids[arg_id]

    if conversion_cfg then
        local arg_ty = arg:get_ty()

        if arg_ty:get_kind() == "Ptr" then
            local arg_pat_hrid = self.tctx:get_nodeid_hrid(arg:get_pat_id())

            ptr_to_ref(arg_ty, conversion_cfg)

            arg:set_ty(arg_ty)

            self.vars[arg_pat_hrid] = Variable.new(arg_id, false)
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
                else
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
            if var and self.node_ids[var.id].is_slice and offset_expr then
                expr:to_index(unwrapped_expr, offset_expr)
            end
        end
    end
end

function Visitor:visit_item_kind(item_kind)
    if item_kind:get_kind() == "Struct" then
        local field_ids = item_kind:get_field_ids()

        item_kind:print()
        print(field_ids)

        for _, field_id in ipairs(field_ids) do
            local ref_cfg = self.node_ids[field_id]

            if ref_cfg and ref_cfg.lifetime then
                item_kind:add_lifetime(ref_cfg.lifetime)
            end
        end
    end
end

function Visitor:visit_struct_field(field)
    local field_id = field:get_id()
    local conversion_cfg = self.node_ids[field_id]

    if conversion_cfg then
        local field_ty = field:get_ty()

        ptr_to_ref(field_ty, conversion_cfg)

        field:set_ty(field_ty)
    end
end

refactor:transform(
    function(transform_ctx)
        node_ids = {
            [12] = RefCfg.new(false, nil),
            [21] = RefCfg.new(false, nil),
            [63] = RefCfg.new(false, nil),
            [73] = RefCfg.new(true, nil),
            [116] = RefCfg.new(false, "r"),
            [120] = RefCfg.new(false, "r"),
            [124] = RefCfg.new(true, "s"),
            [128] = RefCfg.new(true, "s"),
        }
        return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
