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

function RefCfg.new(mod_type, lifetime)
    self = {}
    self.mod_type = mod_type
    self.lifetime = lifetime

    setmetatable(self, RefCfg)
    RefCfg.__index = RefCfg

    return self
end

function RefCfg:is_slice()
    return self.mod_type == "slice"
end

function RefCfg:is_ref()
    return self.mod_type == "ref"
end

function RefCfg:is_opt_any()
    return self:is_opt_box() or self:is_opt_box_slice()
end

function RefCfg:is_opt_box()
    return self.mod_type == "opt_box"
end

function RefCfg:is_opt_box_slice()
    return self.mod_type == "opt_box_slice"
end

function RefCfg:is_slice_any()
    return self:is_slice() or self:is_opt_box_slice()
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
function upgrade_ptr(ptr_ty, conversion_cfg)
    local mut_ty = ptr_ty:get_mut_ty()

    if conversion_cfg:is_slice() then
        local pointee_ty = mut_ty:get_ty()
        pointee_ty:wrap_in_slice()
        mut_ty:set_ty(pointee_ty)
    end

    if conversion_cfg:is_opt_box() then
        local pointee_ty = mut_ty:get_ty()

        pointee_ty:wrap_as_generic_angle_arg("Box")
        pointee_ty:wrap_as_generic_angle_arg("Option")

        return pointee_ty
    elseif conversion_cfg:is_opt_box_slice() then
        local pointee_ty = mut_ty:get_ty()

        pointee_ty:wrap_in_slice()
        pointee_ty:wrap_as_generic_angle_arg("Box")
        pointee_ty:wrap_as_generic_angle_arg("Option")

        return pointee_ty
    else
        ptr_ty:to_rptr(conversion_cfg.lifetime, mut_ty)

        return ptr_ty
    end
end

function Visitor:visit_arg(arg)
    local arg_id = arg:get_id()
    local conversion_cfg = self.node_ids[arg_id]

    if conversion_cfg then
        local arg_ty = arg:get_ty()

        if arg_ty:get_kind() == "Ptr" then
            local arg_pat_hrid = self.tctx:get_nodeid_hrid(arg:get_pat_id())

            arg:set_ty(upgrade_ptr(arg_ty, conversion_cfg))

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
            if var and self.node_ids[var.id]:is_slice_any() and offset_expr then
                expr:to_index(unwrapped_expr, offset_expr)
            end
        end
    elseif expr:get_method_name() == "is_null" then
        expr:set_method_name("is_none")
    elseif expr_kind == "Assign" then
        local exprs = expr:get_exprs()
        local lhs = exprs[1]
        local rhs = exprs[2]

        local id = self.tctx:get_expr_path_hrid(lhs)
        local var = self.vars[id]

        if rhs:get_kind() == "Cast" then
            local cast_expr = rhs:get_exprs()[1]
            local cast_ty = rhs:get_ty()

            if cast_ty:get_kind() == "Ptr" then
                local call_exprs = cast_expr:get_exprs()
                local path_expr = call_exprs[1]
                local param_expr = call_exprs[2]
                local path = path_expr:get_path()
                local segments = path:get_segments()

                -- In case malloc is called from another module check the last segment
                if segments[#segments] == "malloc" then
                    local mut_ty = cast_ty:get_mut_ty()
                    local pointee_ty = mut_ty:get_ty()

                    path:set_segments{"", "core", "mem", "size_of"}
                    path:set_generic_angled_arg_tys(4, {pointee_ty})
                    -- TODO: usize cast
                    path_expr:to_path(path)
                    path_expr:to_call{path_expr}

                    -- TODO: zero-init will only work for numbers, not structs
                    local init = self.tctx:int_lit_expr(0, nil)
                    local usize_ty = self.tctx:ident_path_ty("usize")
                    local cast_expr = self.tctx:cast_expr(param_expr, usize_ty)
                    local binary_expr = self.tctx:binary_expr("Div", cast_expr, path_expr)
                    local some_path_expr = self.tctx:ident_path_expr("Some")
                    local vec_expr = self.tctx:vec_mac_init_num(init, binary_expr)

                    vec_expr:to_method_call("into_boxed_slice", {vec_expr})
                    rhs:to_call{some_path_expr, vec_expr}
                    expr:set_exprs{lhs, rhs}
                end
            end
        end
    end
end

function Visitor:visit_item_kind(item_kind)
    if item_kind:get_kind() == "Struct" then
        local field_ids = item_kind:get_field_ids()

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

        field:set_ty(upgrade_ptr(field_ty, conversion_cfg))
    end
end

function Visitor:visit_local(locl)
    local local_id = locl:get_id()
    local conversion_cfg = self.node_ids[local_id]

    -- let x: *mut T = 0 as *mut T; -> let x = None;
    if conversion_cfg and conversion_cfg:is_opt_any() then
        local init = locl:get_init()

        if init and init:get_kind() == "Cast" then
            local cast_expr = init:get_exprs()[1]
            local cast_ty = init:get_ty()

            if cast_expr:get_lit():get_value() == 0 and cast_ty:get_kind() == "Ptr" then
                init:to_ident_path("None")

                locl:set_ty(nil)
                locl:set_init(init)

                local arg_pat_hrid = self.tctx:get_nodeid_hrid(locl:get_pat_id())

                self.vars[arg_pat_hrid] = Variable.new(local_id, true)
            end
        end
    end
end

refactor:transform(
    function(transform_ctx)
        node_ids = {
            [26] = RefCfg.new("ref", nil),
            [35] = RefCfg.new("ref", nil),
            [77] = RefCfg.new("ref", nil),
            [87] = RefCfg.new("slice", nil),
            [130] = RefCfg.new("ref", "r"),
            [134] = RefCfg.new("ref", "r"),
            [138] = RefCfg.new("slice", "s"),
            [142] = RefCfg.new("slice", "s"),
            [146] = RefCfg.new("opt_box", nil),
            [151] = RefCfg.new("opt_box_slice", nil),
            [159] = RefCfg.new("ref", nil),
            [167] = RefCfg.new("opt_box_slice", nil),
        }
        return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
