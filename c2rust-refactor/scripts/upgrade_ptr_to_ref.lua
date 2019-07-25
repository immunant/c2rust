-- Take a set of node ids (locals/params) and turn them (if a pointer) into a reference
-- or Box
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

Field = {}

function Field.new(id)
    self = {}
    self.id = id

    setmetatable(self, Field)
    Field.__index = Field

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

function RefCfg:is_ref_or_slice()
    return self:is_ref() or self:is_slice()
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
    return self:is_slice() or self:is_opt_box_slice() or self:is_box_slice()
end

function RefCfg:is_box_slice()
    return self.mod_type == "box_slice"
end

Visitor = {}

function Visitor.new(tctx, node_id_cfgs)
    self = {}
    self.tctx = tctx
    -- NodeId -> RefCfg
    self.node_id_cfgs = node_id_cfgs
    -- PatHrId -> Variable
    self.vars = {}
    -- ?HrId -> Field
    self.fields = {}

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
end

-- Takes a ptr type and returns the newly modified type
function upgrade_ptr(ptr_ty, conversion_cfg)
    local mut_ty = ptr_ty:get_mut_ty()
    local pointee_ty = mut_ty:get_ty()

    -- T -> [T]
    if conversion_cfg:is_slice_any() then
        pointee_ty:wrap_in_slice()
    end

    -- T -> &T / &mut T or [T] -> &[T] / &mut [T]
    if conversion_cfg:is_ref_or_slice() then
        mut_ty:set_ty(pointee_ty)
        ptr_ty:to_rptr(conversion_cfg.lifetime, mut_ty)

        return ptr_ty
    end

    -- T -> Option<Box<T>> or [T] -> Option<Box<[T]>>
    pointee_ty:wrap_as_generic_angle_arg("Box")
    pointee_ty:wrap_as_generic_angle_arg("Option")

    return pointee_ty
end

function Visitor:visit_arg(arg)
    local arg_id = arg:get_id()
    local conversion_cfg = self.node_id_cfgs[arg_id]

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
                local var = self.vars[id]

                -- This is a path we're expecting to modify
                if var and self.node_id_cfgs[var.id] then
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
            if var and self.node_id_cfgs[var.id]:is_slice_any() and offset_expr then
                -- If we're using an option, we must unwrap (or map/match)
                if self.node_id_cfgs[var.id]:is_opt_any() then
                    -- TODO: or as_ref
                    unwrapped_expr:to_method_call("as_mut", {unwrapped_expr})
                    unwrapped_expr:to_method_call("unwrap", {unwrapped_expr})
                end

                expr:to_index(unwrapped_expr, offset_expr)
            end
        end
    -- p.is_null() -> p.is_none() or false when not using an option
    elseif expr:get_method_name() == "is_null" then
        local id = self.tctx:get_expr_path_hrid(expr:get_exprs()[1])
        local var = self.vars[id]

        if var then
            if self.node_id_cfgs[var.id]:is_opt_any() then
                expr:set_method_name("is_none")
            else
                expr:to_bool_lit(false)
            end
        end
    elseif expr_kind == "Assign" then
        local exprs = expr:get_exprs()
        local lhs = exprs[1]
        local rhs = exprs[2]
        local rhs_kind = rhs:get_kind()
        local id = self.tctx:get_expr_path_hrid(lhs)
        local var = self.vars[id]

        -- p = malloc(X) as *mut T -> p = Some(vec![0; X / size_of<T>].into_boxed_slice())
        -- or p = vec![0; X / size_of<T>].into_boxed_slice()
        if rhs_kind == "Cast" then
            local conversion_cfg = var and self.node_id_cfgs[var.id]
            local cast_expr = rhs:get_exprs()[1]
            local cast_ty = rhs:get_ty()

            if cast_ty:get_kind() == "Ptr" and cast_expr:get_kind() == "Call" then
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
                    path_expr:to_path(path)
                    path_expr:to_call{path_expr}

                    -- TODO: zero-init will only work for numbers, not structs/unions
                    local init = self.tctx:int_lit_expr(0, nil)
                    local usize_ty = self.tctx:ident_path_ty("usize")
                    local cast_expr = self.tctx:cast_expr(param_expr, usize_ty)
                    local binary_expr = self.tctx:binary_expr("Div", cast_expr, path_expr)
                    local vec_expr = self.tctx:vec_mac_init_num(init, binary_expr)

                    vec_expr:to_method_call("into_boxed_slice", {vec_expr})

                    -- Only wrap in Some if we're assigning to an opt variable
                    if conversion_cfg:is_opt_any() then
                        local some_path_expr = self.tctx:ident_path_expr("Some")
                        rhs:to_call{some_path_expr, vec_expr}
                    else
                        rhs = vec_expr
                    end

                    expr:set_exprs{lhs, rhs}
                end
            end
        -- lhs = rhs -> lhs = Some(rhs)
        -- TODO: Should probably expand to work on more complex exprs
        elseif rhs_kind == "Path" then
            local id = self.tctx:get_expr_path_hrid(rhs)
            local var = self.vars[id]

            if var and not self.node_id_cfgs[var.id]:is_opt_any() then
                local lhs_ty = self.tctx:get_expr_ty(lhs)

                -- If lhs was a ptr, and rhs isn't wrapped in some, wrap it
                -- TODO: Validate rhs needs to be wrapped
                if lhs_ty:get_kind() == "Ptr" then
                    local some_path_expr = self.tctx:ident_path_expr("Some")

                    rhs:to_call{some_path_expr, rhs}
                    expr:set_exprs{lhs, rhs}
                end
            end
        end
    end
end

-- HrIds may be reused in different functions, so we should clear them out
-- so we don't accidentally access old info
function Visitor:visit_fn_decl(fn_decl)
    self.vars = {}
end

function Visitor:visit_item_kind(item_kind)
    if item_kind:get_kind() == "Struct" then
        local field_ids = item_kind:get_field_ids()

        for _, field_id in ipairs(field_ids) do
            local ref_cfg = self.node_id_cfgs[field_id]
            local field_hrid = self.tctx:get_nodeid_hrid(field_id)

            self.fields[field_hrid] = Field.new(field_id)

            if ref_cfg and ref_cfg.lifetime then
                item_kind:add_lifetime(ref_cfg.lifetime)
            end
        end
    end
end

function Visitor:visit_struct_field(field)
    local field_id = field:get_id()
    local conversion_cfg = self.node_id_cfgs[field_id]

    if conversion_cfg then
        local field_ty = field:get_ty()

        field:set_ty(upgrade_ptr(field_ty, conversion_cfg))
    end
end

function is_null_ptr(expr)
    if expr and expr:get_kind() == "Cast" then
        local cast_expr = expr:get_exprs()[1]
        local cast_ty = expr:get_ty()

        if cast_expr:get_lit():get_value() == 0 and cast_ty:get_kind() == "Ptr" then
            return true
        end
    end

    return false
end

function Visitor:visit_local(locl)
    local local_id = locl:get_id()
    local conversion_cfg = self.node_id_cfgs[local_id]

    -- let x: *mut T = 0 as *mut T; -> let mut x = None;
    -- or let mut x;
    if conversion_cfg then
        if conversion_cfg:is_opt_any() then
            local init = locl:get_init()

            if is_null_ptr(init) then
                init:to_ident_path("None")

                locl:set_ty(nil)
                locl:set_init(init)

                local arg_pat_hrid = self.tctx:get_nodeid_hrid(locl:get_pat_id())

                self.vars[arg_pat_hrid] = Variable.new(local_id, true)
            end
        elseif conversion_cfg:is_box_slice() then
            local init = locl:get_init()

            if is_null_ptr(init) then
                locl:set_ty(nil)
                locl:set_init(nil)

                local arg_pat_hrid = self.tctx:get_nodeid_hrid(locl:get_pat_id())

                self.vars[arg_pat_hrid] = Variable.new(local_id, true)
            end
        end
    end
end

refactor:transform(
    function(transform_ctx)
        local node_id_cfgs = {
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
            [221] = RefCfg.new("ref", nil),
            [229] = RefCfg.new("box_slice", nil),
            [288] = RefCfg.new("ref", nil),
        }
        return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_id_cfgs))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
