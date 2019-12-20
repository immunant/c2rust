require "pl"

-- Take a set of node ids (locals/params/fields) and turn them (if a pointer)
-- into a reference or Box
Variable = {}

function Variable.new(node_id, kind)
    local self = {}

    self.id = node_id
    self.kind = kind
    self.shadowed = false

    setmetatable(self, Variable)
    Variable.__index = Variable

    return self
end

Field = {}

function Field.new(node_id)
    local self = {}

    self.id = node_id

    setmetatable(self, Field)
    Field.__index = Field

    return self
end

function strip_int_suffix(expr)
    if expr:kind_name() == "Lit" then
        local lit = expr:get_node()

        if lit then
            lit:strip_suffix()
            expr:to_lit(lit)
        end
    end

    return expr
end

Struct = {}

function Struct.new(lifetimes, is_copy)
    local self = {}

    self.lifetimes = lifetimes
    self.is_copy = is_copy

    setmetatable(self, Struct)
    Struct.__index = Struct

    return self
end

Fn = {}

function Fn.new(node_id, is_foreign, arg_ids)
    local self = {}

    self.id = node_id
    self.is_foreign = is_foreign
    self.arg_ids = arg_ids

    setmetatable(self, Fn)
    Fn.__index = Fn

    return self
end

ConvConfig = {}

function ConvConfig.new(args)
    local self = {}

    self.conv_type = args[1]

    for i, arg in ipairs(args) do
        args[i] = args[i + 1]
    end

    self.extra_data = args

    setmetatable(self, ConvConfig)
    ConvConfig.__index = ConvConfig

    return self
end

function ConvConfig:is_mut()
    if self.extra_data.mutability == nil then
        return nil
    end

    return self.extra_data.mutability == "mut"
end

function ConvConfig.from_marks_and_attrs(marks, attrs)
    local opt = true
    local slice = false
    local mutability = nil
    local binding = nil
    local conv_type = ""
    local mut = marks["mut"]
    local ref = marks["ref"]
    local move = marks["move"]
    local box = marks["box"]

    for _, attr in ipairs(attrs) do
        local attr_ident = attr:ident():get_name()

        if attr_ident == "nonnull" then
            opt = false
        elseif attr_ident == "slice" then
            slice = true
        end
    end

    -- TODO: And technically move is mutually exclusive too
    if ref and mut then
        log_error("Found both ref and mut marks on a single type")
        return
    end

    if opt then
        conv_type = "opt_"
    end

    -- Box and Move are not identical, but have overlap
    if box or move then
        conv_type = conv_type .. "box"

        if slice then
            conv_type = conv_type .. "_slice"
        end

        -- REVIEW: If a ptr is box or move, does it ever make sense
        -- for it to be immut?
        mutability = "mut"
    elseif ref then
        mutability = "immut"

        if slice then
            conv_type = conv_type .. "slice"
        else
            conv_type = conv_type .. "ref"
        end
    elseif mut then
        mutability = "mut"
        binding = "ByValMut"

        if slice then
            conv_type = conv_type .. "slice"
        else
            conv_type = conv_type .. "ref"
        end
    end

    if conv_type == "" or stringx.endswith(conv_type, "_") then
        log_error("Could not build appropriate conversion cfg from: " .. pretty.write(marks))
        return
    end

    return ConvConfig.new{conv_type, mutability=mutability, binding=binding}
end

function ConvConfig:failed_rewrite()
    return self.extra_data.failed_rewrite
end

function ConvConfig:non_null_wrapped()
    return self.extra_data.non_null_wrapped
end

function ConvConfig:is_slice()
    return self.conv_type == "slice"
end

function ConvConfig:is_ref()
    return self.conv_type == "ref"
end

function ConvConfig:is_ref_any()
    return self:is_ref() or self:is_opt_ref()
end

function ConvConfig:is_ref_or_slice()
    return self:is_ref() or self:is_slice()
end

function ConvConfig:is_opt_ref()
    return self.conv_type == "opt_ref"
end

function ConvConfig:is_opt_slice()
    return self.conv_type == "opt_slice"
end

function ConvConfig:is_opt_any()
    return self:is_opt_box_any() or self:is_opt_ref() or self:is_opt_slice()
end

function ConvConfig:is_opt_box()
    return self.conv_type == "opt_box"
end

function ConvConfig:is_opt_box_slice()
    return self.conv_type == "opt_box_slice"
end

function ConvConfig:is_opt_box_any()
    return self:is_opt_box() or self:is_opt_box_slice()
end

function ConvConfig:is_slice_any()
    return self:is_slice() or self:is_opt_box_slice() or self:is_box_slice() or self:is_opt_slice()
end

function ConvConfig:is_box_slice()
    return self.conv_type == "box_slice"
end

function ConvConfig:is_box()
    return self.conv_type == "box"
end

function ConvConfig:is_box_any()
    return self:is_opt_box_any() or self:is_box_slice() or self:is_box()
end

function ConvConfig:is_del()
    return self.conv_type == "del"
end

function ConvConfig:is_byteswap()
    return self.conv_type == "byteswap"
end

function ConvConfig:is_local_mut_slice_offset()
    return self.conv_type == "local_mut_slice_offset"
end

function ConvConfig:is_array()
    return self.conv_type == "array"
end

Ty = {}

function Ty.new(kind)
    local self = {}

    self[1] = "Ty"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind

    setmetatable(self, Ty)
    Ty.__index = Ty

    return self
end

Expr = {}

function Expr.new(kind, attrs)
    local self = {}

    self[1] = "Expr"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind
    self.attrs = attrs or {}

    setmetatable(self, Expr)
    Expr.__index = Expr

    return self
end

Pat = {}

function Pat.new(kind)
    local self = {}

    self[1] = "Pat"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind

    setmetatable(self, Pat)
    Pat.__index = Pat

    return self
end

Local = {}

function Local.new(pat, ty, init, attrs)
    local self = {}

    self[1] = "Local"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.pat = pat
    self.ty = ty
    self.init = init
    self.attrs = attrs

    return self
end

Stmt = {}

function Stmt.new(kind)
    local self = {}

    self[1] = "Stmt"
    self.kind = kind

    return self
end

Ident = {}

function Ident.new(name)
    local self = {}

    self[1] = "Ident"
    self.name = name
    self.span = DUMMY_SP

    setmetatable(self, Ident)
    Ident.__index = Ident

    return self
end

PathSegment = {}

function PathSegment.new(ident, args)
    local self = {}

    self[1] = "PathSegment"
    self.ident = Ident.new(ident)
    self.args = args
    self.id = DUMMY_NODE_ID

    setmetatable(self, PathSegment)
    PathSegment.__index = PathSegment

    return self
end

Path = {}

function Path.new(segments)
    local self = {}
    local path_segments = {}

    for _, segment in ipairs(segments) do
        table.insert(path_segments, PathSegment.new(segment))
    end

    self[1] = "Path"
    self.span = DUMMY_SP
    self.segments = path_segments

    setmetatable(self, Path)
    Path.__index = Path

    return self
end

Visitor = {}

function Visitor.new(tctx, node_id_cfgs)
    local self = {}

    self.tctx = tctx
    -- NodeId -> ConvConfig
    self.node_id_cfgs = node_id_cfgs
    -- PatHirId [except statics] -> Variable
    self.vars = {}
    -- HirId -> Field
    self.fields = {}
    -- HirId -> Struct
    self.structs = {}
    -- HirId -> Fn
    self.fns = {}
    -- Expr NodeId -> Arg NodeId
    self.call_param_expr_to_arg_id = {}

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
end

function Visitor:get_param_cfg(fn, idx)
    if not fn then return end

    local arg_id = fn.arg_ids[idx]

    return self.node_id_cfgs[arg_id]
end

-- Takes a ptr type and returns the newly modified type
function upgrade_ptr(ptr_ty, conversion_cfg)
    local mut_ty = ptr_ty:get_mut_ty()
    local pointee_ty = mut_ty:get_ty()

    -- If we explicitly specify mutability, enforce its application
    -- otherwise we leave it as was (ie *const -> &, *mut -> &mut)
    if conversion_cfg.extra_data.mutability == "mut" then
        mut_ty:set_mutable(true)
    elseif conversion_cfg.extra_data.mutability == "immut" then
        mut_ty:set_mutable(false)
    end

    -- T -> [T]
    if conversion_cfg:is_slice_any() then
        pointee_ty:wrap_in_slice()
    end

    local non_boxed_slice = conversion_cfg:is_slice_any() and not conversion_cfg:is_box_any()

    -- T -> &T / &mut T or [T] -> &[T] / &mut [T]
    if conversion_cfg:is_ref_any() or non_boxed_slice then
        mut_ty:set_ty(pointee_ty)
        pointee_ty:to_rptr(conversion_cfg.extra_data.lifetime, mut_ty)

        if not conversion_cfg:is_box_any() and not conversion_cfg:is_opt_any() then
            return pointee_ty
        end
    end

    -- T -> Box<T> or [T] -> Box<[T]>
    if conversion_cfg:is_box_any() then
        pointee_ty:wrap_as_generic_angle_arg("Box")
    end

    -- Box<T> -> Option<Box<T>> or Box<[T]> -> Option<Box<[T]>>
    if conversion_cfg:is_opt_any() then
        pointee_ty:wrap_as_generic_angle_arg("Option")
    end

    return pointee_ty
end

function Visitor:flat_map_param(param)
    local param_id = param:get_id()
    local conv_config = self.node_id_cfgs[param_id]

    if not conv_config then return {param} end

    local param_ty = param:get_ty()

    if conv_config.extra_data.binding then
        param:set_binding(conv_config.extra_data.binding)
    end

    if param_ty:kind_name() == "Ptr" then
        local param_pat_hrid = self.tctx:nodeid_to_hirid(param:get_pat_id())

        self:add_var(param_pat_hrid, Variable.new(param_id, "param"))

        param:set_ty(upgrade_ptr(param_ty, conv_config))
    end

    return {param}
end

function Visitor:add_var(hirid, var)
    if hirid then
        local hirid_str = tostring(hirid)

        self.vars[hirid_str] = var
    end
end

function Visitor:get_var(hirid)
    local hirid_str = tostring(hirid)

    return self.vars[hirid_str]
end

function Visitor:add_fn(hirid, fn)
    if hirid then
        local hirid_str = tostring(hirid)

        self.fns[hirid_str] = fn
    end
end

function Visitor:get_fn(hirid)
    local hirid_str = tostring(hirid)

    return self.fns[hirid_str]
end

function Visitor:add_field(hirid, field)
    if hirid then
        local hirid_str = tostring(hirid)

        self.fields[hirid_str] = field
    end
end

function Visitor:get_field(hirid)
    local hirid_str = tostring(hirid)

    return self.fields[hirid_str]
end

function Visitor:add_struct(hirid, struct)
    if hirid then
        local hirid_str = tostring(hirid)

        self.structs[hirid_str] = struct
    end
end

function Visitor:get_struct(hirid)
    local hirid_str = tostring(hirid)

    return self.structs[hirid_str]
end

function Visitor:visit_expr(expr, walk)
    local expr_kind = expr:kind_name()

    if expr_kind == "Field" then
        self:rewrite_field_expr(expr)
    elseif expr_kind == "Unary" and expr:get_op() == "Deref" then
        self:rewrite_deref_expr(expr)
    elseif expr_kind == "Assign" then
        self:rewrite_assign_expr(expr)
    elseif expr_kind == "Call" then
        self:rewrite_call_expr(expr)
    elseif expr_kind == "MethodCall" then
        self:rewrite_method_call_expr(expr)
    elseif expr_kind == "Binary" then
        self:rewrite_binary_expr(expr)
    end

    walk(expr)
end

function Visitor:rewrite_binary_expr(expr)
    local lhs = expr:child(2)
    local rhs = expr:child(3)
    local lhs_config = self:get_expr_cfg(lhs)
    local rhs_config = self:get_expr_cfg(rhs)

    -- If lhs is rewritten but rhs isn't, decay lhs to ptr
    if lhs_config and not rhs_config then
        lhs = decay_ref_to_ptr(lhs, lhs_config)
        expr:replace_child(2, lhs)
    -- If rhs is rewritten but lhs isn't, decay rhs to ptr
    elseif rhs_config and not lhs_config then
        rhs = decay_ref_to_ptr(rhs, rhs_config)
        expr:replace_child(3, rhs)
    end
end

function Visitor:rewrite_method_call_expr(expr)
    local exprs = expr:get_exprs()
    local method_name = expr:get_method_name()

    -- x.offset(y) -> &x[y..] or Some(&x.unwrap()[y..])
    -- or -> Some(x.unwrap().split_at_mut(y).1)
    -- Only really works for positive pointer offsets
    if method_name == "offset" then
        local offset_expr, caller = rewrite_chained_offsets(expr)
        local cfg = self:get_expr_cfg(caller)

        if not cfg or cfg:failed_rewrite() then return end

        -- TODO: Refactor this into the below check?
        if cfg:non_null_wrapped() then
            local path_segment = caller:child(1)
            local ident = path_segment and tostring(path_segment:get_ident())

            if ident == "as_ptr#0" then return end

            expr:to_method_call("unwrap", {caller})
            expr:to_method_call("as_ptr", {expr})
            expr:to_method_call("offset", {expr, offset_expr})

            return
        end

        -- Add an unwrap just so that the code is compilable even though
        -- we're probably dealing with an option of a raw ptr (ie maybe
        -- a multi level ptr got deref'd once)
        if not cfg:is_slice_any() and cfg:is_opt_any() then
            expr:to_method_call("unwrap", {caller})
            expr:to_method_call("offset", {expr, offset_expr})

            return
        end

        local is_mut = cfg:is_mut()

        if offset_expr:kind_name() == "Cast" then
            local usize_ty = Ty.new{"Path", nil, Path.new{"usize"}}
            local inner_expr = offset_expr:get_exprs()[1]
            offset_expr = self.tctx:cast_expr(inner_expr, usize_ty)
        end

        if not is_mut then
            offset_expr:to_range(offset_expr, nil)
        end

        if cfg:is_opt_any() and not cfg:non_null_wrapped() then
            caller:to_method_call("unwrap", {caller})

            if is_mut then
                caller:to_method_call("split_at_mut", {caller, offset_expr})
            end
        end

        if not cfg:non_null_wrapped() then
            if not is_mut then
                expr:to_index(caller, offset_expr)
                expr:to_addr_of(expr, is_mut)
            else
                expr:to_field(caller, "1")
            end
        end
    -- var.as_mut/ptr -> &[mut]var
    elseif method_name == "as_ptr" or method_name == "as_mut_ptr" then
        local hirid = self.tctx:resolve_path_hirid(exprs[1])
        local config = self:get_expr_cfg(exprs[1])
        local arg_id = self.call_param_expr_to_arg_id[expr:get_id()]
        local param_cfg = self.node_id_cfgs[arg_id]

        -- There's a special case where we don't want to rewrite this way
        -- when it needs to be decayed to a raw ptr. Here we check that either
        -- we have a config for the param `param_cfg`, meaning it's expected to
        -- not be raw, or if we don't have an `arg_id` then the special case
        -- isn't applicable by viture of not being a call param
        if config and config:is_array() and (param_cfg or not arg_id) then
            expr:to_addr_of(exprs[1], config:is_mut())
        end
    -- p.is_null() -> p.is_none() or false when not using an option
    elseif method_name == "is_null" then
        local callee = expr:get_exprs()[1]
        local config = self:get_expr_cfg(callee)

        if not config then
            return
        end

        if config:is_opt_any() then
            expr:set_method_name("is_none")
        else
            expr:to_bool_lit(false)
        end
    elseif method_name == "wrapping_offset_from" then
        local lhs_cfg = self:get_expr_cfg(exprs[1])
        local rhs_cfg = self:get_expr_cfg(exprs[2])

        if lhs_cfg then
            exprs[1] = decay_ref_to_ptr(exprs[1], lhs_cfg)
        end

        if rhs_cfg then
            exprs[2] = decay_ref_to_ptr(exprs[2], rhs_cfg)
        end

        expr:to_method_call(method_name, {exprs[1], exprs[2]})
    end
end

-- Extracts a raw pointer from a rewritten rust type
function decay_ref_to_ptr(expr, cfg, for_struct_field, ptr_ty)
    local is_mut

    -- If supplied with the ptr type, use its mutability
    -- otherwise the cfg is an ok, but less ideal, fallback
    if ptr_ty and not cfg:is_array() then
        is_mut = tostring(ptr_ty:child(1):get_mutbl()) == "Mutable"
    else
        is_mut = cfg:is_mut()
    end

    -- There are a couple cases:
    -- 1) Option<NonNull<T>> -> opt.map(|r| r.as_ptr()).unwrap_or(0 as *const/mut _)
    -- 2) Option<Box<T>> -> opt.map(|r| &[mut] **r as *const/mut _).unwrap_or(0 as *const/mut _)
    -- where T may be a slice or non slice. (For the slice case, .as_[mut_]ptr is used)
    if cfg:is_opt_any() then
        local closure_expr
        local mutbl = is_mut and "Mutable" or "Immutable"
        local as_ptr = is_mut and "as_mut_ptr" or "as_ptr"
        local unwrap_or_expr = Expr.new{
            "Cast",
            Expr.new{
                "Lit",
                {
                    "Lit",
                    token={"TokenLit", kind="Integer", symbol="0", suffix=nil},
                    kind={"Int", 0, "Unsuffixed"},
                    span=DUMMY_SP,
                }
            },
            Ty.new{"Ptr", {"MutTy", ty=Ty.new{"Infer"}, mutbl=mutbl}},
        }

        if cfg:is_slice_any() or cfg:non_null_wrapped() then
            -- core::ptr::NonNull doesn't have an as_mut_ptr
            if cfg:non_null_wrapped() then
                as_ptr = "as_ptr"
            end

            closure_expr = Expr.new{
                "MethodCall",
                PathSegment.new(as_ptr),
                {
                    Expr.new{
                        "Path",
                        nil,
                        Path.new{"r"},
                    }
                },
            }

            if not is_mut and cfg:non_null_wrapped() then
                closure_expr = Expr.new{
                    "Cast",
                    closure_expr,
                    Ty.new{"Ptr", {"MutTy", ty=Ty.new{"Infer"}, mutbl="Immutable"}},
                }
            end
        else
            closure_expr = Expr.new{
                "Cast",
                Expr.new{
                    "AddrOf",
                    "Ref",
                    mutbl,
                    Expr.new{
                        "Unary",
                        "Deref",
                        Expr.new{
                            "Unary",
                            "Deref",
                            Expr.new{
                                "Path",
                                nil,
                                Path.new{"r"},
                            }
                        }
                    },
                },
                Ty.new{"Ptr", {"MutTy", ty=Ty.new{"Infer"}, mutbl=mutbl}},
            }
        end

        local closure = Expr.new{
            "Closure",
            "Ref",
            "NotAsync",
            "Movable",
            {
                "FnDecl",
                inputs={
                    {
                        "Param",
                        attrs={},
                        ty=Ty.new{"Infer"},
                        pat=Pat.new{
                            "Ident",
                            {
                                "ByValue",
                                "Immutable",
                            },
                            Ident.new("r"),
                            nil,
                        },
                        is_placeholder=false,
                    },
                },
                output={"Default", DUMMY_SP},
            },
            closure_expr,
            DUMMY_SP,
        }

        if not cfg:non_null_wrapped() then
            if is_mut then
                expr:to_method_call("as_mut", {expr})
            else
                expr:to_method_call("as_ref", {expr})
            end
        end

        expr:to_method_call("map", {expr, closure})
        expr:to_method_call(
            "unwrap_or",
            {
                expr,
                unwrap_or_expr,
            }
        )

        return expr
    end

    if cfg:is_box_any() and not cfg:is_slice_any() then
        expr:to_unary("Deref", expr)
        expr:to_addr_of(expr, is_mut)
    elseif cfg:is_slice_any() then
        if is_mut then
            expr:to_method_call("as_mut_ptr", {expr})
        else
            expr:to_method_call("as_ptr", {expr})
        end
    -- If we're using the expr in a field ie (*e.as_mut().unwrap()).bar then
    -- we can skip the deref as rust will do it automatically
    elseif is_mut and cfg:is_opt_any() and not for_struct_field then
        expr:to_unary("Deref", expr)
    elseif cfg:non_null_wrapped() then
        expr:to_method_call("as_ptr", {expr})
    end

    return expr
end

function Visitor:rewrite_field_expr(expr)
    local field_expr = expr:get_exprs()[1]

    if field_expr:kind_name() == "Unary" and field_expr:get_op() == "Deref" then
        local derefed_expr = field_expr:get_exprs()[1]

        if derefed_expr:kind_name() == "Path" then
            local cfg = self:get_expr_cfg(derefed_expr)

            -- This is a path we're expecting to modify
            if not cfg or cfg:failed_rewrite() then return end

            -- (*foo).bar -> (foo.as_mut().unwrap()).bar
            if cfg:is_opt_any() then
                if cfg:is_mut() then
                    derefed_expr:to_method_call("as_mut", {derefed_expr})
                end

                derefed_expr:to_method_call("unwrap", {derefed_expr})

                if cfg:non_null_wrapped() then
                    derefed_expr:to_method_call("as_ptr", {derefed_expr})
                    derefed_expr:to_unary("Deref", derefed_expr)
                end
            end

            -- (*foo).bar -> (foo).bar (can't remove parens..)
            expr:set_exprs{derefed_expr}
        end
    end
end

function rewrite_chained_offsets(unwrapped_expr)
    local offset_expr = nil

    while true do
        local unwrapped_exprs = unwrapped_expr:get_exprs()
        unwrapped_expr = unwrapped_exprs[1]
        local method_name = unwrapped_expr:get_method_name()
        local param_expr = strip_int_suffix(unwrapped_exprs[2])

        -- Accumulate offset params
        if not offset_expr then
            offset_expr = param_expr
        else
            offset_expr:to_binary("Add", param_expr, offset_expr)
        end

        -- May start with conversion to pointer if an array
        if method_name == "as_mut_ptr" then
            local unwrapped_exprs = unwrapped_expr:get_exprs()
            unwrapped_expr = unwrapped_exprs[1]

            break
        elseif method_name ~= "offset" then
            break
        end
    end

    return offset_expr, unwrapped_expr
end

function Visitor:rewrite_deref_expr(expr, output_slice)
    local derefed_exprs = expr:get_exprs()
    local unwrapped_expr = derefed_exprs[1]
    local cfg

    -- *p.offset(x).offset(y) -> p[x + y] (pointer) or
    -- *p.as_mut_ptr().offset(x).offset(y) -> p[x + y] (array)
    if unwrapped_expr:get_method_name() == "offset" then
        local offset_expr, unwrapped_expr = rewrite_chained_offsets(unwrapped_expr)
        local method_name = unwrapped_expr:get_method_name()

        if method_name == "as_ptr" or method_name == "as_mut_ptr" then
            unwrapped_expr = unwrapped_expr:child(2)[1]
        end

        -- Should be left with a path or field, otherwise bail
        cfg = self:get_expr_cfg(unwrapped_expr)

        if not cfg then
            return
        end

        -- We only want to apply this operation if we're converting
        -- a pointer to an array/slice
        if cfg:is_slice_any() or cfg:is_array() then
            -- If we're using an option, we must unwrap (or map/match) using
            -- as_mut (or as_ref) to avoid a move:
            -- *ptr[1] -> *ptr.as_mut().unwrap()[1] otherwise we can just unwrap
            -- *ptr[1] -> *ptr.unwrap()[1]
            if cfg:is_opt_any() then
                if cfg:is_opt_box_any() or cfg:is_mut() then
                    unwrapped_expr:to_method_call("as_mut", {unwrapped_expr})
                end
                unwrapped_expr:to_method_call("unwrap", {unwrapped_expr})
            end
            -- Here we just need to insert "as_ptr" on the NonNull variable, and let it proceed as it was
        elseif cfg:non_null_wrapped() then
            unwrapped_expr:to_method_call("unwrap", {unwrapped_expr})
            unwrapped_expr:to_method_call("as_ptr", {unwrapped_expr})
            unwrapped_expr:to_method_call("offset", {unwrapped_expr, offset_expr})
            expr:to_unary("Deref", unwrapped_expr)

            return
        else
            log_error("Found offset method applied to a reference: " .. tostring(expr))
            return
        end

        -- A cast to isize may have been applied by translator for offset(x)
        -- We should convert it to usize for the index
        if offset_expr:kind_name() == "Cast" then
            local cast_expr = offset_expr:get_exprs()[1]
            local cast_ty = offset_expr:get_ty()

            if cast_ty:kind_name() == "Path" and cast_ty:get_path():get_segments()[1]:get_ident():get_name() == "isize" then
                cast_ty:to_simple_path("usize")

                offset_expr:set_ty(cast_ty)
            end
        end

        expr:to_index(unwrapped_expr, offset_expr)
    -- *ptr = 1 -> **ptr.as_mut().unwrap() = 1
    elseif unwrapped_expr:kind_name() == "Path" then
        cfg = self:get_expr_cfg(unwrapped_expr)

        if not cfg or cfg:failed_rewrite() then return end

        -- If we're using an option, we must unwrap
        -- Must get inner reference to mutate
        if cfg:is_opt_any() then
            local is_mut = cfg:is_mut()

            -- as_ref is not required for immutable refs since &T is Copy
            if is_mut or cfg:is_box_any() then
                unwrapped_expr:to_method_call("as_mut", {unwrapped_expr})
            end

            expr:to_method_call("unwrap", {unwrapped_expr})

            -- Slices need to be indexed at 0 to equate to a ptr deref
            -- *a -> a.unwrap()[0] but thin refs can just be deref'd.
            -- *a -> *a.unwrap()
            if cfg:is_slice_any() then
                local zero_expr = self.tctx:int_lit_expr(0, nil)
                expr:to_index(expr, zero_expr)
            else
                -- *ptr.as_ptr() = 1; where ptr is std::ptr::NonNull
                if cfg:non_null_wrapped() then
                    expr:to_method_call("as_ptr", {expr})
                end

                -- For immut refs we skip the superflous as_ref call,
                -- so we can also skip one of the corresponding derefs
                if is_mut or cfg:is_box_any() then
                    expr:to_unary("Deref", expr)
                end

                expr:to_unary("Deref", expr)
            end
        -- Slices need to be indexed at 0 to equate to a ptr deref
        -- *a -> a.unwrap()[0] but thin refs can just be deref'd.
        -- *a -> *a.unwrap()
        elseif cfg:is_slice_any() then
            local zero_expr = self.tctx:int_lit_expr(0, nil)
            expr:to_index(unwrapped_expr, zero_expr)
        end
    end

    -- May need to output a slice in certain circumstances,
    -- ie if the deref expr is a slice wrapped in an AddrOf
    if output_slice and cfg and cfg:is_slice_any() and expr:kind_name() == "Index" then
        local index = expr:child(2)

        index = Expr.new{"Range", index, nil, "HalfOpen"}

        expr:replace_child(2, index)
    end
end

function Visitor:rewrite_assign_expr(expr)
    local exprs = expr:get_exprs()
    local lhs = exprs[1]
    local rhs = exprs[2]
    local rhs_kind = rhs:kind_name()
    local hirid = self.tctx:resolve_path_hirid(lhs)
    local var = self:get_var(hirid)

    if rhs_kind == "Cast" then
        local cast_expr = rhs:get_exprs()[1]
        local cast_ty = rhs:get_ty()

        -- p = malloc(X) as *mut T -> p = Some(vec![0; X / size_of<T>].into_boxed_slice())
        -- or p = vec![0; X / size_of<T>].into_boxed_slice()
        if cast_ty:kind_name() == "Ptr" and cast_expr:kind_name() == "Call" then
            local call_exprs = cast_expr:get_exprs()
            local path_expr = call_exprs[1]
            local param_expr = call_exprs[2]
            local path = path_expr:get_path()
            local segment_idents = {}
            local conversion_cfg = var and self.node_id_cfgs[var.id]

            if path then
                segment_idents = tablex.map(function(x) return x:get_ident():get_name() end, path:get_segments())
            end

            -- In case malloc is called from another module check the last segment
            if conversion_cfg and segment_idents[#segment_idents] == "malloc" then
                local mut_ty = cast_ty:get_mut_ty()
                local pointee_ty = mut_ty:get_ty()
                local new_rhs = nil
                -- TODO: zero-init will only work for numbers, not structs/unions
                local init = self.tctx:int_lit_expr(0, nil)

                -- For slices we want to use vec![init; num].into_boxed_slice
                if conversion_cfg:is_slice_any() then
                    path:set_segments{"", "core", "mem", "size_of"}
                    path:set_generic_angled_arg_tys(4, {pointee_ty})
                    path_expr:to_path(path)
                    path_expr:to_call{path_expr}

                    local usize_ty = Ty.new{"Path", nil, Path.new{"usize"}}
                    local cast_expr = self.tctx:cast_expr(param_expr, usize_ty)
                    local binary_expr = self.tctx:binary_expr("Div", cast_expr, path_expr)

                    new_rhs = self.tctx:vec_mac_init_num(init, binary_expr)
                    new_rhs:to_method_call("into_boxed_slice", {new_rhs})
                -- For boxes we want Box::new(init)
                elseif conversion_cfg:is_box_any() then
                    path:set_segments{"Box", "new"}
                    path_expr:to_path(path)
                    path_expr:to_call{path_expr, init}

                    new_rhs = path_expr
                end

                -- Only wrap in Some if we're assigning to an opt variable
                if conversion_cfg:is_opt_any() then
                    local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}
                    rhs:to_call{some_path_expr, new_rhs}
                else
                    rhs = new_rhs
                end

                expr:set_exprs{lhs, rhs}
            end
        -- p = 0 as *mut/const T -> p = None
        elseif is_null_ptr(rhs) then
            local conversion_cfg = self:get_expr_cfg(lhs)

            if conversion_cfg and conversion_cfg:is_opt_any() then
                rhs:to_ident_path("None")
                expr:set_exprs{lhs, rhs}
            end
        end
    -- lhs = rhs -> lhs = Some(rhs)
    elseif rhs_kind == "Path" then
        local lhs_cfg = self:get_expr_cfg(lhs)
        local rhs_cfg = self:get_expr_cfg(rhs)

        if not rhs_cfg then return end

        if not rhs_cfg:is_opt_any() then
            local lhs_ty = self.tctx:get_expr_ty(lhs)

            -- If lhs was a ptr, and rhs isn't wrapped in some, wrap it
            -- TODO: Validate rhs needs to be wrapped
            if lhs_ty:kind_name() == "Ptr" then
                local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}

                rhs:to_call{some_path_expr, rhs}
                expr:set_exprs{lhs, rhs}
            end
        end

        if lhs_cfg and not lhs_cfg:non_null_wrapped() then
            lhs_cfg.extra_data.non_null_wrapped = rhs_cfg:non_null_wrapped()
        end
    else
        local lhs_cfg = self:get_expr_cfg(lhs)

        if not lhs_cfg then return end

        if lhs_cfg:is_opt_any() then
            if rhs_kind == "Call" then
                local path_expr = rhs:get_exprs()[1]
                local path = path_expr:get_path()
                local rhs_ty = self.tctx:get_expr_ty(rhs)
                local mut_ty = rhs_ty:child(1)

                path:set_segments{"", "core", "ptr", "NonNull", "new"}
                path_expr:to_path(path)

                -- NonNull takes a mut ptr, so we have to const -> mut cast..
                -- Not sure if there's a better way to handle this
                if tostring(mut_ty:get_mutbl()) == "Immutable" then
                    mut_ty:set_mutbl("Mutable")
                    rhs:to_cast(rhs, Ty.new{"Ptr", mut_ty})
                end

                rhs:to_call{path_expr, rhs}
                expr:set_exprs{lhs, rhs}

                lhs_cfg.extra_data.non_null_wrapped = true

                return
            end

            if lhs_cfg:non_null_wrapped() and rhs_kind ~= "Path" then
                local path = Path.new{"", "core", "ptr", "NonNull", "new"}
                local path_expr = Expr.new{"Path", nil, path}

                rhs = Expr.new{"Call", path_expr, {rhs}}
            else
                local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}
                rhs:to_call{some_path_expr, rhs}
            end

            expr:set_exprs{lhs, rhs}
        end
    end
end

function Visitor:rewrite_call_expr(expr)
    local call_exprs = expr:get_exprs()
    local path_expr = call_exprs[1]
    local first_param_expr = call_exprs[2]
    local path = path_expr:get_path()
    local segment_idents = path and tablex.map(function(x) return x:get_ident():get_name() end, path:get_segments())

    if not segment_idents then return end

    -- Bookkeeping of call param exprs to arg_id
    local fn

    for i, param_expr in ipairs(expr:get_exprs()) do
        if i == 1 then
            local hirid = self.tctx:resolve_path_hirid(param_expr)

            fn = self:get_fn(hirid)
        elseif fn then
            self.call_param_expr_to_arg_id[param_expr:get_id()] = fn.arg_ids[i - 1]
        end
    end

    -- free(foo.bar as *mut libc::c_void) -> foo.bar.take()
    -- In case free is called from another module check the last segment
    if segment_idents[#segment_idents] == "free" then
        local uncasted_expr = first_param_expr

        -- REVIEW: What if there's a multi-layered cast?
        if first_param_expr:kind_name() == "Cast" then
            uncasted_expr = first_param_expr:get_exprs()[1]
        end

        local cast_ty = first_param_expr:get_ty()
        local cfg = self:get_expr_cfg(uncasted_expr)

        if cfg then
            if cfg:is_opt_any() then
                expr:to_method_call("take", {uncasted_expr})

                -- If it's not also boxed, then we probably have an inner raw ptr
                -- and should still call free on it
                if not cfg:is_box_any() then
                    uncasted_expr = decay_ref_to_ptr(expr, cfg)
                    uncasted_expr:to_cast(uncasted_expr, cast_ty)
                    expr:to_call{path_expr, uncasted_expr}
                end
            elseif cfg:is_box_any() then
                local drop = Expr.new{"Path", nil, Path.new{"drop"}}
                expr:to_call{drop, uncasted_expr}
            end
        end
    -- Skip; handled elsewhere by local conversion
    elseif segment_idents[#segment_idents] == "malloc" then
    -- Generic function call param conversions
    -- NOTE: Some(x) counts as a function call on x, so we skip Some
    -- so as to not recurse when we generate that expr
    elseif segment_idents[#segment_idents] ~= "Some" then
        local hirid = self.tctx:resolve_path_hirid(path_expr)
        local fn = self:get_fn(hirid)

        if not fn then return end

        for i, param_expr in ipairs(call_exprs) do
            -- Skip function name path expr
            if i == 1 then goto continue end

            local param_cfg = self:get_param_cfg(fn, i - 1)
            local param_kind = param_expr:kind_name()

            -- array.as_ptr/as_mut_ptr() -> &array/&mut array
            -- &array/&mut array -> Option<&array/&mut array>
            if param_cfg and param_kind == "MethodCall" then
                local exprs = param_expr:get_exprs()
                local path_expr = exprs[1]

                if #exprs == 1 and path_expr:kind_name() == "Path" then
                    local method_name = param_expr:get_method_name()
                    local path_cfg = self:get_expr_cfg(path_expr)

                    -- If we're looking at an array then we likely don't want
                    -- a reference to the array type but a raw pointer
                    if method_name == "as_ptr" and path_cfg then
                        param_expr:to_addr_of(path_expr, param_cfg:is_mut())
                    elseif method_name == "as_mut_ptr" and path_cfg then
                        param_expr:to_addr_of(path_expr, param_cfg:is_mut())
                    end

                    if param_cfg:is_opt_any() then
                        local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}
                        param_expr:to_call{some_path_expr, param_expr}
                    end

                    goto continue
                end
            end

            if param_cfg and param_cfg:is_opt_any() then
                -- 0 as *const/mut T -> None
                if is_null_ptr(param_expr) then
                    param_expr:to_ident_path("None")
                    goto continue
                -- &T -> Some(&T)
                elseif param_kind == "AddrOf" then
                    local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}
                    local kind = param_expr:get_kind()
                    local mutbl = param_cfg:is_mut() and "Mutable" or "Immutable"
                    local target = kind:child(3)

                    if target:kind_name() == "Unary" and tostring(target:child(1)) == "Deref" then
                        self:rewrite_deref_expr(target, true)
                        kind:replace_child(3, target)
                    end

                    kind:replace_child(2, mutbl)
                    param_expr:set_kind(kind)
                    param_expr:to_call{some_path_expr, param_expr}

                    goto continue
                elseif param_kind == "Path" then
                    local path_cfg = self:get_expr_cfg(param_expr)

                    if path_cfg then
                        -- path -> Some(path)
                        if not path_cfg:is_opt_any() then
                            local some_path_expr = Expr.new{"Path", nil, Path.new{"Some"}}
                            param_expr:to_call{some_path_expr, param_expr}
                            goto continue
                        -- Decay mut ref to immut ref inside option
                        elseif path_cfg:is_box_any() and not param_cfg:is_box_any() and
                            not path_cfg:is_opt_any() and not param_cfg:is_opt_any() then

                            param_expr = decay_ref_to_ptr(param_expr, param_cfg)
                            goto continue
                        -- foo(x) -> foo(x.as_ref().map(|r| &**r)) iff both path and param
                        -- aren't boxes (REVIEW: maybe should check that they're opt too?)
                        elseif not (path_cfg:is_box_any() and param_cfg:is_box_any()) then
                            if path_cfg:non_null_wrapped() then
                                log_error("Found unsupported NonNull to safe type conversion in call param")

                                goto continue
                            end

                            local mutbl = param_cfg:is_mut() and "Mutable" or "Immutable"
                            local fn_decl = {
                                "FnDecl",
                                inputs={
                                    {
                                        "Param",
                                        attrs={},
                                        ty=Ty.new{"Infer"},
                                        pat=Pat.new{
                                            "Ident",
                                            {
                                                "ByValue",
                                                "Immutable",
                                            },
                                            Ident.new("r"),
                                            nil,
                                        },
                                        is_placeholder=false,
                                    },
                                },
                                output={"Default", DUMMY_SP},
                            }
                            local var_expr = Expr.new{"Path", nil, Path.new{"r"}}

                            var_expr = Expr.new{"Unary", "Deref", var_expr}
                            var_expr = Expr.new{"Unary", "Deref", var_expr}
                            var_expr = Expr.new{"AddrOf", "Ref", mutbl, var_expr}
                            var_expr = Expr.new{"Closure", "Ref", "NotAsync", "Movable", fn_decl, var_expr}

                            if path_cfg:is_mut() then
                                if not param_cfg:is_mut() then
                                    param_expr:to_method_call("as_ref", {param_expr})
                                else
                                    param_expr:to_method_call("as_mut", {param_expr})
                                end
                            else
                                param_expr:to_method_call("as_ref", {param_expr})
                            end

                            param_expr:to_method_call("map", {param_expr, var_expr})
                            goto continue
                        end
                    end
                end
            end

            -- Avoid nested call exprs
            if param_kind == "Call" then
                goto continue
            end

            --  x -> x[.as_mut()].unwrap().as_[mut_]ptr()
            param_expr:filtermap_subexprs(
                function(expr_kind) return expr_kind == "Unary" or expr_kind == "Path" end,
                function(expr)
                    -- Deref exprs should already be handled by rewrite_deref_expr
                    -- so we should skip over them (maybe only if derefing path?)
                    if expr:get_op() == "Deref" then
                        return expr
                    end

                    local path_cfg = self:get_expr_cfg(expr)

                    if not path_cfg then
                        return expr
                    end

                    -- If we have a config for this path expr, we assume it has been rewritten
                    -- and if we don't have a config for the param itself, we assume it has not
                    -- been rewritten, meaning we must decay it. Foreign functions are a common
                    -- case for not having a param_cfg.
                    if not param_cfg then
                        local ptr_ty = self.tctx:get_expr_ty(expr)
                        expr = decay_ref_to_ptr(expr, path_cfg, false, ptr_ty)
                    end

                    return expr
                end
            )

            ::continue::
        end

        expr:set_exprs(call_exprs)
    end
end

function Visitor:get_expr_cfg(expr)
    local hirid = self.tctx:resolve_path_hirid(expr)
    local node_id = nil
    local var = self:get_var(hirid)

    -- If we're looking at a local or param, lookup from the variable map
    if var then
        node_id = var.id
    -- Otherwise check the field map
    elseif expr:kind_name() == "Field" then
        hirid = self.tctx:get_field_expr_hirid(expr)
        local field = self:get_field(hirid)

        if field then
            node_id = field.id
        end
    end

    return self.node_id_cfgs[node_id]
end

-- HrIds may be reused in different functions, so we should clear them out
-- so we don't accidentally access old info
-- NOTE: If this script encounters any nested functions, this will reset variables
-- prematurely. We should push and pop a stack of variable scopes to account for this
function Visitor:clear_nonstatic_vars()
    local static_vars = {}

    for hirid, var in pairs(self.vars) do
        if var.kind == "static" then
            static_vars[hirid] = var
        end
    end

    self.vars = static_vars
end

function Visitor:flat_map_item(item, walk)
    local item_kind = item:kind_name()

    if item_kind == "Struct" then
        local lifetimes = OrderedMap()
        local fields = item:get_fields()
        local is_copy = true

        for _, field in ipairs(fields) do
            local field_id = field:get_id()
            local cfg = self.node_id_cfgs[field_id]
            local field_hrid = self.tctx:nodeid_to_hirid(field_id)

            self:add_field(field_hrid, Field.new(field_id))

            if cfg then
                if cfg:is_box_any() then
                    is_copy = false
                end

                if cfg.extra_data.lifetime then
                    item:add_lifetime(cfg.extra_data.lifetime)

                    lifetimes[cfg.extra_data.lifetime] = true
                end
            end
        end

        if not is_copy then
            item:remove_copy_derive()
        end

        local hirid = self.tctx:nodeid_to_hirid(item:get_id())

        self:add_struct(hirid, Struct.new(lifetimes, is_copy))
    elseif item_kind == "Fn" then
        self:clear_nonstatic_vars()

        local args = item:get_args()
        local arg_ids = {}

        for i, arg in ipairs(args) do
            local arg_id = arg:get_id()
            local ref_cfg = self.node_id_cfgs[arg_id]

            table.insert(arg_ids, arg_id)

            if ref_cfg and ref_cfg.extra_data.lifetime then
                item:add_lifetime(ref_cfg.extra_data.lifetime)
            end

            local arg_ty = arg:get_ty()

            -- Grab lifetimes from the argument type
            -- REVIEW: Maybe this shouldn't map but just traverse?
            arg_ty:map_ptr_root(function(path_ty)
                if path_ty:kind_name() ~= "Path" then
                    return path_ty
                end

                local hirid = self.tctx:resolve_ty_hirid(path_ty)
                local struct = self:get_struct(hirid)

                if struct then
                    for lifetime in struct.lifetimes:iter() do
                        path_ty:add_lifetime(lifetime)
                        item:add_lifetime(lifetime)
                    end
                end

                return path_ty
            end)

            arg:set_ty(arg_ty)

            -- TODO: Possibly move visit_arg into here?
        end

        item:set_args(args)

        local fn_id = item:get_id()
        local hirid = self.tctx:nodeid_to_hirid(fn_id)

        self:add_fn(hirid, Fn.new(fn_id, false, arg_ids))
    elseif item_kind == "Static" then
        local hirid = self.tctx:nodeid_to_hirid(item:get_id())

        self:add_var(hirid, Variable.new(item:get_id(), "static"))
    -- elseif item_kind == "Impl" then
    --     local seg = item:get_trait_ref():get_segments()
    --     print(seg[#seg])

    --     if seg == "Copy" then
    --         return {}
    --     end
    end

    walk(item)

    return {item}
end

function Visitor:flat_map_foreign_item(foreign_item)
    if foreign_item:kind_name() == "Fn" then
        local fn_id = foreign_item:get_id()
        local hirid = self.tctx:nodeid_to_hirid(fn_id)
        local arg_ids = {}

        for i, arg in ipairs(foreign_item:get_args()) do
            local arg_id = arg:get_id()

            table.insert(arg_ids, arg_id)
        end

        self:add_fn(hirid, Fn.new(fn_id, true, arg_ids))
    end

    return {foreign_item}
end

function Visitor:flat_map_stmt(stmt, walk)
    local cfg = self.node_id_cfgs[stmt:get_id()]

    if not cfg then
        walk(stmt)
        return {stmt}
    end

    -- A stmt may be marked for deletion
    if cfg:is_del() then
        return {}
    -- Here we look for a particular multi-stmt pattern:
    --
    -- let fresh = a;
    -- a = a.offset(x);
    --
    -- where "a" is a mutable slice ref. In particular, we're just looking for the
    -- offset assignment here (locals handeled elsewhere). We rewrite it to:
    --
    -- {
    --     let tup = a[.unwrap()].split_at_mut(x);
    --     fresh = [Some(]tup.0[)];
    --     a = Some(tup.1);
    -- }
    --
    -- We rewrite the new stmts into a new block to try and avoid "tup" from possibly
    -- shadowing a prior variable. Rust doesn't yet have great tuple support, so this has
    -- to be done over multiple lines. Ideally you could do something like in python:
    -- (fresh, a) = a.unwrap().split_at_mut(x) but this doesn't work today.
    --
    -- This rewrite is only necessary when "a" is a mutable slice since they are not Copy.
    -- A caveat is that this doesn't work with negative offsets and even with a positive
    -- offset it may not be the correct usage 100% of the time but seems to work >99% of
    -- the time and avoids a borrowing error that would otherwise occur.
    elseif cfg:is_local_mut_slice_offset() then
        local stmt_kind = stmt:kind_name()

        if stmt_kind == "Semi" then
            local expr = stmt:get_node()

            if expr:kind_name() == "Assign" then
                local exprs = expr:get_exprs()
                local new_lhs = Expr.new{"Path", nil, Path.new{cfg.extra_data[1]}}
                local tup0 = Expr.new{"Path", nil, Path.new{"tup"}}
                local tup1 = Expr.new{"Path", nil, Path.new{"tup"}}
                local locl_cfg = self.node_id_cfgs[cfg.extra_data[2]]
                local offset_expr, _ = rewrite_chained_offsets(exprs[2])
                local offset_caller_cfg = self:get_expr_cfg(exprs[1])
                local init = nil

                if offset_caller_cfg:is_opt_any() then
                    init = self.tctx:method_call_expr("unwrap", {exprs[1]})
                end

                init = self.tctx:method_call_expr("split_at_mut", {init or exprs[1], offset_expr})

                local pat = Pat.new{
                    "Ident",
                    {
                        "ByValue",
                        "Immutable",
                    },
                    Ident.new("tup"),
                    nil,
                }
                local locl = Local.new(pat, nil, init)

                tup0 = Expr.new{"Field", tup0, Ident.new("0")}
                tup1 = Expr.new{"Field", tup1, Ident.new("1")}

                if locl_cfg:is_opt_any() then
                    tup0 = Expr.new{
                        "Call",
                        Expr.new{"Path", nil, Path.new{"Some"}},
                        {tup0},
                    }
                end

                local assign_expr = self.tctx:assign_expr(new_lhs, tup0)
                local assign_expr2 = self.tctx:assign_expr(exprs[1], tup1)
                local stmts = {
                    Stmt.new{"Local", locl},
                    Stmt.new{"Semi", assign_expr},
                    Stmt.new{"Semi", assign_expr2},
                }

                expr:to_block(stmts, nil, true)
                stmt:to_expr(expr, false)
            end
        end
    elseif cfg:is_byteswap() and stmt:kind_name() == "Semi" then
        local expr = stmt:get_node()
        local lhs_id = cfg.extra_data[1]
        local rhs_id = cfg.extra_data[2]
        local lhs = expr:find_subexpr(lhs_id)
        local rhs = expr:find_subexpr(rhs_id)

        if lhs and rhs then
            rhs:to_method_call("swap_bytes", {rhs})

            local assign_expr = self.tctx:assign_expr(lhs, rhs)

            stmt:to_expr(assign_expr, true)
        end
    end

    walk(stmt)

    return {stmt}
end

function Visitor:flat_map_struct_field(field)
    local field_id = field:get_id()
    local field_ty = field:get_ty()
    local cfg = self.node_id_cfgs[field_id]

    if not cfg then return {field} end

    local field_ty_kind = field_ty:kind_name()

    -- *mut T -> Box<T>, or Box<[T]> or Option<Box<T>> or Option<Box<[T]>>
    if field_ty_kind == "Ptr" then
        field:set_ty(upgrade_ptr(field_ty, cfg))
    -- [*mut T; X] -> [Box<T>; X] or [Box<[T]>; X] or [Option<Box<T>>; X]
    -- or [Option<Box<[T]>; X]
    elseif field_ty_kind == "Array" then
        local inner_ty = field_ty:get_tys()[1]

        if inner_ty:kind_name() == "Ptr" then
            inner_ty = upgrade_ptr(inner_ty, cfg)

            field_ty:set_tys{inner_ty}
            field:set_ty(field_ty)
        end
    end

    return {field}
end

function is_null_ptr(expr)
    if expr and expr:kind_name() == "Cast" then
        local cast_expr = expr:get_exprs()[1]
        local cast_ty = expr:get_ty()

        if cast_expr:kind_name() == "Lit" then
            local lit = cast_expr:get_node()

            if lit and lit:get_value() == 0 and cast_ty:kind_name() == "Ptr" then
                return true
            end
        end
    end

    return false
end

function is_void_ptr(ty)
    if ty:kind_name() == "Ptr" then
        local mut_ty = ty:get_mut_ty()
        local pointee_ty = mut_ty:get_ty()
        local path = pointee_ty:get_path()

        if path then
            local segments = path:get_segments()

            if segments[#segments]:get_ident():get_name() == "c_void" then
                return true
            end
        end

        return is_void_ptr(pointee_ty)
    end

    return false
end

function Visitor:visit_local(locl, walk)
    local local_id = locl:get_id()
    local cfg = self.node_id_cfgs[local_id]

    if not cfg then
        walk(locl)
        return
    end

    local init = locl:get_init()

    if init:kind_name() == "Path" then
        local rhs_cfg = self:get_expr_cfg(init)

        if rhs_cfg then
            if cfg:is_opt_any() and not rhs_cfg:is_opt_any() then
                init = Expr.new{
                    "Call",
                    Expr.new{"Path", nil, Path.new{"Some"}},
                    {init},
                }

                locl:set_init(init)
            end

            locl:set_ty(nil)
        end
    -- let x: *mut T = 0 as *mut T; -> let mut x = None;
    -- or let mut x;
    elseif cfg:is_opt_any() and is_null_ptr(init) then
        init:to_ident_path("None")

        locl:set_ty(nil)
        locl:set_init(init)
    elseif is_null_ptr(init) then
        locl:set_ty(nil)
        locl:set_init(nil)
    -- Here we need an explicit type to coerce array ref to slice ref
    elseif init:get_method_name() == "as_mut_ptr" or init:get_method_name() == "as_ptr" then
        local caller = init:child(2)[1]
        local cfg = self:get_expr_cfg(caller)

        if cfg then
            local mut_ty = locl:get_ty():child(1)
            local slice = Ty.new{"Slice", mut_ty:get_ty()}
            local mutbl = "Immutable"

            if cfg:is_mut() then
                mutbl = "Mutable"
            end

            local slice_ref = Ty.new{"Rptr", nil, {"MutTy", ty=slice, mutbl=mutbl}}

            locl:set_ty(slice_ref)
        end
    end

    if cfg.extra_data.clear_init_and_ty then
        locl:set_ty(nil)
        locl:set_init(nil)
    end

    local ty = locl:get_ty()

    -- If we're not looking at a pointer (ie an array) and we haven't removed
    -- the explicit type, then we likely failed to rewrite the local (ie a pointer
    -- cast)
    if ty ~= nil and ty:kind_name() == "Ptr" then
        cfg.extra_data.failed_rewrite = true
        log_error("Failed to rewrite local: " .. tostring(locl))
    end

    local pat_hirid = self.tctx:nodeid_to_hirid(locl:get_pat_id())

    self:add_var(pat_hirid, Variable.new(local_id, "local"))
    walk(locl)
end

function is_empty(tbl)
    return next(tbl) == nil
end

-- ConfigBuilder is an AST visitor which creates conversion configs (ConvConfig).
-- This is done primarily based on ownership analysis markings, but also takes
-- in MallocMarker's malloc analysis "boxes" as supplemental info.
ConfigBuilder = {}

function ConfigBuilder.new(marks, boxes, tctx)
    local self = {}

    self.marks = marks
    self.node_id_cfgs = {}
    self.boxes = boxes
    self.tctx = tctx
    self.pat_to_var_id = {}

    setmetatable(self, ConfigBuilder)
    ConfigBuilder.__index = ConfigBuilder

    return self
end

function ConfigBuilder:visit_local(locl, walk)
    local ty = locl:get_ty()

    -- Locals with no type annotation are skipped
    if not ty then return end

    local ty_id = ty:get_id()
    local id = locl:get_id()
    local pat_hirid = self.tctx:nodeid_to_hirid(locl:get_pat_id())
    local marks = self.marks[ty_id] or {}
    local attrs = locl:get_attrs()

    if self.boxes[tostring(pat_hirid)] then
        marks["box"] = true
    end

    local pat_id = locl:get_pat():get_id()

    -- Skip if there are no marks
    if is_empty(marks) then
        -- However, it's still useful to build a basic cfg for arrays as we might
        -- take pointers/references into them
        if ty:kind_name() == "Array" then
            self.pat_to_var_id[pat_id] = id
            self.node_id_cfgs[id] = ConvConfig.new{"array"}
        end

        walk(locl)
        return
    end

    self.pat_to_var_id[pat_id] = id
    self.node_id_cfgs[id] = ConvConfig.from_marks_and_attrs(marks, attrs)
    walk(locl)
end

function ConfigBuilder:flat_map_item(item, walk)
    local item_kind = item:kind_name()
    local vis = item:get_vis():get_node():kind_name()
    local priv_or_crate_vis = vis == "Crate" or vis == "Inherited"

    if item_kind == "Struct" and priv_or_crate_vis then
        local fields = item:get_fields()

        for _, field in ipairs(fields) do
            local field_id = field:get_id()
            local field_ty = field:get_ty()
            local ty_id = field_ty:get_id()

            if field_ty:kind_name() == "Array" then
                ty_id = field_ty:get_tys()[1]:get_id()
            end

            local marks = self.marks[ty_id] or {}

            if not is_empty(marks) then
                self.node_id_cfgs[field_id] = ConvConfig.from_marks_and_attrs(marks, field:get_attrs())
            end
        end
    -- We don't visit params directly because then we could be looking at foreign params unknowingly
    -- which we don't want
    elseif item_kind == "Fn" then
        local fn_sig = item:child(1)
        local decl = fn_sig:get_decl()
        local params = decl:get_inputs()

        for _, param in ipairs(params) do
            local param_id = param:get_id()
            local param_ty = param:get_ty()
            local param_ty_id = param_ty:get_id()
            local marks = self.marks[param_ty_id] or {}

            -- Skip over pointers to void
            if is_void_ptr(param_ty) then
                goto continue
            end

            -- Skip if there are no marks
            if is_empty(marks) then
                goto continue
            end

            local attrs = param:get_attrs()

            self.pat_to_var_id[param:get_pat():get_id()] = param_id
            self.node_id_cfgs[param_id] = ConvConfig.from_marks_and_attrs(marks, attrs)

            ::continue::
        end
    elseif item_kind == "Static" then
        local ty = item:get_kind():child(1)

        if ty:kind_name() == "Array" then
            self.node_id_cfgs[item:get_id()] = ConvConfig.new{"array"}
        end
    end

    walk(item)

    return {item}
end

function path_to_last_segment(path)
    if not path then return end
    local segments = path:get_segments()
    return segments[#segments]
end

function ConfigBuilder:flat_map_stmt(stmt, walk)
    local stmt_kind = stmt:kind_name()

    -- Here we look for a particular multi-stmt pattern:
    -- let fresh = a;
    -- a = a.offset(x);
    -- where a is a mutable slice
    if stmt_kind == "Local" then
        local locl = stmt:get_node()
        local init = locl:get_init()

        if init and init:kind_name() == "Path" then
            -- Ideally we'd just check if the marking for this local's ty id
            -- is mut. However, the variable may possibly be marked as immutable
            -- if it is only read from (despite containing a mutable ref)
            -- so instead we look up the mutability from the config of the rhs.
            -- This may be the same issue as GH #163
            local hir_id = self.tctx:resolve_path_hirid(init)
            local node_pat_id
            if hir_id then
                node_pat_id = self.tctx:hirid_to_nodeid(hir_id)
            end
            local node_id = self.pat_to_var_id[node_pat_id]
            local init_cfg = self.node_id_cfgs[node_id]

            if init_cfg and init_cfg:is_mut() and init_cfg:is_slice_any() then
                local pat = locl:get_pat()

                self.lhs_ident = pat:get_ident():get_name()
                self.rhs_ident = path_to_last_segment(init:get_path()):get_ident():get_name()
                self.local_stmt_id = stmt:get_id()
                self.local_id = locl:get_id()

                walk(stmt)
                return {stmt}
            end
        end
    elseif stmt_kind == "Semi" and self.local_stmt_id then
        local expr = stmt:get_node()
        local exprs = expr:get_exprs()

        if expr:kind_name() == "Assign" and exprs[1]:kind_name() == "Path" then
            local lhs = exprs[1]
            local rhs = exprs[2]
            local lhs_path = path_to_last_segment(lhs:get_path()):get_ident():get_name()
            local offset_expr, caller = rewrite_chained_offsets(rhs)
            local caller_path = path_to_last_segment(caller:get_path()):get_ident():get_name()
            local local_cfg = self.node_id_cfgs[self.local_id]

            -- Work around for https://github.com/immunant/c2rust/issues/198
            if not local_cfg then
                walk(stmt)
                return {stmt}
            end

            local_cfg.extra_data.clear_init_and_ty = true

            if lhs_path == self.rhs_ident and caller_path == self.rhs_ident then
                self.node_id_cfgs[self.local_stmt_id] = ConvConfig.new{"local_mut_slice_offset"}
                self.node_id_cfgs[stmt:get_id()] = ConvConfig.new{"local_mut_slice_offset", self.lhs_ident, self.local_id}
            end
        end
    end

    -- Clear
    self.lhs_ident = nil
    self.rhs_ident = nil
    self.local_stmt_id = nil
    self.local_id = nil

    walk(stmt)
    return {stmt}
end

-- This visitor finds variables that are assigned
-- a malloc or calloc and marks them as "box"
MallocMarker = {}

function MallocMarker.new(tctx)
    local self = {}

    self.tctx = tctx
    self.boxes = {}

    setmetatable(self, MallocMarker)
    MallocMarker.__index = MallocMarker

    return self
end

function MallocMarker:visit_expr(expr, walk)
    local expr_kind = expr:kind_name()

    -- Mark types as "box" for malloc/calloc
    if expr_kind == "Assign" then
        local exprs = expr:get_exprs()
        local lhs = exprs[1]
        local rhs = exprs[2]
        local rhs_kind = rhs:kind_name()
        local hirid = self.tctx:resolve_path_hirid(lhs)

        if rhs_kind == "Cast" then
            local cast_expr = rhs:get_exprs()[1]
            local cast_ty = rhs:get_ty()

            if cast_ty:kind_name() == "Ptr" and cast_expr:kind_name() == "Call" then
                local call_exprs = cast_expr:get_exprs()
                local path_expr = call_exprs[1]
                local path = path_expr:get_path()
                local segment_idents = {}

                if path then
                    segment_idents = tablex.map(function(x) return x:get_ident():get_name() end, path:get_segments())
                end

                -- In case malloc is called from another module check the last segment
                if segment_idents[#segment_idents] == "malloc" or segment_idents[#segment_idents] == "calloc" then
                    -- TODO: Non path support. IE Field
                    self.boxes[tostring(hirid)] = true
                end
            end
        end
    end

    walk(expr)
    return {arg}
end

function infer_node_id_configs(tctx)
    local marks = tctx:get_marks()
    local malloc_marker = MallocMarker.new(tctx)

    tctx:visit_crate_new(malloc_marker)

    local converter = ConfigBuilder.new(marks, malloc_marker.boxes, tctx)
    tctx:visit_crate_new(converter)
    return converter.node_id_cfgs
end

function run_ptr_upgrades(node_id_cfgs)
    if not node_id_cfgs then
        refactor:run_command("select", {"target", "crate; desc(fn || field);"})
        refactor:run_command("expand_local_ptr_tys", {})
        -- refactor:run_command("ownership_annotate", {"target"})
        refactor:run_command("ownership_mark_pointers", {})
        -- refactor:dump_marks()
    end

    refactor:transform(
        function(transform_ctx)
            if not node_id_cfgs then
                node_id_cfgs = infer_node_id_configs(transform_ctx)
                -- pretty.dump(node_id_cfgs)
            end
            return transform_ctx:visit_crate_new(Visitor.new(transform_ctx, node_id_cfgs))
        end
    )
end
