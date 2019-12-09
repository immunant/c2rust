require "upgrade_ptr_to_ref"

node_id_cfgs = {}

NodeIdFinder = {}

function NodeIdFinder.new(tctx, node_descriptions)
    local self = {}

    self.tctx = tctx
    self.node_descriptions = node_descriptions

    setmetatable(self, NodeIdFinder)
    NodeIdFinder.__index = NodeIdFinder

    return self
end

function NodeIdFinder:flatten_stmt(fn, stmt, i)
    if stmt:kind_name() == "Local" then
        local locl = stmt:child(1)
        local pat = locl:get_pat()
        local pat_kind = pat:get_kind()
        local pat_ident = tostring(pat_kind:child(2):get_name())

        if fn.locl and fn.locl[pat_ident] then
            node_id_cfgs[locl:get_id()] = fn.locl[pat_ident]
        end
    -- "Flatten" stmts
    elseif stmt:child(1):kind_name() == "While" then
        local block = stmt:child(1):get_kind():child(2)
        local stmts = block:get_stmts()

        for _, stmt in ipairs(stmts) do
            self:flatten_stmt(fn, stmt, i)

            i = i + 1
        end
    end

    if fn.stmt and fn.stmt[i] then
        node_id_cfgs[stmt:get_id()] = fn.stmt[i]
    end
end

function NodeIdFinder:flat_map_item(item, walk)
    local kind_name = item:kind_name()
    local ident = tostring(item:get_ident():get_name())
    local item_kind = item:get_kind()

    if kind_name == "Fn" then
        local fn_desc = self.node_descriptions["fn"]
        local fn = fn_desc[ident]

        if fn then
            local fn_sig = item_kind:child(1)
            local fn_decl = fn_sig:get_decl()
            local params = fn_decl:get_inputs()

            for _, param in ipairs(params) do
                local pat = param:get_pat()
                local pat_kind = pat:get_kind()
                local pat_ident = tostring(pat_kind:child(2):get_name())

                if fn.param[pat_ident] then
                    node_id_cfgs[param:get_id()] = fn.param[pat_ident]
                end
            end

            local block = item_kind:child(3)
            local stmts = block:get_stmts()
            local i = 1

            for _, stmt in ipairs(stmts) do
                self:flatten_stmt(fn, stmt, i)

                i = i + 1
            end
        end
    elseif kind_name == "Struct" then
        local struct_desc = self.node_descriptions["struct"]
        local struct = struct_desc[ident]

        if struct then
            local fields = item_kind:child(1):child(1)

            for _, field in ipairs(fields) do
                local ident = tostring(field:get_ident():get_name())

                if struct.field and struct.field[ident] then
                    node_id_cfgs[field:get_id()] = struct.field[ident]
                end
            end
        end
    end

    walk(item)

    return {item}
end

node_descriptions = {
    fn = {
        ten_mul = {
            param = {
                acc = ConvConfig.new{"ref"},
                r = ConvConfig.new{"ref"},
            },
        },
        struct_ptr = {
            param = {
                ctx = ConvConfig.new{"ref"},
                p = ConvConfig.new{"slice", mutability="immut"},
            },
        },
        init_buf = {
            param = {
                sd = ConvConfig.new{"ref"},
            },
            locl = {
                buf = ConvConfig.new{"opt_box_slice"},
            },
        },
        init_buf2 = {
            param = {
                sd = ConvConfig.new{"ref"},
            },
            locl = {
                buf = ConvConfig.new{"box_slice"},
            },
        },
        destroy_buf = {
            param = {
                sd = ConvConfig.new{"ref"},
            },
        },
        explicit_lifetimes = {
            param = {
                _ptrs = ConvConfig.new{"ref", lifetime="a"},
            },
        },
        init_opt_item = {
            param = {
                hi = ConvConfig.new{"ref"},
            },
            locl = {
                ptr = ConvConfig.new{"box"},
            },
        },
        init_opt_item2 = {
            param = {
                hi = ConvConfig.new{"ref"},
            },
            locl = {
                ptr = ConvConfig.new{"opt_box"},
            }
        },
        bm = {
            param = {
                hashp = ConvConfig.new{"ref"},
            },
            locl = {
                ip = ConvConfig.new{"box_slice", mutability="mut"},
            },
        },
        bisearch_cat = {
            param = {
                table = ConvConfig.new{"slice"},
            },
        },
        opt_params = {
            param = {
                p1 = ConvConfig.new{"opt_ref", mutability="mut"},
                p2 = ConvConfig.new{"opt_ref", mutability="immut"},
                p3 = ConvConfig.new{"opt_slice"},
            },
        },
        byteswap = {
            param = {
                srcp = ConvConfig.new{"ref"},
                destp = ConvConfig.new{"ref"},
            },
            stmt = {
                [2] = ConvConfig.new{"byteswap", 790, 808},
                [3] = ConvConfig.new{"del"},
                [4] = ConvConfig.new{"del"},
                [5] = ConvConfig.new{"del"},
                [7] = ConvConfig.new{"del"},
                [8] = ConvConfig.new{"byteswap", 1014, 1042},
                [9] = ConvConfig.new{"del"},
                [10] = ConvConfig.new{"del"},
                [11] = ConvConfig.new{"del"},
                [12] = ConvConfig.new{"byteswap", 1242, 1270},
            },
        },
        byteswap2 = {
            param = {
                hashp = ConvConfig.new{"ref"},
            },
            locl = {
                hdrp = ConvConfig.new{"box"}, -- FIXME: not a box
            },
            stmt = {
                [3] = ConvConfig.new{"del"},
                [4] = ConvConfig.new{"del"},
                [5] = ConvConfig.new{"del"},
                [6] = ConvConfig.new{"byteswap", 1400, 1400},
                [7] = ConvConfig.new{"del"},
            },
        },
    },
    struct = {
        Ptrs = {
            field = {
                r = ConvConfig.new{"ref", lifetime="r"},
                r2 = ConvConfig.new{"ref", lifetime="r"},
                s = ConvConfig.new{"slice", lifetime="s"},
                s2 = ConvConfig.new{"slice", lifetime="s"},
                boxed = ConvConfig.new{"opt_box"},
            },
        },
        SizedData = {
            field = {
                buf = ConvConfig.new{"opt_box_slice"},
            },
        },
        HeapItem = {
            field = {
                item = ConvConfig.new{"box"},
                opt_item = ConvConfig.new{"opt_box"},
            },
        },
        HTab = {
            field = {
                mapp = ConvConfig.new{"opt_box_slice"},
            },
        },
        HashHDR = {
            field = {
                bitmaps = ConvConfig.new{"array"},
                spares = ConvConfig.new{"array"},
            },
        },
    },
}

refactor:transform(
    function(transform_ctx)
        -- transform_ctx:dump_crate()
        return transform_ctx:visit_crate_new(NodeIdFinder.new(transform_ctx, node_descriptions))
    end
)

run_ptr_upgrades(node_id_cfgs)

refactor:save_crate()

print("Finished test_run_ptr_upgrades.lua")
