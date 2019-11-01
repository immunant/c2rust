require "upgrade_ptr_to_ref"

node_id_cfgs = {
    -- ten_mul
    [25] = ConvConfig.new{"ref"},
    [34] = ConvConfig.new{"ref"},
    -- struct_ptr
    [76] = ConvConfig.new{"ref"},
    [86] = ConvConfig.new{"slice", mutability="immut"},
    -- Ptrs
    [129] = ConvConfig.new{"ref", lifetime="r"},
    [133] = ConvConfig.new{"ref", lifetime="r"},
    [137] = ConvConfig.new{"slice", lifetime="s"},
    [141] = ConvConfig.new{"slice", lifetime="s"},
    [145] = ConvConfig.new{"opt_box"},
    -- SizedData
    [150] = ConvConfig.new{"opt_box_slice"},
    -- init_buf
    [158] = ConvConfig.new{"ref"},
    [166] = ConvConfig.new{"opt_box_slice"},
    -- init_buf2
    [220] = ConvConfig.new{"ref"},
    [228] = ConvConfig.new{"box_slice"},
    -- destroy_buf
    [287] = ConvConfig.new{"ref"},
    -- explicit_lifetimes
    [325] = ConvConfig.new{"ref", lifetime="a"},
    -- HeapItem
    [332] = ConvConfig.new{"box"},
    [336] = ConvConfig.new{"opt_box"},
    -- init_opt_item
    [341] = ConvConfig.new{"ref"},
    [347] = ConvConfig.new{"box"},
    -- init_opt_item2
    [422] = ConvConfig.new{"ref"},
    [428] = ConvConfig.new{"opt_box"},
    -- HTab
    [1758] = ConvConfig.new{"opt_box_slice"},
    -- HashHDR
    [1830] = ConvConfig.new{"array"},
    [1839] = ConvConfig.new{"array"},
    -- bm
    [536] = ConvConfig.new{"ref"},
    [560] = ConvConfig.new{"box_slice", mutability="mut"},
    -- byteswap
    [768] = ConvConfig.new{"ref"},
    [773] = ConvConfig.new{"ref"},
    [2064] = ConvConfig.new{"byteswap", 789, 807},
    [2065] = ConvConfig.new{"del"},
    [2066] = ConvConfig.new{"del"},
    [2067] = ConvConfig.new{"del"},
    [2056] = ConvConfig.new{"del"},
    [2057] = ConvConfig.new{"byteswap", 1013, 1041},
    [2058] = ConvConfig.new{"del"},
    [2059] = ConvConfig.new{"del"},
    [2060] = ConvConfig.new{"del"},
    [2061] = ConvConfig.new{"byteswap", 1241, 1269},
    -- byteswap2
    [1290] = ConvConfig.new{"ref"},
    [1296] = ConvConfig.new{"box"}, -- FIXME: not a box
    [2072] = ConvConfig.new{"del"},
    [2073] = ConvConfig.new{"del"},
    [2074] = ConvConfig.new{"del"},
    [2075] = ConvConfig.new{"byteswap", 1399, 1399},
    [2076] = ConvConfig.new{"del"},
    -- _category, Category, categories, bisearch_cat
    [1515] = ConvConfig.new{"slice"},
    -- opt_params
    [1708] = ConvConfig.new{"opt_ref", mutability="mut"},
    [1713] = ConvConfig.new{"opt_ref", mutability="immut"},
    [1718] = ConvConfig.new{"opt_slice"},
}

run_ptr_upgrades(node_id_cfgs)

refactor:save_crate()

print("Finished test_run_ptr_upgrades.lua")

