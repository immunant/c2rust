require "upgrade_ptr_to_ref"

node_id_cfgs = {
    -- ten_mul
    [25] = ConvCfg.new{"ref"},
    [34] = ConvCfg.new{"ref"},
    -- struct_ptr
    [76] = ConvCfg.new{"ref"},
    [86] = ConvCfg.new{"slice", mutability="immut"},
    -- Ptrs
    [129] = ConvCfg.new{"ref", lifetime="r"},
    [133] = ConvCfg.new{"ref", lifetime="r"},
    [137] = ConvCfg.new{"slice", lifetime="s"},
    [141] = ConvCfg.new{"slice", lifetime="s"},
    [145] = ConvCfg.new{"opt_box"},
    -- SizedData
    [150] = ConvCfg.new{"opt_box_slice"},
    -- init_buf
    [158] = ConvCfg.new{"ref"},
    [166] = ConvCfg.new{"opt_box_slice"},
    -- init_buf2
    [220] = ConvCfg.new{"ref"},
    [228] = ConvCfg.new{"box_slice"},
    -- destroy_buf
    [287] = ConvCfg.new{"ref"},
    -- explicit_lifetimes
    [325] = ConvCfg.new{"ref", lifetime="a"},
    -- HeapItem
    [332] = ConvCfg.new{"box"},
    [336] = ConvCfg.new{"opt_box"},
    -- init_opt_item
    [341] = ConvCfg.new{"ref"},
    [347] = ConvCfg.new{"box"},
    -- init_opt_item2
    [422] = ConvCfg.new{"ref"},
    [428] = ConvCfg.new{"opt_box"},
    -- HTab
    [528] = ConvCfg.new{"opt_box_slice"},
    -- HashHDR
    [539] = ConvCfg.new{"array"},
    [548] = ConvCfg.new{"array"},
    -- bm
    [555] = ConvCfg.new{"ref"},
    [579] = ConvCfg.new{"box_slice"},
    -- byteswap
    [789] = ConvCfg.new{"ref"},
    [794] = ConvCfg.new{"ref"},
    [1909] = ConvCfg.new{"byteswap", 810, 828},
    [1910] = ConvCfg.new{"del"},
    [1911] = ConvCfg.new{"del"},
    [1912] = ConvCfg.new{"del"},
    [1901] = ConvCfg.new{"del"},
    [1902] = ConvCfg.new{"byteswap", 1034, 1062},
    [1903] = ConvCfg.new{"del"},
    [1904] = ConvCfg.new{"del"},
    [1905] = ConvCfg.new{"del"},
    [1906] = ConvCfg.new{"byteswap", 1262, 1290},
    -- byteswap2
    [1311] = ConvCfg.new{"ref"},
    [1317] = ConvCfg.new{"box"}, -- FIXME: not a box
    [1917] = ConvCfg.new{"del"},
    [1918] = ConvCfg.new{"del"},
    [1919] = ConvCfg.new{"del"},
    [1920] = ConvCfg.new{"byteswap", 1420, 1420},
    [1921] = ConvCfg.new{"del"},
    -- _category, Category, categories, bisearch_cat
    [1536] = ConvCfg.new{"slice"},
    -- opt_params
    [1729] = ConvCfg.new{"opt_ref", mutability="mut"},
    [1734] = ConvCfg.new{"opt_ref", mutability="immut"},
    [1739] = ConvCfg.new{"opt_slice"},
}

run_ptr_upgrades(node_id_cfgs)

refactor:save_crate()

print("Finished test_run_ptr_upgrades.lua")

