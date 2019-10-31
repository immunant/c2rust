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
    [528] = ConvConfig.new{"opt_box_slice"},
    -- HashHDR
    [539] = ConvConfig.new{"array"},
    [548] = ConvConfig.new{"array"},
    -- bm
    [555] = ConvConfig.new{"ref"},
    [579] = ConvConfig.new{"box_slice"},
    -- byteswap
    [789] = ConvConfig.new{"ref"},
    [794] = ConvConfig.new{"ref"},
    [1909] = ConvConfig.new{"byteswap", 810, 828},
    [1910] = ConvConfig.new{"del"},
    [1911] = ConvConfig.new{"del"},
    [1912] = ConvConfig.new{"del"},
    [1901] = ConvConfig.new{"del"},
    [1902] = ConvConfig.new{"byteswap", 1034, 1062},
    [1903] = ConvConfig.new{"del"},
    [1904] = ConvConfig.new{"del"},
    [1905] = ConvConfig.new{"del"},
    [1906] = ConvConfig.new{"byteswap", 1262, 1290},
    -- byteswap2
    [1311] = ConvConfig.new{"ref"},
    [1317] = ConvConfig.new{"box"}, -- FIXME: not a box
    [1917] = ConvConfig.new{"del"},
    [1918] = ConvConfig.new{"del"},
    [1919] = ConvConfig.new{"del"},
    [1920] = ConvConfig.new{"byteswap", 1420, 1420},
    [1921] = ConvConfig.new{"del"},
    -- _category, Category, categories, bisearch_cat
    [1536] = ConvConfig.new{"slice"},
    -- opt_params
    [1729] = ConvConfig.new{"opt_ref", mutability="mut"},
    [1734] = ConvConfig.new{"opt_ref", mutability="immut"},
    [1739] = ConvConfig.new{"opt_slice"},
}

run_ptr_upgrades(node_id_cfgs)

refactor:save_crate()

print("Finished test_run_ptr_upgrades.lua")

