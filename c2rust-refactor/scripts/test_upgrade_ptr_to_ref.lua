require "upgrade_ptr_to_ref"

node_id_cfgs = {
    -- ten_mul
    [26] = ConvCfg.new{"ref"},
    [35] = ConvCfg.new{"ref"},
    -- struct_ptr
    [77] = ConvCfg.new{"ref"},
    [87] = ConvCfg.new{"slice", mutability="immut"},
    -- Ptrs
    [130] = ConvCfg.new{"ref", lifetime="r"},
    [134] = ConvCfg.new{"ref", lifetime="r"},
    [138] = ConvCfg.new{"slice", lifetime="s"},
    [142] = ConvCfg.new{"slice", lifetime="s"},
    [146] = ConvCfg.new{"opt_box"},
    -- SizedData
    [151] = ConvCfg.new{"opt_box_slice"},
    -- init_buf
    [159] = ConvCfg.new{"ref"},
    [167] = ConvCfg.new{"opt_box_slice"},
    -- init_buf2
    [221] = ConvCfg.new{"ref"},
    [229] = ConvCfg.new{"box_slice"},
    -- destroy_buf
    [288] = ConvCfg.new{"ref"},
    -- explicit_lifetimes
    [326] = ConvCfg.new{"ref", lifetime="a"},
    -- HeapItem
    [333] = ConvCfg.new{"box"},
    [337] = ConvCfg.new{"opt_box"},
    -- init_opt_item
    [342] = ConvCfg.new{"ref"},
    [348] = ConvCfg.new{"box"},
    -- init_opt_item2
    [423] = ConvCfg.new{"ref"},
    [429] = ConvCfg.new{"opt_box"},
    -- HTab
    [529] = ConvCfg.new{"opt_box_slice"},
    -- HashHDR
    [540] = ConvCfg.new{"array"},
    [549] = ConvCfg.new{"array"},
    -- bm
    [556] = ConvCfg.new{"ref"},
    [580] = ConvCfg.new{"box_slice"},
    -- byteswap
    [790] = ConvCfg.new{"ref"},
    [795] = ConvCfg.new{"ref"},
    [1909] = ConvCfg.new{"byteswap", 811, 829},
    [1910] = ConvCfg.new{"del"},
    [1911] = ConvCfg.new{"del"},
    [1912] = ConvCfg.new{"del"},
    [1901] = ConvCfg.new{"del"},
    [1902] = ConvCfg.new{"byteswap", 1035, 1063},
    [1903] = ConvCfg.new{"del"},
    [1904] = ConvCfg.new{"del"},
    [1905] = ConvCfg.new{"del"},
    [1906] = ConvCfg.new{"byteswap", 1263, 1291},
    -- byteswap2
    [1312] = ConvCfg.new{"ref"},
    [1318] = ConvCfg.new{"box"}, -- FIXME: not a box
    [1917] = ConvCfg.new{"del"},
    [1918] = ConvCfg.new{"del"},
    [1919] = ConvCfg.new{"del"},
    [1920] = ConvCfg.new{"byteswap", 1421, 1421},
    [1921] = ConvCfg.new{"del"},
    -- _category, Category, categories, bisearch_cat
    [1535] = ConvCfg.new{"slice"},
    -- opt_params
    [1728] = ConvCfg.new{"opt_ref"},
    [1733] = ConvCfg.new{"opt_ref", mutability="immut"},
    [1738] = ConvCfg.new{"opt_slice"},
}

run_ptr_upgrades(node_id_cfgs)

refactor:save_crate()

print("Finished test_run_ptr_upgrades.lua")

