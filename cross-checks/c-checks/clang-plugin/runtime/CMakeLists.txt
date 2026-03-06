
add_library(runtime STATIC
    hash.c
    )

target_compile_options(runtime PRIVATE -ffunction-sections)
