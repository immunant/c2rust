set(LLVM_TARGET_DEFINITIONS Options.td)
tablegen(LLVM Options.inc -gen-opt-parser-defs)
add_public_tablegen_target(CrossCheckOptionsTableGen)

add_llvm_loadable_module(CrossChecks
    crosschecks.cpp
    default_config.cpp
    types.cpp
    PLUGIN_TOOL clang)

# Required for std::variant
set_target_properties(CrossChecks PROPERTIES CXX_STANDARD 17)

if(LLVM_ENABLE_PLUGINS AND (WIN32 OR CYGWIN))
  target_link_libraries(CrossChecks PRIVATE
    clangAST
    clangBasic
    clangFrontend
    LLVMSupport
    )
endif()

# FIXME: we could use add_custom_command() to build the Rust library ourselves
# here, but for now it's easier to rely on it being built in advance
set(XCHECK_CONFIG_LIB "" CACHE PATH "Path to built libcross_check_config_capi.a")
if(XCHECK_CONFIG_LIB)
    target_link_libraries(CrossChecks PRIVATE ${XCHECK_CONFIG_LIB})
endif()
