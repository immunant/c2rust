set(LLVM_TARGET_DEFINITIONS Options.td)
tablegen(LLVM Options.inc -gen-opt-parser-defs)
add_public_tablegen_target(CrossCheckOptionsTableGen)

add_llvm_loadable_module(CrossChecks
    crosschecks.cpp
    config.cpp
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
