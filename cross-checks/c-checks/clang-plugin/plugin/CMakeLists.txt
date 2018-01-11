add_llvm_loadable_module(CrossChecks crosschecks.cpp PLUGIN_TOOL clang)

if(LLVM_ENABLE_PLUGINS AND (WIN32 OR CYGWIN))
  target_link_libraries(CrossChecks PRIVATE
    clangAST
    clangBasic
    clangFrontend
    LLVMSupport
    )
endif()
