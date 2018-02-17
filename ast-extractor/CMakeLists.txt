set(LLVM_LINK_COMPONENTS support)

add_clang_executable(ast-extractor
  AstExtractor.cpp
  )
target_link_libraries(ast-extractor
  clangTooling
  clangBasic
  clangASTMatchers
  tinycbor
  )

# ast-extractor will use the same headers that
# clang does, so they need to be in the same place
# in the build directory
add_dependencies(ast-extractor clang-headers)
