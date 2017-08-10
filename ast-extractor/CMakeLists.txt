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
