set(LLVM_LINK_COMPONENTS support)

add_clang_executable(ast-extractor
  AstExtractor.cpp
  )

# PRIVATE was added to make ast-extractor build with LLVM 6.0. Keyword
# description: https://cmake.org/pipermail/cmake/2016-May/063400.html
target_link_libraries(ast-extractor PRIVATE
  clangAST
  clangFrontend
  clangTooling
  clangBasic
  clangASTMatchers
  tinycbor
  )

# ast-extractor will use the same headers that
# clang does, so they need to be in the same place
# in the build directory
add_dependencies(ast-extractor clang-headers)
