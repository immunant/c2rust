set(LLVM_LINK_COMPONENTS support)


set(TINYCBOR_URL "https://github.com/intel/tinycbor/archive/v0.5.1.tar.gz"
    CACHE STRING "tinycbor download URL")
set(TINYCBOR_MD5 "7b4fd204441cc24a9c50b3b4b92c97fd" CACHE STRING "tinycbor archive md5 sum")
set(TINYCBOR_PREFIX "${CMAKE_BINARY_DIR}/tinycbor" CACHE STRING "tinycbor install prefix")
set(TINYCBOR_VERSION_H "${TINYCBOR_PREFIX}/src/tinycbor_build/src/tinycbor-version.h")

# Get the number of processors to pass to make -j
include(ProcessorCount)
ProcessorCount(NUM_PROCS)
if (NOT NUM_PROCS)
    set(NUM_PROCS 1)
endif()

include(ExternalProject)
ExternalProject_Add(tinycbor_build
            PREFIX ${TINYCBOR_PREFIX}
            URL ${TINYCBOR_URL}
            URL_HASH MD5=${TINYCBOR_MD5}
            CONFIGURE_COMMAND ""
            BUILD_COMMAND make --quiet prefix=${TINYCBOR_PREFIX} -j${NUM_PROCS}
            INSTALL_COMMAND make --quiet prefix=${TINYCBOR_PREFIX} install 
            # work around https://github.com/intel/tinycbor/issues/136
            TEST_COMMAND cp ${TINYCBOR_VERSION_H} ${TINYCBOR_PREFIX}/include/
            BUILD_IN_SOURCE 1
            BUILD_BYPRODUCTS ${TINYCBOR_PREFIX}/lib/libtinycbor.a
)

include_directories(${TINYCBOR_PREFIX}/include)

add_library(tinycbor STATIC IMPORTED)
set_target_properties(tinycbor PROPERTIES IMPORTED_LOCATION ${TINYCBOR_PREFIX}/lib/libtinycbor.a)
add_dependencies(tinycbor tinycbor_build)

add_clang_executable(ast-exporter
  AstExporter.cpp
  FloatingLexer.cpp
  )

# PRIVATE was added to make ast-exporter build with LLVM 6.0. Keyword
# description: https://cmake.org/pipermail/cmake/2016-May/063400.html
target_link_libraries(ast-exporter PRIVATE
  clangAST
  clangFrontend
  clangTooling
  clangBasic
  clangASTMatchers
  tinycbor
)

# ast-exporter will use the same headers that
# clang does, so they need to be in the same place
# in the build directory
add_dependencies(ast-exporter clang-headers)
