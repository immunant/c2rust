cmake_minimum_required(VERSION 3.4.3)
project(ASTExporter)

#################################################
# TinyCBOR                                      #
#################################################

set(TINYCBOR_REPO "https://github.com/intel/tinycbor.git" CACHE STRING "tinycbor git repo URL")

# v0.6.3 tag, but using the commit hash instead (of the tarball hash) for integrity checks
# unlike a .tar.gz MD5 hash, this SHA-1 commit hash should stay stable regardless of compression/archiving
# (GitHub has changed this), and still retains the integrity check
set(TINYCBOR_TAG "d393c16f3eb30d0c47e6f9d92db62272f0ec4dc7" CACHE STRING "tinycbor git tag/branch/commit hash")

set(TINYCBOR_PREFIX "${CMAKE_BINARY_DIR}/tinycbor" CACHE STRING "tinycbor install prefix")

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    set(MAKE "gmake")
else()
    set(MAKE "make")
endif()

include(ExternalProject)
ExternalProject_Add(tinycbor_build
            PREFIX ${TINYCBOR_PREFIX}
            INSTALL_DIR ${CMAKE_BINARY_DIR}
            GIT_REPOSITORY ${TINYCBOR_REPO}
            GIT_TAG ${TINYCBOR_TAG}
            # the fd redirection here fails when the build run inside Cargo.
            # patch from upstream:
            # https://github.com/intel/tinycbor/commit/6176e0a28d7c5ef3a5e9cbd02521999c412de72c
            PATCH_COMMAND patch --forward -p1 < ${CMAKE_CURRENT_SOURCE_DIR}/tinycbor_fix_build.patch || true
            CONFIGURE_COMMAND ${MAKE} .config && cat ${CMAKE_CURRENT_SOURCE_DIR}/tinycbor.config >> .config
            BUILD_COMMAND ${MAKE} --quiet prefix=<INSTALL_DIR> CFLAGS=-fPIC
            INSTALL_COMMAND ${MAKE} --quiet prefix=<INSTALL_DIR> install
            BUILD_IN_SOURCE 1
            BUILD_BYPRODUCTS ${CMAKE_BINARY_DIR}/lib/libtinycbor.a
)

include_directories(${CMAKE_BINARY_DIR}/include)

add_library(tinycbor STATIC IMPORTED)
set_target_properties(tinycbor PROPERTIES IMPORTED_LOCATION ${CMAKE_BINARY_DIR}/lib/libtinycbor.a)
add_dependencies(tinycbor tinycbor_build)

set(AST_EXPORTER_SRCS
  AstExporter.cpp
  FloatingLexer.cpp
  ExportResult.cpp
  )

set(AST_EXPORTER_BIN_SRCS
  ${AST_EXPORTER_SRCS}
  Main.cpp
  )

find_package(LLVM REQUIRED CONFIG)

# Debian and Ubuntu's clang cmake files are broken, so we can't require the
# package here. We already have to manually order the link against the clang
# libs in build.rs, so that's not so bad.
find_package(Clang CONFIG)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})

if (DEFINED CLANG_INSTALL_PREFIX)
  add_definitions(-DCLANG_BIN_PATH="${CLANG_INSTALL_PREFIX}/bin")
elseif(DEFINED LLVM_INSTALL_PREFIX)
  add_definitions(-DCLANG_BIN_PATH="${LLVM_INSTALL_PREFIX}/bin")
elseif(DEFINED LLVM_TOOLS_BINARY_DIR)
  add_definitions(-DCLANG_BIN_PATH="${LLVM_TOOLS_BINARY_DIR}")
else()
  message(FATAL_ERROR "Cannot find path to clang binary")
endif()
add_definitions(-DCLANG_VERSION_STRING="${LLVM_PACKAGE_VERSION}")

set(LLVM_LINK_COMPONENTS support)

# LLVM is not always built with RTTI, we don't need it either.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

# The executable
add_executable(c2rust-ast-exporter ${AST_EXPORTER_BIN_SRCS})

# The library
add_library(clangAstExporter STATIC ${AST_EXPORTER_SRCS})

add_definitions(-DCLANG_LIBDIR_SUFFIX="${LLVM_LIBDIR_SUFFIX}")

set_target_properties(c2rust-ast-exporter PROPERTIES
  CXX_STANDARD 14
  CXX_EXTENSIONS OFF
  )
# PRIVATE was added to make c2rust-ast-exporter build with LLVM 6.0. Keyword
# description: https://cmake.org/pipermail/cmake/2016-May/063400.html
target_link_libraries(c2rust-ast-exporter PRIVATE
  clangAST
  clangFrontend
  clangTooling
  clangBasic
  clangASTMatchers
  tinycbor
  )

set_target_properties(clangAstExporter PROPERTIES
  CXX_STANDARD 17 # will decay to 14 if compiler doesn't support c++17
  CXX_EXTENSIONS OFF
  )
target_link_libraries(clangAstExporter PRIVATE
  clangAST
  clangFrontend
  clangTooling
  clangBasic
  clangASTMatchers
  tinycbor
  )
