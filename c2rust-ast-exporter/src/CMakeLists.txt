cmake_minimum_required(VERSION 3.4.3)


#################################################
# TinyCBOR                                      #
#################################################
set(TINYCBOR_URL "https://github.com/intel/tinycbor/archive/v0.5.1.tar.gz"
    CACHE STRING "tinycbor download URL")
set(TINYCBOR_MD5 "7b4fd204441cc24a9c50b3b4b92c97fd" CACHE STRING "tinycbor archive md5 sum")
set(TINYCBOR_PREFIX "${CMAKE_BINARY_DIR}/tinycbor" CACHE STRING "tinycbor install prefix")
set(TINYCBOR_VERSION_H "${TINYCBOR_PREFIX}/src/tinycbor_build/src/tinycbor-version.h")

include(ExternalProject)
ExternalProject_Add(tinycbor_build
            PREFIX ${TINYCBOR_PREFIX}
            INSTALL_DIR ${CMAKE_BINARY_DIR}
            URL ${TINYCBOR_URL}
            URL_HASH MD5=${TINYCBOR_MD5}
            CONFIGURE_COMMAND ""
            BUILD_COMMAND make --quiet prefix=<INSTALL_DIR> CFLAGS=-fPIC
            INSTALL_COMMAND make --quiet prefix=<INSTALL_DIR> install
            # work around https://github.com/intel/tinycbor/issues/136
            TEST_COMMAND cp ${TINYCBOR_VERSION_H} <INSTALL_DIR>/include/
            BUILD_IN_SOURCE 1
            BUILD_BYPRODUCTS lib/libtinycbor.a
)

include_directories(${CMAKE_BINARY_DIR}/include)

add_library(tinycbor STATIC IMPORTED)
set_target_properties(tinycbor PROPERTIES IMPORTED_LOCATION lib/libtinycbor.a)
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

if( PROJECT_NAME STREQUAL "LLVM" )
  # We are building in-tree, we can use LLVM cmake functions

  add_definitions(-DCLANG_BIN_PATH="${CMAKE_INSTALL_PREFIX}/bin")
  add_definitions(-DCLANG_VERSION_STRING="${PACKAGE_VERSION}")

  set(LLVM_OPTIONAL_SOURCES Main.cpp)
  add_clang_executable(c2rust-ast-exporter ${AST_EXPORTER_BIN_SRCS} DEPENDS clang-headers)
  add_clang_library(clangAstExporter ${AST_EXPORTER_SRCS} DEPENDS clang-headers)

  set(LLVM_LINK_COMPONENTS support)
else()
  find_package(LLVM REQUIRED CONFIG)

  # Debian and Ubuntu's clang cmake files are broken, so we can't require the
  # package here. We already have to manually order the link against the clang
  # libs in build.rs, so that's not so bad.
  find_package(Clang CONFIG)

  include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})

  if(Clang_FOUND)
    add_definitions(-DCLANG_BIN_PATH="${CLANG_INSTALL_PREFIX}/bin")
  else()
    add_definitions(-DCLANG_BIN_PATH="${LLVM_INSTALL_PREFIX}/bin")
  endif()
  add_definitions(-DCLANG_VERSION_STRING="${LLVM_PACKAGE_VERSION}")

  set(LLVM_LINK_COMPONENTS support)

  # LLVM is not always built with RTTI, we don't need it either.
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

  # The executable
  add_executable(c2rust-ast-exporter ${AST_EXPORTER_BIN_SRCS})

  # The library
  add_library(clangAstExporter STATIC ${AST_EXPORTER_SRCS})
endif()

add_definitions(-DCLANG_LIBDIR_SUFFIX="${LLVM_LIBDIR_SUFFIX}")

set_target_properties(c2rust-ast-exporter PROPERTIES
  CXX_STANDARD 11
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
  CXX_STANDARD 11
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
