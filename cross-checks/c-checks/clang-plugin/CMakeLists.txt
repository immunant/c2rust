cmake_minimum_required(VERSION 3.4)

execute_process(COMMAND uname -n
    OUTPUT_VARIABLE MACHINE_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
set(LLVM_DIR ${CMAKE_SOURCE_DIR}/../../../llvm.build.${MACHINE_NAME})
set(CLANG_DIR ${LLVM_DIR}/tools/clang)

find_package(Clang REQUIRED CONFIG
    HINTS "${LLVM_DIR}/lib/cmake/clang")

add_definitions(${CLANG_DEFINITIONS})
include_directories(${CLANG_INCLUDE_DIRS})

# Needed for add_llvm_loadable_module
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

add_subdirectory(plugin)
