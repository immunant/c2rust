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

find_program(LLVM_TABLEGEN_EXE "llvm-tblgen" ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH)

# TableGen needs this
set(LLVM_MAIN_INCLUDE_DIR ${LLVM_BUILD_MAIN_INCLUDE_DIR})
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Needed for add_llvm_loadable_module
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
include(TableGen)
include(HandleLLVMOptions)

add_subdirectory(plugin)
add_subdirectory(runtime)
