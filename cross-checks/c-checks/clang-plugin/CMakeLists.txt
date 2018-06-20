cmake_minimum_required(VERSION 3.8)

find_package(Clang REQUIRED CONFIG)

add_definitions(${CLANG_DEFINITIONS} ${LLVM_DEFINITIONS})
include_directories(${CLANG_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})

find_program(LLVM_TABLEGEN_EXE "llvm-tblgen" ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH)

# TableGen needs this
# Find "llvm/Option/OptParser.td" for Options.td,
# since LLVM_MAIN_INCLUDE_DIR needs to be a single path
find_path(LLVM_OPT_PARSER_PATH OptParser.td
    PATHS ${LLVM_INCLUDE_DIRS}
    PATH_SUFFIXES llvm/Option
    NO_DEFAULT_PATH
    )
get_filename_component(LLVM_INCLUDE_LLVM_DIR
    ${LLVM_OPT_PARSER_PATH} DIRECTORY)
get_filename_component(LLVM_MAIN_INCLUDE_DIR
    ${LLVM_INCLUDE_LLVM_DIR} DIRECTORY)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Needed for add_llvm_loadable_module
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
include(TableGen)
include(HandleLLVMOptions)

add_subdirectory(plugin)
add_subdirectory(runtime)
add_subdirectory(test)
