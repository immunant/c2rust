#!/bin/bash

set -e

SCRIPT_DIR=$(readlink -f $(dirname $0))
C2RUST_REFACTOR_DIR=$(dirname $SCRIPT_DIR)
LUA_AST_NODE_RS=src/scripting/lua_ast_node_gen.inc.rs

# Auto-generate the .rs file before running ldoc
cd $C2RUST_REFACTOR_DIR
./gen/process_ast.py lua_ast_node $LUA_AST_NODE_RS
ldoc .
rm $LUA_AST_NODE_RS
