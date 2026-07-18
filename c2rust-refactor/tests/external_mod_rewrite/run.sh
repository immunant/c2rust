#!/bin/sh
set -e

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor rewrite_expr '123' '124' -- old.rs $rustflags

# The rewrite happens in the external module's own file; the parent file is
# checked by the shared runner.
diff -wB content.expected content.new
