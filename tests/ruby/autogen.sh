#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
(cd "$SCRIPT_DIR/repo" && autoconf) 2>&1 > $(basename $0).log
