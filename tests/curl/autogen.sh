#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
(cd "$SCRIPT_DIR/repo" && ./buildconf)
