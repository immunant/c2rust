#!/bin/bash
set -e; set -o pipefail

(cd repo && cargo build 2>&1 | tee ../`basename "$0"`.log)