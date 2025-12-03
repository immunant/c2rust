#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

(cd "${SCRIPT_DIR}/repo" && ./configure --disable-shared --without-computed-gotos 2>&1 \
     | tee ../`basename "$0"`.log)

# The configure script may enable an optimization for va_list's such
# that they are memcpy'ed rather than va_copy'ed. Disable optimization
# after configure has run since there is not an option to turn it off.
SED_EXPR="s/define VA_LIST_IS_ARRAY 1/undef VA_LIST_IS_ARRAY/"
(cd "${SCRIPT_DIR}/repo" && find . -type f -name pyconfig.h -exec sed -ie "${SED_EXPR}" {} + )
