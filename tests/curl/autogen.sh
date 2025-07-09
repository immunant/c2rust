#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
(cd "$SCRIPT_DIR/repo" &&
# ldap now exports `ldap_connect` with a different type. to avoid errors, rename
# curl's definition.
sed -i -E -e 's/\bldap_connect\b/curl_ldap_connect/g' 'lib/openldap.c' &&
./buildconf) 2>&1 > "$(basename "$0")".log
