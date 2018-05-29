#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from common import (
    ROOT_DIR as C2RUST_DIR,
)
from transpile import transpile_files

import argparse

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates libxml2 into the repo/rust/src directory")
parser.add_argument('-d', '--debug',
                    default=False, action='store_true',
                    help='Use the debug build of the ast-importer')
# parser.add_argument('-o', '--only',
#                     default=False, action='store_true',
#                     help='Translates only a single libxml2 C file')
# parser.add_argument('-x', '--cross-checks',
#                     default=False, action='store_true',
#                     help='enable cross-checks')
# parser.add_argument('-X', '--cross-check-config',
#                     default=[], action='append',
#                     help='cross-check configuration file(s)')

FILES_TO_TRANSLATE = {
    "": {},
}

if __name__ == "__main__":
    args = parser.parse_args()

    # transpile_files("cc_db", "jobs", extra_impo_args=["--reloop-cfgs"], debug=args.debug)

