#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import Config
from plumbum.cmd import mv
from plumbum import local
from transpile import transpile_files

import argparse
import multiprocessing
import os

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates libxml2 into the repo/rust/src directory")
parser.add_argument('-f', '--filter',
                    default=None,
                    help='Filters translated files')
# parser.add_argument('-o', '--only',
#                     default=False, action='store_true',
#                     help='Translates only a single libxml2 C file')
# parser.add_argument('-x', '--cross-checks',
#                     default=False, action='store_true',
#                     help='enable cross-checks')
# parser.add_argument('-X', '--cross-check-config',
#                     default=[], action='append',
#                     help='cross-check configuration file(s)')

config = Config()

C2RUST_DIR = config.ROOT_DIR
LIBXML2_REPO = os.path.join(C2RUST_DIR, "examples/libxml2/repo")
COMPILE_COMMANDS = os.path.join(LIBXML2_REPO, "compile_commands.json")
RUST_ROOT_DIR = os.path.join(LIBXML2_REPO, "rust")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")

if __name__ == "__main__":
    args = parser.parse_args()
    importer_args = ["--reloop-cfgs"]
    num_jobs = multiprocessing.cpu_count()

    assert os.path.isfile(COMPILE_COMMANDS), "Could not find {}".format(COMPILE_COMMANDS)

    with open(COMPILE_COMMANDS, 'r') as cc_json:
        transpile_files(cc_json, num_jobs, extra_impo_args=importer_args, filter=args.filter, emit_build_files=False)

    # .rs files will be written to repos, but we need it in repos/rust
    plumbum_rs_glob = local.path(LIBXML2_REPO) // "*.rs"

    mv_args = [plumbum_rs_glob, RUST_SRC_DIR]
    retcode, stdout, stderr = mv[mv_args].run()

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)
