#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import errno
import logging
import argparse
import multiprocessing

from common import *
from transpile import transpile_files

LUA_URL = "https://www.lua.org/ftp/lua-5.3.4.tar.gz"
LUA_ARCHIVE = os.path.basename(LUA_URL)
LUA_SRC = LUA_ARCHIVE.replace(".tar.gz", "")
LUA_SRC = os.path.join(DEPS_DIR, LUA_SRC)

RUBY_URL = "https://cache.ruby-lang.org/pub/ruby/2.4/ruby-2.4.1.tar.gz"
RUBY_ARCHIVE = os.path.basename(RUBY_URL)
RUBY_SRC = RUBY_ARCHIVE.replace(".tar.gz", "")
RUBY_SRC = os.path.join(DEPS_DIR, RUBY_SRC)

JSON_C_URL = "https://s3.amazonaws.com/" + \
    "json-c_releases/releases/json-c-0.12.1.tar.gz"
JSON_C_ARCHIVE = os.path.basename(JSON_C_URL)
JSON_C_SRC = JSON_C_ARCHIVE.replace(".tar.gz", "")
JSON_C_SRC = os.path.join(DEPS_DIR, JSON_C_SRC)

TAR = get_cmd_or_die("tar")
SED = get_cmd_or_die("sed")
MAKE = get_cmd_or_die("make")
BEAR = get_cmd_or_die(BEAR_BIN)
JOBS = "-j2"  # main updates jobs based on args

minimal_snippet = """ \
int main() { return 0; }
"""

hello_world_snippet = """ \
#include <stdio.h>
int main() { 
  printf("Hello, World!\\n");
  return 0; 
}
"""

minimal_cc_db = """ \
[
  {{
    "arguments": [ "cc", "-c", "test.c" ],
    "directory": "{}",
    "file": "test.c"
  }}
]
""".format(os.path.join(ROOT_DIR, "scripts"))


def _test_minimal(code_snippet: str) -> None:
    ast_extr = get_cmd_or_die(AST_EXTR)
    ast_impo = get_cmd_or_die(AST_IMPO)
    cfile = os.path.join(ROOT_DIR, "scripts/test.c")
    with open(cfile, 'w') as fh:
        fh.write(code_snippet)

    # avoid warnings about missing compiler flags, not strictly required
    minimal_cc_db_path = os.path.join(ROOT_DIR, "scripts/compile_commands.json")
    with open(minimal_cc_db_path, 'w') as fh:
        fh.write(minimal_cc_db)

    cborfile = cfile + '.cbor'

    invoke(ast_extr[cfile])

    ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in pb.local.env:
        ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

    args = []
    args += ['--ddump-untyped-clang-ast']
    args += [cborfile]

    # import extracted ast
    with pb.local.env(RUST_BACKTRACE='1',
                      LD_LIBRARY_PATH=ld_lib_path):
        invoke(ast_impo, args)


def test_minimal(_: argparse.Namespace) -> None:
    _test_minimal(minimal_snippet)


def test_hello_world(_: argparse.Namespace) -> None:
    _test_minimal(hello_world_snippet)


def test_json_c(args: argparse.Namespace) -> None:
    with pb.local.cwd(DEPS_DIR):
        download_archive(JSON_C_URL, JSON_C_ARCHIVE)
        invoke_quietly(TAR, "xf", JSON_C_ARCHIVE)

    cc_db_file = os.path.join(JSON_C_SRC, CC_DB_JSON)
    if not os.path.isfile(cc_db_file):
        with pb.local.cwd(JSON_C_SRC), pb.local.env(CC="clang"):
            if os.path.isfile('Makefile'):
                invoke(MAKE['clean'])
            configure = pb.local.get("./configure")
            invoke(configure)
            invoke(BEAR[MAKE[JOBS]])

    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    with open(cc_db_file) as cc_db:
        transpile_files(cc_db, args.jobs)


def test_lua(args: argparse.Namespace) -> None:
    """
    download lua, compile lua with bear to create
    a compiler command database, and use it to
    drive the transpiler.
    """
    with pb.local.cwd(DEPS_DIR):
        download_archive(LUA_URL, LUA_ARCHIVE)
        invoke_quietly(TAR, "xf", LUA_ARCHIVE)

    # edit $LUA_SRC/src/Makefile to change CC
    expr = 's/^CC=/CC?=/g'
    makefile = os.path.join(LUA_SRC, "src/Makefile")
    SED('--in-place', '-e', expr, makefile)

    cc_db_file = os.path.join(LUA_SRC, CC_DB_JSON)
    if not os.path.isfile(cc_db_file):
        with pb.local.cwd(LUA_SRC), pb.local.env(CC="clang"):
            invoke(MAKE['clean'])
            invoke(BEAR[MAKE["linux", JOBS]])

    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    with open(cc_db_file) as cc_db:
        transpile_files(cc_db, args.jobs)


def test_ruby(args: argparse.Namespace) -> None:
    with pb.local.cwd(DEPS_DIR):
        download_archive(RUBY_URL, RUBY_ARCHIVE)
        invoke_quietly(TAR, "xf", RUBY_ARCHIVE)

    cc_db_file = os.path.join(RUBY_SRC, CC_DB_JSON)
    if not os.path.isfile(cc_db_file):
        with pb.local.cwd(RUBY_SRC), pb.local.env(CC="clang",
                                                  cflags="-w"):
            configure = pb.local.get("./configure")
            invoke(configure)
            invoke(BEAR[MAKE[JOBS]])

    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    with open(cc_db_file) as cc_db:
        transpile_files(cc_db, args.jobs)


def parse_args() -> argparse.Namespace:
    """
    define and parse command line arguments here.
    """
    desc = 'run integration tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-w', '--what', type=str, default='')
    parser.add_argument('-j', '--jobs', type=int, dest="jobs",
                        default=multiprocessing.cpu_count(),
                        help='max number of concurrent jobs')
    return parser.parse_args()


def main() -> None:
    global JOBS
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [BEAR_BIN, AST_EXTR, AST_IMPO]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_ast_extractor.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(DEPS_DIR)

    args = parse_args()
    JOBS = '-j' + str(args.jobs)

    # filter what gets tested using `what` argument
    tests = [test_minimal,
             test_hello_world,
             test_json_c,
             test_ruby,
             test_lua]
    tests = [t for t in tests if args.what in t.__name__]

    if not tests:
        die("nothing to test")

    for t in tests:
        logging.debug("running test: %s", t.__name__)
        t(args)

    # FIXME: test lighttpd, varnish, Python, etc.
    # FIXME: add rebuild option?.

    logging.info("PASS")

if __name__ == "__main__":
    main()
