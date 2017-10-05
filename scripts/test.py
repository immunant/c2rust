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

TAR = get_cmd_or_die("tar")
SED = get_cmd_or_die("sed")
MAKE = get_cmd_or_die("make")
BEAR = get_cmd_or_die(BEAR_BIN)


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
            invoke(BEAR[MAKE["linux"]])

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
        with pb.local.cwd(RUBY_SRC), pb.local.env(CC="clang"):
            configure = pb.local.get("./configure")
            invoke(configure)
            invoke(BEAR[MAKE])

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
    parser.add_argument('-w', '--what', type=str, nargs='+', default='')
    parser.add_argument('-j', '--jobs', type=int, dest="jobs",
                        default=multiprocessing.cpu_count(),
                        help='max number of concurrent jobs')
    return parser.parse_args()


def main() -> None:
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

    # filter what gets tested using `what` argument
    tests = [test_ruby, test_lua]
    tests = [t for t in tests if args.what in t.__name__]
    for t in tests:
        logging.debug("running test: %s", t.__name__)
        t(args)

    # FIXME: test lighttpd, varnish, Python, etc.

    logging.info("PASS")

if __name__ == "__main__":
    main()
