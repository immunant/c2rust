#!/usr/bin/env -S uv run
# -*- coding: utf-8 -*-

import argparse
import errno
import logging
import multiprocessing
import os
from shutil import rmtree
import sys
import tempfile

from common import (
    config as c,
    pb,
    get_cmd_or_die,
    invoke,
    get_rust_toolchain_libpath,
    download_archive,
    invoke_quietly,
    die,
    on_mac,
    regex,
    setup_logging,
    ensure_dir,
    transpile,
)


# LUA_URL = "https://www.lua.org/ftp/lua-5.3.4.tar.gz"
# LUA_ARCHIVE = os.path.basename(LUA_URL)
LUA_URL = "https://github.com/LuaDist/lua/archive/5.3.2.tar.gz"
LUA_ARCHIVE = "lua-5.3.2.tar.gz"
LUA_SRC = LUA_ARCHIVE.replace(".tar.gz", "")
LUA_SRC = os.path.join(c.BUILD_DIR, LUA_SRC)

RUBY_URL = "https://cache.ruby-lang.org/pub/ruby/2.4/ruby-2.4.1.tar.gz"
RUBY_ARCHIVE = os.path.basename(RUBY_URL)
RUBY_SRC = RUBY_ARCHIVE.replace(".tar.gz", "")
RUBY_SRC = os.path.join(c.BUILD_DIR, RUBY_SRC)

JSON_C_URL = "https://s3.amazonaws.com/" + \
             "json-c_releases/releases/json-c-0.13.1.tar.gz"
JSON_C_ARCHIVE = os.path.basename(JSON_C_URL)
JSON_C_SRC = JSON_C_ARCHIVE.replace(".tar.gz", "")
JSON_C_SRC = os.path.join(c.BUILD_DIR, JSON_C_SRC)

TAR = get_cmd_or_die("tar")
SED = get_cmd_or_die("sed")
MAKE = get_cmd_or_die("make")
CMAKE = get_cmd_or_die("cmake")
BEAR = get_cmd_or_die("bear")
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
""".format(tempfile.gettempdir())


def _test_minimal(code_snippet: str) -> bool:
    transpiler = get_cmd_or_die(c.TRANSPILER)

    tempdir = tempfile.gettempdir()
    cfile = os.path.join(tempdir, "test.c")
    with open(cfile, 'w') as fh:
        fh.write(code_snippet)

    # avoid warnings about missing compiler flags, not strictly required
    cc_json = os.path.join(tempdir, "compile_commands.json")
    with open(cc_json, 'w') as fh:
        fh.write(minimal_cc_db)

    ld_lib_path = get_rust_toolchain_libpath()

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in pb.local.env:
        ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

    args = []
    args += ['--ddump-untyped-clang-ast']
    args += [cfile]

    # import ast
    with pb.local.env(RUST_BACKTRACE='1',
                      LD_LIBRARY_PATH=ld_lib_path):
        invoke(transpiler, args)

    return True  # if we get this far, test passed


def test_minimal(_: argparse.Namespace) -> bool:
    return _test_minimal(minimal_snippet)


def test_hello_world(_: argparse.Namespace) -> bool:
    return _test_minimal(hello_world_snippet)


def test_json_c(args: argparse.Namespace) -> bool:
    if not os.path.isfile(os.path.join(c.BUILD_DIR, JSON_C_ARCHIVE)):
        with pb.local.cwd(c.BUILD_DIR):
            download_archive(JSON_C_URL, JSON_C_ARCHIVE)
            invoke_quietly(TAR, "xf", JSON_C_ARCHIVE)

    cc_db_file = os.path.join(JSON_C_SRC, c.CC_DB_JSON)
    # unconditionally compile json-c since we don't know if
    # cc_db was generated from the environment we're in.
    with pb.local.cwd(JSON_C_SRC), pb.local.env(CC="clang"):
        if os.path.isfile('Makefile'):
            invoke(MAKE['clean'])
        configure = pb.local.get("./configure")
        invoke(configure)
        invoke(BEAR[MAKE[JOBS]])

    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    return transpile(cc_db_file)


def test_lua(args: argparse.Namespace) -> bool:
    """
    download lua, compile lua with bear to create
    a compiler command database, and use it to
    drive the transpiler.
    """

    if not os.path.isfile(os.path.join(c.BUILD_DIR, LUA_ARCHIVE)):
        with pb.local.cwd(c.BUILD_DIR):
            download_archive(LUA_URL, LUA_ARCHIVE)
    if not os.path.isdir(LUA_SRC):
        with pb.local.cwd(c.BUILD_DIR):
            invoke_quietly(TAR, "xf", LUA_ARCHIVE)

    # unconditionally compile lua since we don't know if
    # cc_db was generated from the environment we're in.
    build_dir = os.path.join(LUA_SRC, "build")
    rmtree(build_dir, ignore_errors=True)
    os.mkdir(build_dir)
    with pb.local.cwd(build_dir), pb.local.env(CC="clang"):
        invoke(CMAKE['-DCMAKE_EXPORT_COMPILE_COMMANDS=1', LUA_SRC])
        invoke(MAKE[JOBS])

    cc_db_file = os.path.join(LUA_SRC, "build", c.CC_DB_JSON)
    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    return transpile(cc_db_file)


def test_ruby(args: argparse.Namespace) -> bool:
    if on_mac():
        die("transpiling ruby on mac is not supported.")

    if not os.path.isfile(os.path.join(c.BUILD_DIR, RUBY_ARCHIVE)):
        with pb.local.cwd(c.BUILD_DIR):
            download_archive(RUBY_URL, RUBY_ARCHIVE)
            invoke_quietly(TAR, "xf", RUBY_ARCHIVE)

    cc_db_file = os.path.join(RUBY_SRC, c.CC_DB_JSON)

    # unconditionally compile ruby since we don't know if
    # cc_db was generated from the environment we're in.
    with pb.local.cwd(RUBY_SRC), pb.local.env(CC="clang",
                                              cflags="-w"):
        configure = pb.local.get("./configure")
        invoke(configure)
        invoke(BEAR[MAKE[JOBS]])

    if not os.path.isfile(cc_db_file):
        die("missing " + cc_db_file, errno.ENOENT)

    return transpile(cc_db_file)


def parse_args() -> argparse.Namespace:
    """
    define and parse command line arguments here.
    """
    desc = 'run integration tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument(
        '--only', dest='regex', type=regex,
        default='.*', help="Regular expression to filter which tests to run"
    )
    parser.add_argument('-j', '--jobs', type=int, dest="jobs",
                        default=multiprocessing.cpu_count(),
                        help='max number of concurrent jobs')
    parser.add_argument('-v', '--verbose', default=False, dest="verbose",
                        help='enable verbose output')
    return parser.parse_args()


def main() -> None:
    global JOBS
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [c.TRANSPILER]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_translator.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(c.BUILD_DIR)

    args = parse_args()
    JOBS = '-j' + str(args.jobs)

    # filter what gets tested using `what` argument
    tests = [test_minimal,
             test_hello_world,
             test_json_c,
             test_ruby,
             test_lua]
    tests = [t for t in tests if args.regex.search(t.__name__)]

    if not tests:
        die("nothing to test")

    success = True
    for t in tests:
        logging.debug("running test: %s", t.__name__)
        success = success and t(args)

    # FIXME: test lighttpd, varnish, Python, etc.
    # FIXME: add rebuild option?.

    if success:
        logging.info("PASS")
    else:
        logging.info("FAIL")
        quit(1)


if __name__ == "__main__":
    main()
