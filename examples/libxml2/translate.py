#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import (
    Colors,
    Config,
    get_cmd_or_die,
    pb,
    transpile
)
from plumbum.cmd import mv, mkdir
from plumbum import local

import argparse
import multiprocessing
import os

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates libxml2 into the repo/rust/src directory")
# parser.add_argument('-o', '--only',
#                     default=False, action='store_true',
#                     help='Translates only a single libxml2 C file')
parser.add_argument('-x', '--cross-checks',
                    default=False, action='store_true',
                    help='enable cross-checks')
# parser.add_argument('-X', '--cross-check-config',
#                     default=[], action='append',
#                     help='cross-check configuration file(s)')

config = Config()

C2RUST_DIR = config.ROOT_DIR
LIBXML2_REPO = os.path.join(C2RUST_DIR, "examples", "libxml2", "repo")
CROSS_CHECK_CONFIG_YAML = os.path.join(LIBXML2_REPO, "xchecks", "libxml2_rust.yaml")
COMPILE_COMMANDS = os.path.join(LIBXML2_REPO, "compile_commands.json")
RUST_ROOT_DIR = os.path.join(LIBXML2_REPO, "rust")
RUST_EXAMPLES_DIR = os.path.join(RUST_ROOT_DIR, "examples")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")

TESTS = [
    "xmllint",
    "runtest",
    "testapi",
    "testSAX",
    "testURI",
    "testdict",
    "testHTML",
    "testC14N",
    "testchar",
    "testRelax",
    "testXPath",
    "testModule",
    #"testWriter",
    "testlimits",
    "testReader",
    "testRegexp",
    "testrecurse",
    "testSchemas",
    "testThreads",
    "testAutomata",
]

if __name__ == "__main__":
    args = parser.parse_args()

    assert os.path.isfile(COMPILE_COMMANDS), "Could not find {}".format(COMPILE_COMMANDS)

    # Build the tests first
    print(Colors.OKBLUE + "Transpiling tests..." + Colors.NO_COLOR)
    transpile(COMPILE_COMMANDS,
              filter='^(test|xmllint|runtest)',
              emit_build_files=False,
              cross_checks=args.cross_checks,
              cross_check_config=[CROSS_CHECK_CONFIG_YAML])

    print(Colors.OKBLUE + "Transpiling rest of files..." + Colors.NO_COLOR)
    transpile(COMPILE_COMMANDS,
              emit_build_files=True,
              cross_checks=args.cross_checks,
              cross_check_config=[CROSS_CHECK_CONFIG_YAML])

    # Create rust/examples directory if it doesn't exist
    mkdir_args = ["-p", RUST_EXAMPLES_DIR]
    retcode, stdout, stderr = mkdir[mkdir_args].run()

    assert retcode != 1, "Could not make directory:\n{}".format(stderr)

    # Move test files to examples directory (since they have their own main function)
    mv_args = [os.path.join(LIBXML2_REPO, "%s.rs" % test) for test in TESTS]
    mv_args.append(RUST_EXAMPLES_DIR)
    retcode, stdout, stderr = mv[mv_args].run()

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)

    # Move source files to src directory
    plumbum_rs_glob = local.path(LIBXML2_REPO) // "*.rs"

    mv_args = [plumbum_rs_glob, RUST_SRC_DIR]
    retcode, stdout, stderr = mv[mv_args].run()

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)
    print(Colors.OKGREEN + "Done!" + Colors.NO_COLOR)
