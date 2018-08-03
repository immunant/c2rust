#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import Config
from plumbum.cmd import mv, mkdir
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
RUST_EXAMPLES_DIR = os.path.join(RUST_ROOT_DIR, "examples")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")

if __name__ == "__main__":
    args = parser.parse_args()
    num_jobs = multiprocessing.cpu_count()

    assert os.path.isfile(COMPILE_COMMANDS), "Could not find {}".format(COMPILE_COMMANDS)

    with open(COMPILE_COMMANDS, 'r') as cc_json:
        transpile_files(cc_json, filter=args.filter, emit_build_files=False)

    # Create rust/examples directory if it doesn't exist
    mkdir_args = ["-p", RUST_EXAMPLES_DIR]
    retcode, stdout, stderr = mkdir[mkdir_args].run()

    assert retcode != 1, "Could not make directory:\n{}".format(stderr)

    # Move test files to examples directory (since they have their own main function)
    xmllint_rs = os.path.join(LIBXML2_REPO, "xmllint.rs")
    runtest_rs = os.path.join(LIBXML2_REPO, "runtest.rs")
    testapi_rs = os.path.join(LIBXML2_REPO, "testapi.rs")
    testsax_rs = os.path.join(LIBXML2_REPO, "testSAX.rs")
    testuri_rs = os.path.join(LIBXML2_REPO, "testURI.rs")
    testdict_rs = os.path.join(LIBXML2_REPO, "testdict.rs")
    testhtml_rs = os.path.join(LIBXML2_REPO, "testHTML.rs")
    testc14n_rs = os.path.join(LIBXML2_REPO, "testC14N.rs")
    testchar_rs = os.path.join(LIBXML2_REPO, "testchar.rs")
    testrelax_rs = os.path.join(LIBXML2_REPO, "testRelax.rs")
    testxpath_rs = os.path.join(LIBXML2_REPO, "testXPath.rs")
    testmodule_rs = os.path.join(LIBXML2_REPO, "testModule.rs")
    # testwriter_rs = os.path.join(LIBXML2_REPO, "testWriter.rs")
    testlimits_rs = os.path.join(LIBXML2_REPO, "testlimits.rs")
    testreader_rs = os.path.join(LIBXML2_REPO, "testReader.rs")
    testregexp_rs = os.path.join(LIBXML2_REPO, "testRegexp.rs")
    testrecurse_rs = os.path.join(LIBXML2_REPO, "testrecurse.rs")
    testschemas_rs = os.path.join(LIBXML2_REPO, "testSchemas.rs")
    testthreads_rs = os.path.join(LIBXML2_REPO, "testThreads.rs")
    testautomata_rs = os.path.join(LIBXML2_REPO, "testAutomata.rs")
    mv_args = [
        xmllint_rs,
        testapi_rs,
        testsax_rs,
        testuri_rs,
        testdict_rs,
        testhtml_rs,
        testc14n_rs,
        testchar_rs,
        testrelax_rs,
        testxpath_rs,
        testregexp_rs,
        testmodule_rs,
        # testwriter_rs,
        testlimits_rs,
        testreader_rs,
        testrecurse_rs,
        testschemas_rs,
        testthreads_rs,
        testautomata_rs,
        runtest_rs,
        RUST_EXAMPLES_DIR,
    ]
    retcode, stdout, stderr = mv[mv_args].run()

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)

    # Move source files to src directory
    plumbum_rs_glob = local.path(LIBXML2_REPO) // "*.rs"

    mv_args = [plumbum_rs_glob, RUST_SRC_DIR]
    retcode, stdout, stderr = mv[mv_args].run()

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)
