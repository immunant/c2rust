#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import json
import cbor
import errno
import shutil
import signal
import logging
import argparse
import platform
import multiprocessing

from typing import List, Union
from concurrent.futures import ThreadPoolExecutor

# FIXME: extract common functions and vars to separate file
from build_ast_extractor import *


def transpile_files(args) -> None:
    ast_extr = os.path.join(LLVM_BIN, "ast-extractor")
    ast_extr = get_cmd_or_die(ast_extr)
    ast_impo = os.path.join(
        SCRIPT_DIR,
        "ast-importer/target/debug/ast-importer")
    ast_impo = get_cmd_or_die(ast_impo)

    cc_db = json.load(args.commands_json)

    if args.filter:  # skip commands not matching file filter
        cc_db = [c for c in cc_db if args.filter in f['file']]

    cc_name = "cc"
    include_dirs = get_system_include_dirs(cc_name)

    def transpile_single(cmd):
        if args.import_only:
            cbor_file = os.path.join(cmd['directory'], cmd['file'] + ".cbor")
        else:
            cbor_file = extract_ast_from(ast_extr, args.commands_json.name,
                                         include_dirs, **cmd)
        assert os.path.isfile(cbor_file), "missing: " + cbor_file

        # import extracted ast
        with pb.local.env(RUST_BACKTRACE='1'):
            logging.info(" importing ast from %s", os.path.basename(cbor_file))
            retcode, stdout, stderr = invoke_quietly(ast_impo, cbor_file)
            # FIXME: error handling

    if args.jobs == 1:
        for cmd in cc_db:
            transpile_single(cmd)
    else:
        # We use the ThreadPoolExecutor (not ProcesssPoolExecutor) because
        # 1. we spend most of the time outside the python interpreter, and
        # 2. it does not require that shared objects can be pickled.
        with ThreadPoolExecutor(args.jobs) as executor:
            for cmd in cc_db:
                executor.submit(transpile_single, cmd)


def parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'transpile files in compiler_commands.json.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('commands_json', type=argparse.FileType('r'))
    parser.add_argument('-i', '--import-only', default=False,
                        action='store_true', dest='import_only',
                        help='skip ast extraction step')
    parser.add_argument('-f', '--filter', default="",
                        help='only process files matching filter')
    parser.add_argument('-j', '--jobs', type=int, dest="jobs",
                        default=multiprocessing.cpu_count(),
                        help='max number of concurrent jobs')
    return parser.parse_args()


def main():
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    args = parse_args()
    transpile_files(args)

    logging.info(u"success 👍")

if __name__ == "__main__":
    main()
