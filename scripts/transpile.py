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

from common import *
from typing import *
from typing.io import *
from concurrent.futures import ThreadPoolExecutor


def try_locate_elf_object(cmd: dict) -> Optional[str]:
    # first look for -o in compiler command
    if "arguments" in cmd:
        command = " ".join(cmd['arguments'])
    elif "command" in cmd:
        command = cmd['command']
    else:
        die("malformed entry in compile_commands.json:\n" +
            json_pp_obj(cmd))

    if "directory" not in cmd:
        die("malformed entry in compile_commands.json:\n" +
            json_pp_obj(cmd))
    dir = cmd['directory']

    # FIXME: assumes that outfile has .o suffix
    m = re.search(r"\s-o\s+([^\0]+\.o)\s", command)
    if m:
        outfile = m.group(1)
        outpath = os.path.join(dir, outfile)        
    else:
        # try replacing suffix of C file with .c
        inpath = os.path.join(dir, cmd['file'])
        outpath = inpath.replace(".c", ".o")

    if os.path.isfile(outpath):
        logging.debug("found output filename: %s", outpath)
        return outpath
    else:
        logging.debug("didn't find output filename for command:\n%s",
                      json_pp_obj(cmd))
        return None


def ensure_code_compiled_with_clang(cc_db: List[dict]) -> None:
    # filter non C code commands first
    c_cc_db = [c for c in cc_db if c['file'].endswith(".c")]
    if not len(c_cc_db):
        msg = "didn't find any commands compling C files"
        die(msg)

    obj_files = [try_locate_elf_object(c) for c in c_cc_db]
    readelf = get_cmd_or_die("readelf")
    comment_sections = [(f, readelf('-p', '.comment', f))
                        for f in obj_files if f]
    non_clang_files = [(f, c) for (f, c) in comment_sections
                       if "clang" not in c]

    if len(non_clang_files):
        msg = "some ELF objects were not compiled with clang:\n"
        msg += "\n".join([f for (f, c) in comment_sections])
        die(msg)

# this global is used as a flag shared between threads in transpile_files
exception_raised = False


def transpile_files(cc_db: TextIO,
                    jobs: int,
                    filter: str = None,
                    import_only: bool = False) -> None:
    """
    run the ast-extractor and ast-importer on all C files
    in a compile commands database.
    """
    ast_extr = get_cmd_or_die(AST_EXTR)
    ast_impo = get_cmd_or_die(AST_IMPO)
    cc_db_name = cc_db.name
    cc_db = json.load(cc_db)

    if filter:  # skip commands not matching file filter
        cc_db = [c for c in cc_db if filter in f['file']]

    ensure_code_compiled_with_clang(cc_db)
    include_dirs = get_system_include_dirs()

    def transpile_single(cmd) -> None:
        global exception_raised
        if exception_raised:
            return

        if import_only:
            cbor_file = os.path.join(cmd['directory'], cmd['file'] + ".cbor")
        else:
            cbor_file = extract_ast_from(ast_extr, cc_db_name,
                                         include_dirs, **cmd)
        assert os.path.isfile(cbor_file), "missing: " + cbor_file

        # since we compiled ast-importer with custom c2rust toolchain
        # we need to point to the custom toolchain's lib dir
        ld_lib_path = os.path.join(
            COMPILER_SUBMOD_DIR,
            "/build/x86_64-unknown-linux-gnu/stage2/lib")
        emsg = "custom rust compiler lib path missing: " + ld_lib_path
        assert os.path.isdir(ld_lib_path), emsg
        
        # don't overwrite existing ld lib path if any...
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # import extracted ast
        with pb.local.env(RUST_BACKTRACE='1',
                          LD_LIBRARY_PATH=ld_lib_path):
            logging.info(" importing ast from %s", os.path.basename(cbor_file))
            retcode, stdout, stderr = invoke_quietly(ast_impo, cbor_file)
            if retcode != 0:
                exception_raised = True
                argv = str(ast_impo[cbor_file])
                raise pb.ProcessExecutionError(argv,
                                               retcode,
                                               "(stdout elided)",
                                               stderr)

    if jobs == 1:
        for cmd in cc_db:
            transpile_single(cmd)
    else:
        # We use the ThreadPoolExecutor (not ProcesssPoolExecutor) because
        # 1. we spend most of the time outside the python interpreter, and
        # 2. it does not require that shared objects can be pickled.
        # 3. we can use a shared flag variable to acquiesce on error
        with ThreadPoolExecutor(jobs) as executor:
            futures = [executor.submit(transpile_single, cmd)
                       for cmd in cc_db]
        try:
            results = [f.result() for f in futures]
        except Exception as exc:
            die(str(exc))


def parse_args() -> argparse.Namespace:
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
    transpile_files(args.commands_json,
                    args.jobs,
                    args.filter,
                    args.import_only)

    logging.info(u"success 👍")

if __name__ == "__main__":
    main()
