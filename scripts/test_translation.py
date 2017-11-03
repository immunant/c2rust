#!/usr/bin/env python3

import os
import sys
import errno
import logging
import argparse
import multiprocessing
import subprocess

from common import *

# Executables we are going to test
ast_extractor = get_cmd_or_die(AST_EXTR)
ast_importer  = get_cmd_or_die(AST_IMPO)

# Tools we will need
clang = get_cmd_or_die("clang")
rustc = get_cmd_or_die("rustc")
diff  = get_cmd_or_die("diff")

driver = os.path.join(ROOT_DIR, "scripts/driver.c")

class TestCase:
    def __init__(self, path: str, pass_expected: bool) -> None:
        directory, cfile = os.path.split(path)
        filebase, ext = os.path.splitext(cfile)

        self.pass_expected = pass_expected
        self.directory = directory

        # Absolute paths to all of the files we will attempt to generate
        self.src_c     = path
        self.rust_src  = os.path.join(directory, filebase + '.rs')
        self.cc_db     = os.path.join(directory, 'compile_commands.json')
        self.cbor      = path + '.cbor'
        self.rust_obj  = os.path.join(directory, 'lib' + filebase + '.a')
        self.rust_exec = os.path.join(directory, filebase + '_rust')
        self.c_exec    = os.path.join(directory, filebase + '_c')
        self.rust_out  = os.path.join(directory, filebase + '_out.txt')
        self.c_out     = os.path.join(directory, filebase + '_out.txt')

    def generate_cc_db(self):
        directory, cfile = os.path.split(self.src_c)

        compile_commands = """ \
        [
          {{
            "arguments": [ "cc", "-c", "{0}" ],
            "directory": "{1}",
            "file": "{0}"
          }}
        ]
        """.format(cfile, directory)

        with open(self.cc_db, 'w') as fh:
            fh.write(compile_commands)

        return 0, "", ""

    def export_cbor(self):

        # run the extractor
        args = [ self.src_c ]
        return ast_extractor[args].run()


    def translate(self):

        # help plumbum find rust
        ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # run the importer
        args = [ self.cbor ]
        with pb.local.env(RUST_BACKTRACE='1', LD_LIBRARY_PATH=ld_lib_path):
            return ( ast_importer[args] > self.rust_src ).run()

    def compile_translated_rustc(self):

        # run rustc
        args = [
            '--crate-type=staticlib',
            '-o', self.rust_obj,
            self.rust_src
        ]
        return rustc[args].run()

    def compile_translated_clang(self):

        # run clang linking in the rust object file
        args = [
            '-lSystem', '-lresolv', '-lc', '-lm',
            '-o', self.rust_exec,
            self.rust_obj, driver,
        ]
        return clang[args].run()

    def compile_original_clang(self):

        # run clang
        args = [
            '-o', self.c_exec,
            driver, self.src_c
        ]
        return clang[args].run()

    def compare_run_outputs(self):

        # run the Rust executable
        run_result = ( get_cmd_or_die(self.rust_exec) > self.rust_out ).run()
        if run_result[0]: return run_result

        # run the C executable
        run_result = ( get_cmd_or_die(self.c_exec) > self.c_out ).run()
        if run_result[0]: return run_result

        # diff the two outputs
        args = [ '--minimal', self.rust_out, self.c_out ]
        return diff[args].run()


    def run(self):

        # List of things to do and the order in which to do them
        commands = [
            ("generate 'compile_commands.json'", self.generate_cc_db),
            ("extract the C file into CBOR",     self.export_cbor),
            ("import and translate the CBOR",    self.translate),
            ("compile the generated Rust",       self.compile_translated_rustc),
            ("link against the driver program",  self.compile_translated_clang),
            ("compile the original C program",   self.compile_original_clang),
            ("compare their outputs",            self.compare_run_outputs)
        ]

        failed = False

        for description, command in commands:

            # Run the step
            retcode, stdout, stderr = command()

            # Document failures
            if retcode:
                failed = True

                if self.pass_expected:
                    print("Unexpected failure", self.src_c)
                    print("Failed to", description)
                    if stdout: print("STDOUT:\n", stdout)
                    if stderr: print("STDERR:\n", stderr)

                break

        expect = "Unexpected" if self.pass_expected == failed else "(expected)"
        status = "failure" if failed else "success"
        print(self.src_c, ":", expect, status)

    def cleanup(self):

        files = [
            self.cc_db,
            self.cbor,
            self.c_exec,
            self.c_out,
            self.rust_src,
            self.rust_obj,
            self.rust_exec,
            self.rust_out
        ]

        # Try remove files and don't barf if they don't exist
        for filename in files:
            try:
                os.remove(filename)
            except OSError:
                pass

def readable_directory(directory: str) -> str:
    """
    Check that a directory is exists and is readable
    """

    if not os.path.isdir(directory):
        msg = "directory:{0} is not a valid path".format(directory)
        raise argparse.ArgumentTypeError(msg)
    elif not os.access(directory, os.R_OK):
        msg = "directory:{0} cannot be read".format(directory)
        raise argparse.ArgumentTypeError(msg)
    else:
        return directory

def get_testcases(directory: str) -> List[TestCase]:
    """
    Find the test cases in a directory
    """

    testcases = []

    for entry in os.listdir(directory):
        path = os.path.abspath(os.path.join(directory, entry))

        # Expect all C files to be test cases that should pass
        if os.path.isfile(path) and os.path.splitext(path)[1] == ".c":
            testcases.append(TestCase(path, True))

    return testcases


if __name__ == "__main__":
    desc = 'run regression / unit / feature tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('directory', type=readable_directory)
    parser.add_argument('-d', '--what', type=str, default='.*')

    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [AST_EXTR, AST_IMPO]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_ast_extractor.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(DEPS_DIR)

    args = parser.parse_args()
    testcases = get_testcases(args.directory)

    # TODO: filter what gets tested using `what` argument

    if not testcases:
        die("nothing to test")

    for testcase in testcases:
        logging.debug("running test: %s", testcase.src_c)
        try:
            testcase.run()
        finally:
            testcase.cleanup()

