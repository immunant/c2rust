#!/usr/bin/env python3

import os
import sys
import logging
import argparse
import re

from common import *

# Executables we are going to test
ast_extractor = get_cmd_or_die(AST_EXTR)
ast_importer = get_cmd_or_die(AST_IMPO)

# Tools we will need
clang = get_cmd_or_die("clang")
rustc = get_cmd_or_die("rustc")
diff = get_cmd_or_die("diff")

driver = os.path.join(ROOT_DIR, "scripts/driver.c")

# Terminal escape codes
OKBLUE = '\033[94m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
NO_COLOUR = '\033[0m'

# Intermediate files
intermediate_files = [
    'cc_db', 'cbor', 'c_exec', 'c_out',
    'rust_src', 'rust_obj', 'rust_exec', 'rust_out'
]


class TestCase:
    def __init__(self, path: str, pass_expected: bool, keep: List[str]) -> None:
        directory, cfile = os.path.split(path)
        filebase, ext = os.path.splitext(cfile)

        self.shortname = filebase
        self.directory = directory
        self.pass_expected = pass_expected
        self.keep = keep
        self.status = "unknown"

        # Absolute paths to all of the files we will attempt to generate
        self.src_c = path
        self.rust_src = os.path.join(directory, filebase + '.rs')
        self.cc_db = os.path.join(directory, 'compile_commands.json')
        self.cbor = path + '.cbor'
        self.rust_obj = os.path.join(directory, 'lib' + filebase + '.a')
        self.rust_exec = os.path.join(directory, filebase + '_rust')
        self.c_exec = os.path.join(directory, filebase + '_c')
        self.rust_out = os.path.join(directory, filebase + '_rust.txt')
        self.c_out = os.path.join(directory, filebase + '_c.txt')

    def print_status(self, color: str, status: str, message) -> None:
        """
        Print coloured status information. Overwrites current line.
        """
        sys.stdout.write('\r')
        sys.stdout.write('\033[K')

        sys.stdout.write(color + ' [ ' + status + ' ] ' + NO_COLOUR)
        sys.stdout.write(self.shortname)
        if message:
            sys.stdout.write(": " + message)

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
        args = [self.src_c]
        # make sure we can locate system libraries
        sys_incl_dirs = get_system_include_dirs()
        args += ["-extra-arg=-I" + i for i in sys_incl_dirs]
        return ast_extractor[args].run(retcode=None)

    def translate(self):

        # help plumbum find rust
        ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # run the importer
        args = [self.cbor]
        with pb.local.env(RUST_BACKTRACE='1', LD_LIBRARY_PATH=ld_lib_path):
            return (ast_importer[args] > self.rust_src).run(retcode=None)

    def compile_translated_rustc(self):

        # run rustc
        args = [
            '--crate-type=staticlib',  # could just as well be 'cdylib'
            '-o', self.rust_obj,
            self.rust_src
        ]
        return rustc[args].run(retcode=None)

    def compile_translated_clang(self):

        # run clang linking in the rust object file
        args = [
            '-lSystem', '-lresolv', '-lc', '-lm',
            '-o', self.rust_exec,
            self.rust_obj, driver,
        ]
        return clang[args].run(retcode = None)

    def compile_original_clang(self):

        # run clang
        args = [
            '-o', self.c_exec,
            driver, self.src_c
        ]
        return clang[args].run(retcode = None)

    def compare_run_outputs(self):

        # run the Rust executable
        run_result = (get_cmd_or_die(self.rust_exec) > self.rust_out).run(retcode=None)
        if run_result[0]: return run_result

        # run the C executable
        run_result = (get_cmd_or_die(self.c_exec) > self.c_out).run(retcode=None)
        if run_result[0]:
            return run_result

        # diff the two outputs
        args = ['--minimal', self.rust_out, self.c_out]
        return diff[args].run(retcode=None)

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

        failed_message = None

        for description, command in commands:

            # Run the step
            self.print_status(WARNING, "RUNNING", description + "...")
            retcode, stdout, stderr = command()

            # Document failures
            if retcode:
                failed_message = "failed to " + description + "."

                if self.pass_expected:
                    logging.error("Unexpected failure for " + self.shortname)
                    logging.error("Failed to " + description)
                    if stdout:
                        logging.error("STDOUT:\n" + stdout)
                    if stderr:
                        logging.error("STDERR:\n" + stderr)
                else:
                    logging.warning("Expected failure for " + self.shortname)
                    logging.warning("Failed to " + description)
                    if stdout:
                        logging.warning("STDOUT:\n" + stdout)
                    if stderr:
                        logging.warning("STDERR:\n" + stderr)

                break
        else:
            logging.info("Expected success for " + self.shortname)

        if bool(failed_message) == self.pass_expected:
            self.print_status(FAIL, "FAILED", failed_message or "(unexpected success)")
            self.status = "unexpected failures" if failed_message else "unexpected successes"
        elif failed_message:
            self.print_status(OKBLUE, "FAILED", failed_message + " (expected)")
            self.status = "expected failures"
        else:
            self.print_status(OKGREEN, "OK", None)
            self.status = "successes"

        sys.stdout.write("\n")

    def cleanup(self):

        files = [getattr(self, f) for f in intermediate_files if f not in self.keep]

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


def regex(raw: str):
    """
    Check that a string is a valid regex
    """

    try:
        return re.compile(raw)
    except re.error:
        msg = "only:{0} is not a valid regular expression".format(raw)
        raise argparse.ArgumentTypeError(msg)


def get_testcases(directory: str, keep: List[str]) -> List[TestCase]:
    """
    Find the test cases in a directory
    """

    testcases = []

    for entry in os.listdir(directory):
        path = os.path.abspath(os.path.join(directory, entry))

        # Expect all C files to be test cases that should pass
        if os.path.isfile(path) and os.path.splitext(path)[1] == ".c":

            # Files that mention the word "fail" in their first line are marked
            # as being expected to fail.
            try:
                with open(path, 'r') as f:
                    pass_expected = "fail" not in f.readline()
            except IOError:
                pass_expected = False

            testcases.append(TestCase(path, pass_expected, keep))

    return testcases


def main() -> None:
    desc = 'run regression / unit / feature tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('directory', type=readable_directory)
    parser.add_argument(
        '--only', dest='regex', type=regex,
        default='.*', help="Regular expression to filter which tests to run"
    )
    parser.add_argument(
        '--log', dest='logLevel',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
        default='CRITICAL', help="Set the logging level"
    )
    parser.add_argument(
        '--keep', dest='keep', action='append',
        choices=intermediate_files, default=[],
        help="Which intermediate files to not clear"
    )

    args = parser.parse_args()
    testcases = get_testcases(args.directory, args.keep)
    setup_logging(args.logLevel)

    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [AST_EXTR, AST_IMPO]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_translator.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(DEPS_DIR)

    if not testcases:
        die("nothing to test")

    # Accumulate test case stats
    test_results = {
        "unexpected failures": 0,
        "unexpected successes": 0,
        "expected failures": 0,
        "successes": 0
    }

    # Testcases are run one after another. Only tests that match the '--only'
    # argument are run. We make a best effort to clean up all files left behind.
    for testcase in testcases:
        if args.regex.fullmatch(testcase.shortname):
            logging.debug("running test: %s", testcase.src_c)
            try:
                testcase.run()
            finally:
                testcase.cleanup()

            if testcase.status:
                test_results[testcase.status] += 1

        else:
            logging.debug("skipping test: %s", testcase.src_c)

    # Print out test case stats
    sys.stdout.write("\nTest summary:\n")
    for variant, count in test_results.items():
        sys.stdout.write("  {}: {}\n".format(variant, count))

    # If anything failed (or there is something unknown), exit with error code 1
    if 0 < test_results["unexpected failures"] + test_results["unexpected successes"]:
        quit(1)


if __name__ == "__main__":
    main()
