#!/usr/bin/env python3

import os
import sys
import logging
import argparse
import re

from common import *
from enum import Enum
from rust_file import (
    RustFile,
    RustFileBuilder,
    RustFunction,
    RustMatch,
    RustMod,
    RustUse,
    RustVisibility,
)
from typing import Generator, List, Optional, Set, Tuple

# Executables we are going to test
ast_extractor = get_cmd_or_die(AST_EXTR)
ast_importer = get_cmd_or_die(AST_IMPO)

# Tools we will need
clang = get_cmd_or_die("clang")
rustc = get_cmd_or_die("rustc")
diff = get_cmd_or_die("diff")

driver = os.path.join(ROOT_DIR, "scripts/driver.c")


# Intermediate files
intermediate_files = [
    'cc_db', 'cbor', 'rust_src', 'rust_test_exec',
]


class TestOutcome(Enum):
    Success = "successes"
    Failure = "expected failures"
    UnexpectedFailure = "unexpected failures"
    UnexpectedSuccess = "unexpected successes"


class NonZeroReturn(Exception): # IOException?
    pass


class CborFile:
    def __init__(self, path: str, enable_relooper: bool=False) -> None:
        self.path = path
        self.enable_relooper = enable_relooper

    def translate(self) -> RustFile:
        c_file_path, _ = os.path.splitext(self.path)
        extensionless_file, _ = os.path.splitext(c_file_path)
        rust_src = extensionless_file + ".rs"

        # help plumbum find rust
        ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # run the importer
        args = [self.path]

        if self.enable_relooper:
            args.append("--reloop-cfgs")

        with pb.local.env(RUST_BACKTRACE='1', LD_LIBRARY_PATH=ld_lib_path):
            # log the command in a format that's easy to re-run
            translation_cmd = "LD_LIBRARY_PATH=" + ld_lib_path + " \\\n"
            translation_cmd += str(ast_importer[args] > rust_src)
            logging.debug("translation command:\n %s", translation_cmd)
            retcode, stdout, stderr = (ast_importer[args] > rust_src).run(retcode=None)

        logging.debug("stdout:\n%s", stdout)

        if retcode != 0:
            raise NonZeroReturn(stderr)

        return RustFile(extensionless_file + ".rs")


class CFile:
    def __init__(self, path: str, flags: Set[str]=None) -> None:
        if not flags:
            flags = set()

        self.path = path
        self.enable_relooper = "enable_relooper" in flags

    def extract(self) -> CborFile:
        # run the extractor
        args = [self.path]

        # make sure we can locate system libraries
        sys_incl_dirs = get_system_include_dirs()
        args += ["-extra-arg=-I" + i for i in sys_incl_dirs]

        # log the command in a format that's easy to re-run
        logging.debug("extraction command:\n %s", str(ast_extractor[args]))
        retcode, stdout, stderr = ast_extractor[args].run(retcode=None)

        logging.debug("stdout:\n%s", stdout)

        if retcode != 0:
            raise NonZeroReturn(stderr)

        return CborFile(self.path + ".cbor", self.enable_relooper)


class TestFunction:
    def __init__(self, name: str, flags: Set[str]=set()) -> None:
        self.name = name
        self.pass_expected = "xfail" not in flags


class TestFile:
    def __init__(self, path: str, test_functions: List[TestFunction]=None, flags: Set[str]=None) -> None:
        if not flags:
            flags = set()

        self.path = path
        self.test_functions = test_functions or []
        self.pass_expected = "xfail" not in flags


class TestDirectory:
    def __init__(self, full_path: str, files: str, keep: List[str]) -> None:
        self.c_files = []
        self.rs_test_files = []
        self.full_path = full_path
        self.files = files
        self.name = full_path.split('/')[-1]
        self.keep = keep
        self.generated_files = {
            "rust_src": [],
            "cbor": [],
            "rust_test_exec": [],
            "cc_db": [],
        }

        for entry in os.listdir(full_path):
            path = os.path.abspath(os.path.join(full_path, entry))

            if os.path.isfile(path):
                _, ext = os.path.splitext(path)
                filename = os.path.splitext(os.path.basename(path))[0]

                if ext == ".c":
                    c_file = self._read_c_file(path)

                    if c_file:
                        self.c_files.append(c_file)
                elif filename.startswith("test_") and ext == ".rs" and files.search(filename):
                    rs_test_file = self._read_rust_test_file(path)

                    self.rs_test_files.append(rs_test_file)

    def _read_c_file(self, path: str) -> Optional[CFile]:
        file_config = None
        file_flags = set()

        with open(path, 'r') as file:
            file_config = re.match(r"//! (.*)\n", file.read())

        if file_config:
            flag_str = file_config.group(0)[3:]
            file_flags = {flag.strip() for flag in flag_str.split(',')}

        if "skip_translation" in file_flags:
            return

        return CFile(path, file_flags)

    def _read_rust_test_file(self, path: str) -> TestFile:
        with open(path, 'r') as file:
            file_buffer = file.read()

        file_config = re.match(r"//! (.*)\n", file_buffer)
        file_flags = set()

        if file_config:
            flags_str = file_config.group(0)[3:]
            file_flags = {flag.strip() for flag in flags_str.split(',')}

        found_tests = re.findall(r"(//(.*))?\npub fn (test_\w+)\(\)", file_buffer)
        test_fns = []

        for _, config, test_name in found_tests:
            test_flags = {flag.strip() for flag in config.split(',')}

            test_fns.append(TestFunction(test_name, test_flags))

        return TestFile(path, test_fns, file_flags)

    def print_status(self, color: str, status: str, message: Optional[str]) -> None:
        """
        Print coloured status information. Overwrites current line.
        """
        sys.stdout.write('\r')
        sys.stdout.write('\033[K')

        sys.stdout.write(color + ' [ ' + status + ' ] ' + NO_COLOUR)
        if message:
            sys.stdout.write(message)

    def _generate_cc_db(self, c_file_path: str) -> None:
        directory, cfile = os.path.split(c_file_path)

        compile_commands = """ \
        [
          {{
            "arguments": [ "cc", "-Wwrite-strings", "-D_FORTIFY_SOURCE=0", "-c", "{0}" ],
            "directory": "{1}",
            "file": "{0}"
          }}
        ]
        """.format(cfile, directory)

        cc_db = os.path.join(directory, "compile_commands.json")

        self.generated_files["cc_db"] = [cc_db]

        # REVIEW: This will override the previous compile_commands.json
        # Is there a way to specify different compile_commands_X.json files
        # to the extractor?
        with open(cc_db, 'w') as fh:
            fh.write(compile_commands)

    def _compile_rustc(self, rust_src_path: str) -> Tuple[int, str, str]:
        extensionless_file, _ = os.path.splitext(rust_src_path)

        # run rustc
        args = [
            '--crate-type=bin',
            '-o', extensionless_file,
            rust_src_path,
        ]
        # log the command in a format that's easy to re-run
        logging.debug("rustc compile command: %s", str(rustc[args]))
        return rustc[args].run(retcode=None)

    def run(self) -> TestOutcome:
        outcomes = []

        any_tests = any(test_fn for test_file in self.rs_test_files for test_fn in test_file.test_functions)

        if not self.files.pattern and not any_tests:
            description = "No tests were found...\n"
            self.print_status(OKBLUE, "SKIPPED", description)
            return []

        # .c -> .c.cbor
        for c_file in self.c_files:
            _, c_file_short = os.path.split(c_file.path)
            description = f"{c_file_short}: extracting the C file into CBOR..."

            # Run the step
            self.print_status(WARNING, "RUNNING", description)

            self._generate_cc_db(c_file.path)

            try:
                cbor_file = c_file.extract()
            except NonZeroReturn as exception:
                self.print_status(FAIL, "FAILED", "extract " + c_file_short)
                sys.stdout.write('\n')
                sys.stdout.write(str(exception))

                outcomes.append(TestOutcome.UnexpectedFailure)
                continue

            self.generated_files["cbor"].append(cbor_file)

        rust_file_builder = RustFileBuilder()
        rust_file_builder.add_features(["libc", "i128_type"])

        # .cbor -> .rs
        for cbor_file in self.generated_files["cbor"]:
            _, cbor_file_short = os.path.split(cbor_file.path)
            description = f"{cbor_file_short}: translate the CBOR..."

            self.print_status(WARNING, "RUNNING", description)

            try:
                translated_rust_file = cbor_file.translate()
            except NonZeroReturn as exception:
                self.print_status(FAIL, "FAILED", "translate " + c_file_short)
                sys.stdout.write('\n')
                sys.stdout.write(str(exception))

                outcomes.append(TestOutcome.UnexpectedFailure)
                continue

            self.generated_files["rust_src"].append(translated_rust_file)

            _, rust_file_short = os.path.split(translated_rust_file.path)
            extensionless_rust_file, _ = os.path.splitext(rust_file_short)

            rust_file_builder.add_mod(RustMod(extensionless_rust_file, RustVisibility.Public))

        match_arms = []

        matches = [i for i, test_file in enumerate(self.rs_test_files) if not test_file.pass_expected]
        failed_files = []
        for i in matches:
            failed_files.append(self.rs_test_files[i])
            self.rs_test_files.pop(i)

        for failed_file in failed_files:
            _, file_name = os.path.split(failed_file.path)
            self.print_status(OKBLUE, "FAILED", f"Expected failure {file_name}")
            sys.stdout.write('\n')
            outcomes.append(TestOutcome.Failure)
            if len(self.rs_test_files) == 0:
                return outcomes
            continue

        # Build one binary that can call all the tests
        for test_file in self.rs_test_files:
            _, file_name = os.path.split(test_file.path)
            extensionless_file_name, _ = os.path.splitext(file_name)

            for test_function in test_file.test_functions:
                rust_file_builder.add_mod(RustMod(extensionless_file_name, RustVisibility.Public))
                left = f"Some(\"{extensionless_file_name}::{test_function.name}\")"
                right = f"{extensionless_file_name}::{test_function.name}()"
                match_arms.append((left, right))

        match_arms.append(("e", "panic!(\"Tried to run unknown test: {:?}\", e)"))

        test_main_body = [
            RustMatch("std::env::args().nth(1).as_ref().map(String::as_ref)", match_arms),
        ]
        test_main = RustFunction("main",
                                 visibility=RustVisibility.Public,
                                 body=test_main_body)

        rust_file_builder.add_function(test_main)

        main_file = rust_file_builder.build(self.full_path + "/tests_main.rs")

        self.generated_files["rust_src"].append(main_file)

        # Try and compile test binary
        retcode, stdout, stderr = self._compile_rustc(main_file.path)

        if retcode != 0:
            _, main_file_path_short = os.path.split(main_file.path)

            self.print_status(FAIL, "FAILED", f"compile {main_file_path_short}")
            sys.stdout.write('\n')
            sys.stdout.write(stderr)

            return outcomes

        main_bin_path = self.full_path + "/tests_main"

        self.generated_files["rust_test_exec"].append(main_bin_path)

        main = get_cmd_or_die(main_bin_path)

        for test_file in self.rs_test_files:
            _, file_name = os.path.split(test_file.path)
            extensionless_file_name, _ = os.path.splitext(file_name)

            for test_function in test_file.test_functions:
                args = [f"{extensionless_file_name}::{test_function.name}"]

                retcode, stdout, stderr = main[args].run(retcode=None)

                test_str = file_name + ' - ' + test_function.name

                if retcode == 0:
                    if test_function.pass_expected:
                        self.print_status(OKGREEN, "OK", "    test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Success)
                    else:
                        self.print_status(FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.UnexpectedSuccess)

                elif retcode != 0:
                    if test_function.pass_expected:
                        self.print_status(FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')
                        sys.stdout.write(stderr)

                        outcomes.append(TestOutcome.UnexpectedFailure)
                    else:
                        self.print_status(OKBLUE, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Failure)

        if not outcomes:
            self.print_status(OKBLUE, "N/A", "   No rust file(s) matching " + self.files.pattern + " within this folder\n")
        return outcomes

    def cleanup(self) -> None:
        if "all" in self.keep:
            return

        for file_type, file_paths in self.generated_files.items():
            if file_type in self.keep:
                continue

            # Try remove files and don't barf if they don't exist
            for file_path in file_paths:
                try:
                    os.remove(getattr(file_path, "path", file_path)) # FIXME: Hacky
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


def get_testdirectories(directory: str, files: str, keep: List[str]) -> Generator[TestDirectory, None, None]:
    for entry in os.listdir(directory):
        path = os.path.abspath(os.path.join(directory, entry))

        if os.path.isdir(path):
            yield TestDirectory(path, files, keep)


def main() -> None:
    desc = 'run regression / unit / feature tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('directory', type=readable_directory)
    parser.add_argument(
        '--only-files', dest='regex_files', type=regex,
        default='.*', help="Regular expression to filter which tests to run"
    )
    parser.add_argument(
        '--only-directories', dest='regex_directories', type=regex,
        default='.*', help="Regular expression to filter which tests to run"
    )
    parser.add_argument(
        '--log', dest='logLevel',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
        default='CRITICAL', help="Set the logging level"
    )
    parser.add_argument(
        '--keep', dest='keep', action='append',
        choices=intermediate_files + ['all'], default=[],
        help="Which intermediate files to not clear"
    )

    args = parser.parse_args()
    test_directories = get_testdirectories(args.directory, args.regex_files, args.keep)
    setup_logging(args.logLevel)

    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [AST_EXTR, AST_IMPO]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_translator.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(DEPS_DIR)
    ensure_rustc_version(CUSTOM_RUST_RUSTC_VERSION)

    if not test_directories:
        die("nothing to test")

    # Accumulate test case stats
    test_results = {
        "unexpected failures": 0,
        "unexpected successes": 0,
        "expected failures": 0,
        "successes": 0
    }

    for test_directory in test_directories:
        if args.regex_directories.fullmatch(test_directory.name):
            sys.stdout.write(f"{test_directory.name}:\n")

            # TODO: Support regex for filtering by directory or name
            # Testdirectories are run one after another. Only tests that match the '--only'
            # argument are run. We make a best effort to clean up files we left behind.
            try:
                statuses = test_directory.run()
            finally:
                test_directory.cleanup()

            for status in statuses:
                test_results[status.value] += 1

        # else:
        #     logging.debug("skipping test: %s", testcase.src_c)

    # Print out test case stats
    sys.stdout.write("\nTest summary:\n")
    for variant, count in test_results.items():
        sys.stdout.write("  {}: {}\n".format(variant, count))

    # If anything unexpected happened, exit with error code 1
    unexpected = \
        test_results["unexpected failures"] + \
        test_results["unexpected successes"]
    if 0 < unexpected:
        quit(1)


if __name__ == "__main__":
    main()
