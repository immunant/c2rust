#!/usr/bin/env python3

import errno
import os
import sys
import logging
import argparse
import re

from common import (
    config as c,
    pb,
    Colors,
    get_cmd_or_die,
    get_rust_toolchain_libpath,
    NonZeroReturn,
    regex,
    setup_logging,
    die,
    ensure_dir,
)
from enum import Enum
from rust_file import (
    CrateType,
    RustFile,
    RustFileBuilder,
    RustFunction,
    RustMatch,
    RustMod,
    RustVisibility,
)
from typing import Generator, List, Optional, Set, Iterable

# Tools we will need
clang = get_cmd_or_die("clang")
rustc = get_cmd_or_die("rustc")
diff = get_cmd_or_die("diff")
ar = get_cmd_or_die("ar")


# Intermediate files
intermediate_files = [
    'cc_db', 'cbor', 'c_obj', 'c_lib', 'rust_src', 'rust_test_exec',
]


class TestOutcome(Enum):
    Success = "successes"
    Failure = "expected failures"
    UnexpectedFailure = "unexpected failures"
    UnexpectedSuccess = "unexpected successes"


class CStaticLibrary:
    def __init__(self, path: str, link_name: str,
                 obj_files: List[str]) -> None:
        self.path = path
        self.link_name = link_name
        self.obj_files = obj_files


class CFile:
    def __init__(self, path: str, flags: Set[str] = None) -> None:
        if not flags:
            flags = set()

        self.path = path
        self.enable_relooper = "enable_relooper" in flags
        self.disallow_current_block = "disallow_current_block" in flags

    def translate(self, extra_args: List[str] = []) -> RustFile:
        extensionless_file, _ = os.path.splitext(self.path)

        # help plumbum find rust
        ld_lib_path = get_rust_toolchain_libpath()
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # run the transpiler
        transpiler = get_cmd_or_die(c.TRANSPILER)

        args = [
            self.path,
            "--prefix-function-names",
            "rust_",
        ]

        if self.enable_relooper:
            args.append("--reloop-cfgs")
            #  args.append("--use-c-loop-info")
            #  args.append("--use-c-multiple-info")
        if self.disallow_current_block:
            args.append("--fail-on-multiple")

        args.append("--")
        args.extend(extra_args)

        with pb.local.env(RUST_BACKTRACE='1', LD_LIBRARY_PATH=ld_lib_path):
            # log the command in a format that's easy to re-run
            translation_cmd = "LD_LIBRARY_PATH=" + ld_lib_path + " \\\n"
            translation_cmd += str(transpiler[args])
            logging.debug("translation command:\n %s", translation_cmd)
            retcode, stdout, stderr = (transpiler[args]).run(
                retcode=None)

        logging.debug("stdout:\n%s", stdout)

        if retcode != 0:
            raise NonZeroReturn(stderr)

        return RustFile(extensionless_file + ".rs")


def build_static_library(c_files: Iterable[CFile],
                         output_path: str) -> Optional[CStaticLibrary]:
    current_path = os.getcwd()

    os.chdir(output_path)

    # create .o files
    args = ["-c", "-fPIC", "-march=native"]

    args.extend(c_file.path for c_file in c_files)

    if len(args) == 2:
        return

    logging.debug("complication command:\n %s", str(clang[args]))
    retcode, stdout, stderr = clang[args].run(retcode=None)

    logging.debug("stdout:\n%s", stdout)

    if retcode != 0:
        raise NonZeroReturn(stderr)

    args = ["-rv", "libtest.a"]
    obj_files = []

    for c_file in c_files:
        extensionless_file_name, _ = os.path.splitext(c_file.path)
        args.append(extensionless_file_name + ".o")
        obj_files.append(extensionless_file_name + ".o")

    logging.debug("combination command:\n %s", str(ar[args]))
    retcode, stdout, stderr = ar[args].run(retcode=None)

    logging.debug("stdout:\n%s", stdout)

    if retcode != 0:
        raise NonZeroReturn(stderr)

    os.chdir(current_path)

    return CStaticLibrary(output_path + "/libtest.a", "test", obj_files)


class TestFunction:
    def __init__(self, name: str, flags: Set[str] = set()) -> None:
        self.name = name
        self.pass_expected = "xfail" not in flags


class TestFile(RustFile):
    def __init__(self, path: str, test_functions: List[TestFunction] = None,
                 flags: Set[str] = None) -> None:
        if not flags:
            flags = set()

        super().__init__(path)

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
            "c_obj": [],
            "c_lib": [],
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
                elif (filename.startswith("test_") and ext == ".rs" and
                      files.search(filename)):
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

        found_tests = re.findall(
            r"(//(.*))?\npub fn (test_\w+)\(\)", file_buffer)
        test_fns = []

        for _, config, test_name in found_tests:
            test_flags = {flag.strip() for flag in config.split(',')}

            test_fns.append(TestFunction(test_name, test_flags))

        return TestFile(path, test_fns, file_flags)

    def print_status(self, color: str, status: str,
                     message: Optional[str]) -> None:
        """
        Print coloured status information. Overwrites current line.
        """
        sys.stdout.write('\r')
        sys.stdout.write('\033[K')

        sys.stdout.write(color + ' [ ' + status + ' ] ' + Colors.NO_COLOR)
        if message:
            sys.stdout.write(message)

    def _generate_cc_db(self, c_file_path: str) -> None:
        directory, cfile = os.path.split(c_file_path)

        compile_commands = """ \
        [
          {{
            "arguments": [ "cc", "-D_FORTIFY_SOURCE=0", "-c", "{0}" ],
            "directory": "{1}",
            "file": "{0}"
          }}
        ]
        """.format(cfile, directory)

        cc_db = os.path.join(directory, "compile_commands.json")

        self.generated_files["cc_db"] = [cc_db]

        # REVIEW: This will override the previous compile_commands.json
        # Is there a way to specify different compile_commands_X.json files
        # to the exporter?
        with open(cc_db, 'w') as fh:
            fh.write(compile_commands)

    def run(self) -> List[TestOutcome]:
        outcomes = []

        any_tests = any(test_fn for test_file in self.rs_test_files
                        for test_fn in test_file.test_functions)

        if not any_tests:
            description = "No tests were found..."
            logging.debug("%s:", self.name)
            logging.debug("%s [ SKIPPED ] %s %s", Colors.OKBLUE,
                          Colors.NO_COLOR, description)
            return []

        if not self.c_files:
            description = "No c files were found..."
            logging.debug("%s:", self.name)
            logging.debug("%s [ SKIPPED ] %s %s", Colors.OKBLUE,
                          Colors.NO_COLOR, description)
            return []

        sys.stdout.write("{}:\n".format(self.name))

        # .c -> .a
        description = "libtest.a: creating a static C library..."

        self.print_status(Colors.WARNING, "RUNNING", description)

        try:
            static_library = build_static_library(self.c_files, self.full_path)
        except NonZeroReturn as exception:
            self.print_status(Colors.FAIL, "FAILED", "create libtest.a")
            sys.stdout.write('\n')
            sys.stdout.write(str(exception))

            outcomes.append(TestOutcome.UnexpectedFailure)

            return outcomes

        self.generated_files["c_lib"].append(static_library)
        self.generated_files["c_obj"].extend(static_library.obj_files)

        rust_file_builder = RustFileBuilder()
        rust_file_builder.add_features(["libc", "extern_types", "simd_ffi", "stdsimd", "const_transmute"])

        # .c -> .rs
        for c_file in self.c_files:
            _, c_file_short = os.path.split(c_file.path)
            description = "{}: translating the C file into Rust...".format(
                c_file_short)

            # Run the step
            self.print_status(Colors.WARNING, "RUNNING", description)

            self._generate_cc_db(c_file.path)

            try:
                translated_rust_file = c_file.translate(extra_args=["-march=native"])
            except NonZeroReturn as exception:
                self.print_status(Colors.FAIL, "FAILED", "translate " +
                                  c_file_short)
                sys.stdout.write('\n')
                sys.stdout.write(str(exception))

                outcomes.append(TestOutcome.UnexpectedFailure)
                continue

            self.generated_files["rust_src"].append(translated_rust_file)

            _, rust_file_short = os.path.split(translated_rust_file.path)
            extensionless_rust_file, _ = os.path.splitext(rust_file_short)

            rust_file_builder.add_mod(RustMod(extensionless_rust_file,
                                              RustVisibility.Public))

        match_arms = []

        # Build one binary that can call all the tests
        for test_file in self.rs_test_files:
            _, file_name = os.path.split(test_file.path)
            extensionless_file_name, _ = os.path.splitext(file_name)

            if not test_file.pass_expected:
                try:
                    test_file.compile(CrateType.Library, save_output=False,
                                      extra_args=["-C", "target-cpu=native"])

                    self.print_status(Colors.FAIL, "OK",
                                      "Unexpected success {}".format(file_name))
                    sys.stdout.write('\n')

                    outcomes.append(TestOutcome.UnexpectedSuccess)
                except NonZeroReturn as exception:
                    self.print_status(Colors.OKBLUE, "FAILED",
                                      "Expected failure {}".format(file_name))
                    sys.stdout.write('\n')

                    logging.error("stderr:%s\n", str(exception))

                    outcomes.append(TestOutcome.Failure)

                continue

            for test_function in test_file.test_functions:
                rust_file_builder.add_mod(RustMod(extensionless_file_name,
                                                  RustVisibility.Public))
                left = "Some(\"{}::{}\")".format(extensionless_file_name,
                                                 test_function.name)
                right = "{}::{}()".format(extensionless_file_name,
                                          test_function.name)
                match_arms.append((left, right))

        match_arms.append(("e",
                           "panic!(\"Tried to run unknown test: {:?}\", e)"))

        test_main_body = [
            RustMatch("std::env::args().nth(1).as_ref().map(AsRef::<str>::as_ref)", match_arms),
        ]
        test_main = RustFunction("main",
                                 visibility=RustVisibility.Public,
                                 body=test_main_body)

        rust_file_builder.add_function(test_main)

        main_file = rust_file_builder.build(self.full_path + "/tests_main.rs")

        self.generated_files["rust_src"].append(main_file)

        # Try and build test binary
        try:
            main = main_file.compile(CrateType.Binary, save_output=True,
                                     extra_args=["-C", "target-cpu=native"])
        except NonZeroReturn as exception:
            _, main_file_path_short = os.path.split(main_file.path)

            self.print_status(Colors.FAIL, "FAILED", "compile {}".format(main_file_path_short))
            sys.stdout.write('\n')
            sys.stdout.write(str(exception))

            outcomes.append(TestOutcome.UnexpectedFailure)

            return outcomes

        self.generated_files["rust_test_exec"].append(str(main.executable))

        for test_file in self.rs_test_files:
            if not test_file.pass_expected:
                continue

            _, file_name = os.path.split(test_file.path)
            extensionless_file_name, _ = os.path.splitext(file_name)

            for test_function in test_file.test_functions:
                args = ["{}::{}".format(extensionless_file_name, test_function.name)]

                retcode, stdout, stderr = main[args].run(retcode=None)

                logging.debug("stdout:%s\n", stdout)

                test_str = file_name + ' - ' + test_function.name

                if retcode == 0:
                    if test_function.pass_expected:
                        self.print_status(Colors.OKGREEN, "OK", "    test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Success)
                    else:
                        self.print_status(Colors.FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.UnexpectedSuccess)

                elif retcode != 0:
                    if test_function.pass_expected:
                        self.print_status(Colors.FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')
                        sys.stdout.write(stderr)

                        outcomes.append(TestOutcome.UnexpectedFailure)
                    else:
                        self.print_status(Colors.OKBLUE, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Failure)

        if not outcomes:
            display_text = "   No rust file(s) matching " + self.files.pattern
            display_text += " within this folder\n"
            self.print_status(Colors.OKBLUE, "N/A", display_text)
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
                    # FIXME: Hacky. Some items are string paths,
                    # others are classes with a path attribute
                    os.remove(getattr(file_path, "path", file_path))
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


def get_testdirectories(
        directory: str, files: str,
        keep: List[str]) -> Generator[TestDirectory, None, None]:
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
    c.add_args(parser)

    args = parser.parse_args()
    c.update_args(args)
    test_directories = get_testdirectories(args.directory, args.regex_files,
                                           args.keep)
    setup_logging(args.logLevel)

    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [c.TRANSPILER]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run build_translator.py first?"
            die(msg, errno.ENOENT)

    ensure_dir(c.DEPS_DIR)
    # NOTE: it seems safe to disable this check since we now
    # that we use a rust-toolchain file for rustc versioning.
    # ensure_rustc_version(c.CUSTOM_RUST_RUSTC_VERSION)

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
            # Testdirectories are run one after another. Only test directories
            # that match the '--only-directories' or tests that match the
            # '--only-files' arguments are run.  We make a best effort to clean
            # up files we left behind.
            try:
                statuses = test_directory.run()
            except (KeyboardInterrupt, SystemExit):
                test_directory.cleanup()
                raise
            finally:
                test_directory.cleanup()

            for status in statuses:
                test_results[status.value] += 1

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
