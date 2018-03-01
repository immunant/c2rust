#!/usr/bin/env python3

import os
import sys
import logging
import argparse
import re

from common import *
from enum import Enum
from rust_file import RustFile, RustFunction, RustMod, RustUse, RustVisibility
from typing import Generator, List, Optional, Tuple

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
    'cc_db', 'cbor', 'c_exec', 'c_out',
    'rust_src', 'rust_obj', 'rust_exec', 'rust_out',
    'rust_test_obj', 'rust_test_exec',
]


class TestOutcome(Enum):
    Success = "successes"
    Failure = "expected failures"
    UnexpectedFailure = "unexpected failures"
    UnexpectedSuccess = "unexpected successes"


class TestDirectory:
    def __init__(self, full_path: str, keep: List[str]) -> None:
        self.c_files = []
        self.rs_test_files = {}
        self.full_path = full_path
        self.name = full_path.split('/')[-1]
        self.keep = keep
        self.generated_files = {
            "rust_src": [],
            "cbor": [],
            "rust_obj": [],
            "rust_exec": [],
            "c_exec": [],
            "rust_out": [],
            "c_out": [],
            "rust_test_obj": [],
            "rust_test_exec": [],
            "cc_db": [],
        }

        for entry in os.listdir(full_path):
            path = os.path.abspath(os.path.join(full_path, entry))

            if os.path.isfile(path):
                extensionless_path, ext = os.path.splitext(path)

                if ext == ".c":
                    self.c_files.append(path)
                elif ext == ".rs":
                    with open(path, 'r') as file:
                        self.rs_test_files[path] = re.findall(r"fn (test_\w+)\(\)", file.read())

        print(self.rs_test_files) # TODO: Remove me

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

    def _export_cbor(self, c_file_path: str) -> Tuple[int, str, str]:
        self._generate_cc_db(c_file_path)
        self.generated_files["cbor"].append(c_file_path + ".cbor")

        # run the extractor
        args = [c_file_path]
        # NOTE: it doesn't seem necessary to specify system include
        # directories and in fact it may cause problems on macOS.
        ## make sure we can locate system include files
        ## sys_incl_dirs = get_system_include_dirs()
        ## args += ["-extra-arg=-I" + i for i in sys_incl_dirs]
        # log the command in a format that's easy to re-run
        logging.debug("extraction command:\n %s", str(ast_extractor[args]))
        return ast_extractor[args].run(retcode=None)

    def _translate(self, cbor_file_path: str) -> Tuple[int, str, str]:
        c_file_path, _ = os.path.splitext(cbor_file_path)
        extensionless_file, _ = os.path.splitext(c_file_path)
        rust_src = extensionless_file + ".rs"

        self.generated_files["rust_src"].append(rust_src)

        # help plumbum find rust
        ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # run the importer
        args = [cbor_file_path]
        with pb.local.env(RUST_BACKTRACE='1', LD_LIBRARY_PATH=ld_lib_path):
            # log the command in a format that's easy to re-run
            translation_cmd = "LD_LIBRARY_PATH=" + ld_lib_path + " \\\n"
            translation_cmd += str(ast_importer[args] > rust_src)
            logging.debug("translation command:\n %s", translation_cmd)
            return (ast_importer[args] > rust_src).run(retcode=None)

    def _compile_rustc(self, rust_src_path: str, crate_type: str) -> Tuple[int, str, str]:
        extensionless_file, _ = os.path.splitext(rust_src_path)
        rust_obj = extensionless_file

        if crate_type == "staticlib":
            rust_obj += ".a"
            self.generated_files["rust_obj"].append(rust_obj)
        elif crate_type == "bin":
            self.generated_files["rust_exec"].append(rust_obj)
        else:
            self.generated_files["rust_obj"].append(rust_obj)

        # run rustc
        args = [
            f'--crate-type={crate_type}',  # could just as well be 'cdylib'
            '-o', rust_obj,
            rust_src_path,
        ]
        # log the command in a format that's easy to re-run
        logging.debug("rustc compile command: %s", str(rustc[args]))
        return rustc[args].run(retcode=None)

    def run(self) -> TestOutcome:
        outcomes = []

        # .c -> .c.cbor
        for c_file in self.c_files:
            _, c_file_short = os.path.split(c_file)
            description = f"{c_file_short}: extracting the C file into CBOR"

            # Run the step
            self.print_status(WARNING, "RUNNING", description + "...")

            retcode, stdout, stderr = self._export_cbor(c_file)

            # print("Ret:", retcode)
            # print("Stdout:", stdout)
            # print("Stderr:", stderr)
            # print("---------------")

        # .cbor -> .rs
        for cbor_file in self.generated_files["cbor"]:
            _, cbor_file_short = os.path.split(cbor_file)
            description = f"{cbor_file_short}: import and translate the CBOR"

            self.print_status(WARNING, "RUNNING", description + "...")

            retcode, stdout, stderr = self._translate(cbor_file)

            # print("Ret:", retcode)
            # print("Stdout:", stdout)
            # print("Stderr:", stderr)
            # print("---------------")

        pub_mods = []

        # .rs -> .a
        for rust_file in self.generated_files["rust_src"]:
            _, rust_file_short = os.path.split(rust_file)
            extensionless_rust_file, _ = os.path.splitext(rust_file_short)
            description = f"{rust_file_short}: compile the generated Rust"

            pub_mods.append(RustMod(extensionless_rust_file, RustVisibility.Public))

            self.print_status(WARNING, "RUNNING", description + "...")

            retcode, stdout, stderr = self._compile_rustc(rust_file, "staticlib")

            # print("Ret:", retcode)
            # print("Stdout:", stdout)
            # print("Stderr:", stderr)
            # print("---------------")

        for file_path, test_names in self.rs_test_files.items():
            path, file_name = os.path.split(file_path)
            extensionless_file_name, _ = os.path.splitext(file_name)

            for test_name in test_names:
                features = ["libc", "i128_type"]
                mods = pub_mods + [RustMod(extensionless_file_name, RustVisibility.Public)]
                uses = [RustUse([extensionless_file_name, test_name])]
                functions = [RustFunction("main", RustVisibility.Public, [f"{test_name}();\n"])]

                main = RustFile(features, mods, uses, functions)

                main_src_path = os.path.join(self.full_path, test_name + "_main.rs")
                main_bin_path = os.path.join(self.full_path, test_name + "_main")

                self.generated_files["rust_src"].append(main_src_path)
                self.generated_files["rust_test_exec"].append(main_bin_path)

                with open(main_src_path, 'w') as fh:
                    fh.write(str(main))

                retcode, stdout, stderr = self._compile_rustc(main_src_path, "bin")

                # FIXME:
                pass_expected = True

                # print("Ret:", retcode)
                # print("Stdout:", stdout)
                # print("Stderr:", stderr)
                # print("---------------")
                if retcode != 0 and pass_expected:
                    self.print_status(FAIL, "FAILED", "test " + file_name + ' - ' + test_name)
                    sys.stdout.write('\n')
                    sys.stdout.write(stderr)

                    outcomes.append(TestOutcome.UnexpectedFailure)
                    continue

                # This will fail is previous section failed: (duh)
                main = get_cmd_or_die(main_bin_path)
                retcode, stdout, stderr = main.run(retcode=None)

                test_str = file_name + ' - ' + test_name

                if retcode == 0:
                    if pass_expected:
                        self.print_status(OKGREEN, "OK", "    test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Success)
                    else:
                        self.print_status(FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.UnexpectedSuccess)

                elif retcode != 0:
                    if pass_expected:
                        self.print_status(FAIL, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')
                        sys.stdout.write(stderr)

                        outcomes.append(TestOutcome.UnexpectedFailure)
                    else:
                        self.print_status(OKBLUE, "FAILED", "test " + test_str)
                        sys.stdout.write('\n')

                        outcomes.append(TestOutcome.Failure)

                # Ret 101 is panic + stderr
                # Ret 0 is ok
                # print("Ret:", retcode)
                # print("Stdout:", stdout)
                # print("Stderr:", stderr)
                # print("---------------")

        # print('\n')
        # print(self.generated_files)

        assert outcomes, "No valid test outcomes were determined"
        return outcomes

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

                sys.stdout.write('\033[1000D\033[K\r')
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
            sys.stdout.write('\033[1000D\033[K\r')
            logging.info("Expected success for " + self.shortname)

        if bool(failed_message) == self.pass_expected:
            self.print_status(FAIL, "FAILED",
                              failed_message or "(unexpected success)")
            self.status = "unexpected failures" if failed_message else \
                          "unexpected successes"
        elif failed_message:
            self.print_status(OKBLUE, "FAILED", failed_message + " (expected)")
            self.status = "expected failures"
        else:
            self.print_status(OKGREEN, "OK", None)
            self.status = "successes"

        sys.stdout.write("\n")

    def cleanup(self) -> None:
        if "all" in self.keep:
            return

        for file_type, file_paths in self.generated_files.items():
            if file_type in self.keep:
                continue

            # Try remove files and don't barf if they don't exist
            for file_path in file_paths:
                try:
                    os.remove(file_path)
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


def get_testdirectories(directory: str, keep: List[str]) -> Generator[TestDirectory, None, None]:
    for entry in os.listdir(directory):
        path = os.path.abspath(os.path.join(directory, entry))

        if os.path.isdir(path):
            yield TestDirectory(path, keep)


def main() -> None:
    desc = 'run regression / unit / feature tests.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('directory', type=readable_directory)
    parser.add_argument(
        '--only-file', dest='regex_file', type=regex,
        default='.*', help="Regular expression to filter which tests to run"
    )
    parser.add_argument(
        '--only-directory', dest='regex_directory', type=regex,
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
    test_directories = get_testdirectories(args.directory, args.keep)
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
        if args.regex_directory.fullmatch(test_directory.name):
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
    unepected = \
        test_results["unexpected failures"] + \
        test_results["unexpected successes"]
    if 0 < unepected:
        quit(1)


if __name__ == "__main__":
    main()
