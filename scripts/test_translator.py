#!/usr/bin/env -S uv run

import errno
import os
from pathlib import Path
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
    on_mac,
)
from enum import Enum
from rust_file import (
    CrateType,
    RustFile,
    RustFileBuilder,
    RustMod,
    RustVisibility,
)
from typing import Any, Dict, Generator, List, Optional, Set, Iterable

# Tools we will need
clang = get_cmd_or_die("clang")
rustc = get_cmd_or_die("rustc")
diff = get_cmd_or_die("diff")
ar = get_cmd_or_die("ar")
cargo = get_cmd_or_die("cargo")


# Intermediate files
intermediate_files = [
    'cc_db', 'c_obj', 'c_lib', 'rust_src',
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
    def __init__(self, log_level: str, path: str, flags: Set[str] = set()) -> None:

        self.log_level = log_level
        self.path = path
        self.disable_incremental_relooper = "disable_incremental_relooper" in flags
        self.disallow_current_block = "disallow_current_block" in flags
        self.translate_const_macros = "translate_const_macros" in flags
        self.reorganize_definitions = "reorganize_definitions" in flags
        self.emit_build_files = "emit_build_files" in flags

    def translate(self, cc_db: str, ld_lib_path: str, extra_args: List[str] = []) -> RustFile:
        extensionless_file, _ = os.path.splitext(self.path)

        # run the transpiler
        transpiler = get_cmd_or_die(c.TRANSPILER)

        args = [
            cc_db,
            "--prefix-function-names",
            "rust_",
            "--overwrite-existing",
        ]

        # return nonzero if translation fails
        args.append("--fail-on-error")

        if self.disable_incremental_relooper:
            args.append("--no-incremental-relooper")
        if self.disallow_current_block:
            args.append("--fail-on-multiple")
        if self.translate_const_macros:
            args.append("--translate-const-macros")
            args.append("experimental")
        if self.reorganize_definitions:
            args.append("--reorganize-definitions")
        if self.emit_build_files:
            args.append("--emit-build-files")

        if self.log_level == 'DEBUG':
            args.append("--log-level=debug")

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
            logging.debug("stderr:\n%s", stderr)

        if retcode != 0:
            raise NonZeroReturn(stderr)

        return RustFile(extensionless_file + ".rs")


def get_native_arch() -> str:
    rustc_cfg_args = ["--print", "cfg"]
    retcode, stdout, stderr = rustc[rustc_cfg_args].run(retcode=None)
    for line in stdout.split("\n"):
        if line.startswith("target_arch"):
            return line.split("=")[1].replace('"', '')
    raise KeyError


def rustc_has_target(target: str) -> bool:
    args = ["--target", target, "--print", "target-libdir"]
    stdout = rustc[args]()
    target_libdir = Path(stdout.strip())
    return target_libdir.exists()


def target_args(target: Optional[str]) -> List[str]:
    if target:
        return ["-target", target]
    else:
        return ["-march=native"]


def build_static_library(c_files: Iterable[CFile],
                         output_path: str,
                         target: Optional[str]) -> Optional[CStaticLibrary]:
    current_path = os.getcwd()

    os.chdir(output_path)

    # create .o files
    args = ["-c", "-fPIC", "-Wno-error=int-conversion"]
    args += target_args(target)
    paths = [c_file.path for c_file in c_files]

    if len(paths) == 0:
        return None
    else:
        args += paths

    logging.debug("compilation command:\n %s", str(clang[args]))
    retcode, stdout, stderr = clang[args].run(retcode=None)

    logging.debug("stdout:\n%s", stdout)

    if retcode != 0:
        raise NonZeroReturn(stderr)

    args = ["-rv", "libtest.a"]
    obj_files = []

    for c_file in c_files:
        extensionless_file_path, _ = os.path.splitext(c_file.path)
        extensionless_file_name = os.path.basename(extensionless_file_path)
        obj_file_path = os.path.join(output_path, extensionless_file_name + ".o")

        args.append(obj_file_path)
        obj_files.append(obj_file_path)

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
    def __init__(self, path: str, test_functions: Optional[List[TestFunction]] = None,
                 flags: Optional[Set[str]] = None) -> None:
        if not flags:
            flags = set()

        super().__init__(path)

        self.test_functions = test_functions or []
        self.pass_expected = "xfail" not in flags
        self.extern_crates = {flag[13:] for flag in flags if flag.startswith("extern_crate_")}
        self.features = {flag[8:] for flag in flags if flag.startswith("feature_")}


class TestDirectory:
    rs_test_files: list[TestFile]

    def __init__(self, full_path: str, files: 're.Pattern', keep: List[str], log_level: str) -> None:
        self.c_files = []
        self.rs_test_files = []
        self.full_path = full_path
        self.full_path_src = os.path.join(full_path, "src")
        self.files = files
        self.name = os.path.basename(full_path)
        self.keep = keep
        self.log_level = log_level
        self.generated_files: Dict[str, List[Any]] = {
            "rust_src": [],
            "c_obj": [],
            "c_lib": [],
            "cc_db": [],
        }

        # if the test is arch-specific, check if we can run it natively; if not,
        # set self.target to a known-working target tuple for it
        self.target = None

        # include the compiler resource directory in compile_commands.json.
        # we should never have to do this but for some reason SIMD includes
        # are broken without it on macOS 12.
        # limit this to macOS because if we do happen to have multiple versions of Clang around, we
        # don't know which to use here, and using the wrong can one break things badly
        if sys.platform == "darwin":
            _, stdout, _ = clang["-print-resource-dir"].run(retcode=None)
            self.clang_resource_dir = " \"-I{}/include\",".format(stdout.strip())
        else:
            self.clang_resource_dir = ""

        # parse target arch from directory name if it includes a dot
        split_by_dots = self.name.split('.')
        if len(split_by_dots) > 1:
            target_arch = split_by_dots[-1]
            # if native and target arch differ, cross-compile to specific target
            if target_arch != get_native_arch():
                with open(self.full_path + "/target-tuple", 'r', encoding="utf-8") as file:
                    self.target = file.read().strip()

        for entry in os.listdir(self.full_path_src):
            path = os.path.abspath(os.path.join(self.full_path_src, entry))

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

        with open(path, 'r', encoding="utf-8") as file:
            file_config = re.match(r"//! (.*)\n", file.read())

        if file_config:
            flag_str = file_config.group(0)[3:]
            file_flags = {flag.strip() for flag in flag_str.split(',')}

        if "skip_translation" in file_flags:
            return None

        return CFile(self.log_level, path, file_flags)

    def _read_rust_test_file(self, path: str) -> TestFile:
        with open(path, 'r', encoding="utf-8") as file:
            file_buffer = file.read()

        file_config = re.match(r"//! (.*)\n", file_buffer)
        file_flags = set()

        if file_config:
            flags_str = file_config.group(0)[3:]
            file_flags = {flag.strip() for flag in flags_str.split(',')}

        found_tests = re.findall(
            r"(//(.*))?\n\s*pub fn (test_\w+)\(\)", file_buffer)
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

        target_args = '"-target", "{}", '.format(self.target) if self.target else ""

        compile_commands = """ \
        [
          {{
            "arguments": [ "cc", "-D_FORTIFY_SOURCE=0",{3} "-c", {2}"{0}" ],
            "directory": "{1}",
            "file": "{0}"
          }}
        ]
        """.format(cfile, directory, target_args, self.clang_resource_dir)

        cc_db = os.path.join(directory, "compile_commands.json")

        self.generated_files["cc_db"] = [cc_db]

        # REVIEW: This will override the previous compile_commands.json
        # Is there a way to specify different compile_commands_X.json files
        # to the exporter?
        with open(cc_db, 'w') as fh:
            fh.write(compile_commands)

    def run(self) -> List[TestOutcome]:
        if self.target and not rustc_has_target(self.target):
            self.print_status(Colors.OKBLUE, "SKIPPED",
                              "building test {} because the {} target is not installed"
                              .format(self.name, self.target))
            sys.stdout.write('\n')
            return []

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
            static_library = build_static_library(self.c_files, self.full_path, self.target)
        except NonZeroReturn as exception:
            self.print_status(Colors.FAIL, "FAILED", "create libtest.a")
            sys.stdout.write('\n')
            sys.stdout.write(str(exception))

            outcomes.append(TestOutcome.UnexpectedFailure)

            return outcomes

        assert static_library is not None  # for mypy

        self.generated_files["c_lib"].append(static_library)
        self.generated_files["c_obj"].extend(static_library.obj_files)

        rust_file_builder = RustFileBuilder()
        rust_file_builder.add_features([
            "extern_types",
            "simd_ffi",
            "stdsimd",
            "linkage",
            "register_tool",
        ])
        rust_file_builder.add_pragma("register_tool", ["c2rust"])

        # Ensure that path to rustc's lib dir is in`LD_LIBRARY_PATH`
        ld_lib_path = get_rust_toolchain_libpath()
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # .c -> .rs
        for c_file in self.c_files:
            _, c_file_short = os.path.split(c_file.path)
            description = "{}: translating the C file into Rust...".format(
                c_file_short)

            # Run the step
            self.print_status(Colors.WARNING, "RUNNING", description)

            self._generate_cc_db(c_file.path)

            try:
                logging.debug("translating %s", c_file_short)
                translated_rust_file = c_file.translate(self.generated_files["cc_db"][0],
                                                        ld_lib_path,
                                                        extra_args=target_args(self.target))
            except NonZeroReturn as exception:
                self.print_status(Colors.FAIL, "FAILED", "translate " +
                                  c_file_short)
                sys.stdout.write('\n')
                sys.stdout.write(str(exception))

                outcomes.append(TestOutcome.UnexpectedFailure)
                continue

            self.generated_files["rust_src"].append(translated_rust_file)
            if c_file.emit_build_files:
                self.generated_files["rust_src"].append(self.full_path + "/src/Cargo.toml")
                self.generated_files["rust_src"].append(self.full_path + "/src/build.rs")
                self.generated_files["rust_src"].append(self.full_path + "/src/c2rust-lib.rs")
                self.generated_files["rust_src"].append(self.full_path + "/src/rust-toolchain.toml")

            _, rust_file_short = os.path.split(translated_rust_file.path)
            extensionless_rust_file, _ = os.path.splitext(rust_file_short)

            rust_file_builder.add_mod(RustMod(extensionless_rust_file,
                                              RustVisibility.Public))

        rustc_extra_args = ["-C", "target-cpu=native"]

        # Build one binary that can call all the tests
        for test_file in self.rs_test_files:
            # rustc_extra_args.append(["-L", "crate={}".format(c.TARGET_DIR)])
            rust_file_builder.add_features(test_file.features)
            rust_file_builder.add_extern_crates(test_file.extern_crates)

            _, file_name = os.path.split(test_file.path)
            extensionless_file_name, _ = os.path.splitext(file_name)
            if test_file.pass_expected:
                rust_file_builder.add_mod(RustMod(extensionless_file_name, RustVisibility.Private))

            if not test_file.pass_expected:
                try:
                    test_file.compile(CrateType.Library, save_output=False,
                                      extra_args=rustc_extra_args)

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

        lib_file = rust_file_builder.build(self.full_path + "/src/lib.rs")

        self.generated_files["rust_src"].append(lib_file)

        # Copy `generated-rust-toolchain.toml`.
        # We `c2rust-transpile` `*.c` files individually, so `--emit-build-files` doesn't work
        # (if it's generated, it's in the wrong directory and may be different for each transpiled file).
        # We could also change things to transpile all `*.c` files at once, but that's more involved.
        generated_rust_toolchain = Path(c.TRANSPILE_CRATE_DIR) / "src/build_files/generated-rust-toolchain.toml"
        rust_toolchain = Path(self.full_path) / "rust-toolchain.toml"
        rust_toolchain.unlink(missing_ok=True)
        rust_toolchain.symlink_to(generated_rust_toolchain)
        self.generated_files["rust_src"].append(str(rust_toolchain))

        # Build
        with pb.local.cwd(self.full_path):
            args = ["build"]

            if c.BUILD_TYPE == 'release':
                args.append('--release')

            if self.target:
                args += ["--target", self.target]

            retcode, stdout, stderr = cargo[args].run(retcode=None)

        if retcode != 0:
            _, lib_file_path_short = os.path.split(lib_file.path)

            self.print_status(Colors.FAIL, "FAILED", "compile {}".format(lib_file_path_short))
            sys.stdout.write('\n')
            sys.stdout.write(stderr)

            outcomes.append(TestOutcome.UnexpectedFailure)

            return outcomes

        # Test
        with pb.local.cwd(self.full_path):
            args = ["test"]

            if c.BUILD_TYPE == 'release':
                args.append('--release')

            if self.target:
                args += ["--target", self.target]

            retcode, stdout, stderr = cargo[args].run(retcode=None)
        
        if retcode != 0:
            _, lib_file_path_short = os.path.split(lib_file.path)

            self.print_status(Colors.FAIL, "FAILED", "test {}".format(lib_file_path_short))
            sys.stdout.write('\n')
            sys.stdout.write(stdout)
        else:
            for line in stdout.split("\n"):
                if "... ok" in line:
                    self.print_status(Colors.OKGREEN, "OK", "{}".format(line))
                    sys.stdout.write('\n')
        
        # Don't distinguish between expected and unexpected failures.
        # `#[should_panic]` is used for that instead of `// xfail` now.
        # Also, `cargo test -- --format json` is unstable, so it's easier to just parse very simply.
        for _ in range(stdout.count("... ok")):
            outcomes.append(TestOutcome.Success)
        for _ in range(stdout.count("... FAILED")):
            outcomes.append(TestOutcome.UnexpectedFailure)

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
        directory: str,
        files: 're.Pattern',
        keep: List[str],
        log_level: str,
) -> Generator[TestDirectory, None, None]:
    dir = Path(directory)
    for path in dir.iterdir():
        if path.is_dir():
            if path.name == "longdouble" and on_mac():
                continue
            yield TestDirectory(str(path.absolute()), files, keep, log_level)


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
        '--log', dest='log_level',
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
    test_directories = get_testdirectories(args.directory,
                                           args.regex_files,
                                           args.keep,
                                           args.log_level)
    setup_logging(args.log_level)

    logging.debug("args: %s", " ".join(sys.argv))

    # check that the binaries have been built first
    bins = [c.TRANSPILER]
    for b in bins:
        if not os.path.isfile(b):
            msg = b + " not found; run cargo build --release first?"
            die(msg, errno.ENOENT)

    # NOTE: it seems safe to disable this check since we now
    # that we use a rust-toolchain.toml file for rustc versioning.
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
