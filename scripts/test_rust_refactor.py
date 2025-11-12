#!/usr/bin/env -S uv run

import os
import logging
from typing import List

import plumbum as pb

from common import (
    config as c,
    Colors,
    get_cmd_or_die,
    get_rust_toolchain_libpath,
    get_host_triplet,
    setup_logging,
    die,
)

# Tools we will need
rustfmt = get_cmd_or_die("rustfmt")
diff = get_cmd_or_die("diff")


def get_testcases(directory: str) -> List[str]:
    """
    Find the test cases in a directory
    """

    testcases = []
    scriptnam = "run.sh"

    for root, subdirs, files in os.walk(directory):
        if scriptnam in files:
            testcases.append(os.path.join(root, scriptnam))

    return testcases


def run_tests(testcases: List[str]) -> None:
    ipath = os.path.join(c.ROOT_DIR, "target/debug/c2rust-refactor")
    # refactor = '{ip} -P ../.. -p plugin_stub -r alongside'.format(ip=ipath)
    # NOTE:PL: I removed the plugin options (-P, -p) to get the tests to run.
    refactor = '{ip} -r alongside'.format(ip=ipath)

    # help plumbum find rust
    ld_lib_path = get_rust_toolchain_libpath()
    if 'LD_LIBRARY_PATH' in pb.local.env:
        ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

    rustflags = "-L {rust_lib_dir}/rustlib/{triplet}/lib"
    rustflags = rustflags.format(rust_lib_dir=ld_lib_path,
                                 triplet=get_host_triplet())

    with pb.local.env(RUST_BACKTRACE='1',
                      RUST_LOG="c2rust_refactor=info",
                      LD_LIBRARY_PATH=ld_lib_path,
                      not_LD_LIBRARY_PATH=ld_lib_path,
                      refactor=refactor,
                      rustflags=rustflags):
        for test in testcases:
            script = pb.local.get(test)
            testdir = os.path.dirname(test)
            testname = os.path.basename(testdir)
            try:
                with pb.local.cwd(testdir):
                    logging.debug("testing: %s", testdir)
                    script.run()

                    old_new_rust = os.path.join(testdir, "old.rs.new")
                    assert os.path.isfile(old_new_rust), "missing rewritten rust"
                    mode = "overwrite"  # set to 'replace' to generate backups
                    rustfmt["--force", "--write-mode", mode, old_new_rust].run()

                    new_rust = os.path.join(testdir, "new.rs")
                    diff["-wB", new_rust, old_new_rust].run()

                    print(" {}[ OK ]{} ".format(Colors.OKGREEN, Colors.NO_COLOR) + testname)
                    logging.debug(" [ OK ] " + testname)
            except pb.ProcessExecutionError as pee:
                print(" {}[FAIL]{} ".format(Colors.FAIL, Colors.NO_COLOR) + testname)
                logging.debug(" [FAIL] " + testname)
                logfile = os.path.join(testdir, "log")
                if os.path.exists(logfile):
                    with open(logfile, "r") as fh:
                        lines = fh.readlines()
                        logging.debug("".join(lines))


def main() -> None:
    # TODO: implement rustfmt and diff actions from `run-test.sh`

    setup_logging()
    # NOTE: it seems safe to disable this check since we now
    # that we use a rust-toolchain.toml file for rustc versioning.
    # ensure_rustc_version(c.CUSTOM_RUST_RUSTC_VERSION)
    # TODO: update rustfmt version check once c2rust-refactor bitrot has been fixed
    # ensure_rustfmt_version()
    test_dir = os.path.join(c.RREF_DIR, "tests")
    assert os.path.isdir(test_dir), "test dir missing: " + test_dir
    refactor_binary = os.path.join(c.ROOT_DIR, "target/debug/c2rust-refactor")
    if not os.path.isfile(refactor_binary):
        die("build refactor binary first. expected: " + refactor_binary)

    testcases = get_testcases(test_dir)
    run_tests(sorted(testcases))


if __name__ == "__main__":
    main()
