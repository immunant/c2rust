#!/usr/bin/env python3

import os
import plumbum as pb

from common import *

# Tools we will need
clang = get_cmd_or_die("rustfmt")
rustc = get_cmd_or_die("diff")


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


def run_tests(testcases: List[str]):
    ipath = os.path.join(RREF_DIR, "target/debug/idiomize")
    # refactor = '{ip} -P ../.. -p plugin_stub -r alongside'.format(ip=ipath)
    # NOTE:PL: I removed the plugin options (-P, -p) to get the tests to run.
    refactor = '{ip} -r alongside'.format(ip=ipath)

    # help plumbum find rust
    ld_lib_path = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)
    if 'LD_LIBRARY_PATH' in pb.local.env:
        ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

    rustflags = "-L {rust_lib_dir}/rustlib/{triplet}/lib"
    rustflags = rustflags.format(rust_lib_dir=ld_lib_path,
                                 triplet=get_host_triplet())

    with pb.local.env(RUST_BACKTRACE='1',
                      RUST_LOG="idiomize=info",                      
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
                    script.run()
                    print("[ OK ] " + testname)
            except pb.ProcessExecutionError as pee:
                print("[FAIL] " + testname)
                # print(pee.stderr)
                # quit(1)

def main():
    # TODO: implement rustfmt and diff actions from `run-test.sh`
    # TODO: check rustfmt version    

    ensure_rustc_version(CUSTOM_RUST_RUSTC_VERSION)
    test_dir = os.path.join(RREF_DIR, "tests")
    testcases = get_testcases(test_dir)
    run_tests(testcases)


if __name__ == "__main__":
    main()
