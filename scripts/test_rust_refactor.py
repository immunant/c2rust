#!/usr/bin/env python3

import os
import plumbum as pb

from common import *

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
                    logging.debug("testing: %s", testdir)
                    script.run()

                    old_new_rust = os.path.join(testdir, "old.rs.new")
                    assert os.path.isfile(old_new_rust), "missing rewritten rust"
                    rustfmt["--force", old_new_rust].run()
                    
                    new_rust = os.path.join(testdir, "new.rs")
                    diff["-wB", new_rust, old_new_rust].run()

                    print(" {}[ OK ]{} ".format(OKGREEN, NO_COLOUR) + testname)
                    logging.debug(" [ OK ] " + testname)
            except pb.ProcessExecutionError as pee:
                print(" {}[FAIL]{} ".format(FAIL, NO_COLOUR) + testname)
                logging.debug(" [FAIL] " + testname)
                logging.debug(pee.stderr)
                # print(pee.stderr)
                # quit(1)

def main():
    # TODO: implement rustfmt and diff actions from `run-test.sh`
    # TODO: check rustfmt version   
     
    setup_logging()
    ensure_rustc_version(CUSTOM_RUST_RUSTC_VERSION)
    ensure_rustfmt_version()
    test_dir = os.path.join(RREF_DIR, "tests")
    assert os.path.isdir(test_dir), "test dir missing: " + test_dir
    idiomize_binary = os.path.join(RREF_DIR, "target/debug/idiomize")
    if not os.path.isfile(idiomize_binary):
        die("build idiomize binary first. expected: " + idiomize_binary)
    
    testcases = get_testcases(test_dir)
    run_tests(sorted(testcases))


if __name__ == "__main__":
    main()
