#!/usr/bin/python

import subprocess
import filecmp

# libfakechecks is for testing of hash checks inserts only.
# LIBRARY_PATH should be changed if we intend to use
# a different shared library (i.e. libclevrbuf.so)
LIBRARY_PATH = "../../../libfakechecks"

# Build libfakechecks shared library
LIBRARY_BUILD = subprocess.Popen(["make"],
                                 stderr=subprocess.STDOUT, cwd="../../../libfakechecks")
LIBRARY_BUILD.wait()

# Build the clang plugin for testing in clang-plugin/ folder
CLANG_PLUGIN_BUILD = subprocess.Popen(["make"],
                                      stderr=subprocess.STDOUT, cwd="../../clang-plugin")
CLANG_PLUGIN_BUILD.wait()

# Build the gcc instrumentation method for testing in cyg-profile/ folder
GCC_INSTRUMENTATION_BUILD = subprocess.Popen(["make fakecheckoutput"],
                                             stderr=subprocess.STDOUT, stdout=subprocess.PIPE,
                                             cwd="../../cyg-profile",shell=True)
GCC_INSTRUMENTATION_BUILD.wait()

# Run the gcc instrumented executable and dump the output in a txt
subprocess.call(
    "LD_PRELOAD=../../libfakechecks/libfakechecks.so ./fakecheckoutput > fakecheckoutput.txt 2>&1",
    cwd="../../cyg-profile",
    shell=True)

# Run the executable generated from clang plugin
subprocess.call(
    "./CrossChecker ../../cyg-profile/test.c > crosschecked.c", cwd="../build", shell=True)

# Compile the source after transforming the source with cross-checks by setting environent
# variable pointing to location of shared library
CROSS_CHECK_COMPILE = subprocess.Popen('gcc -o verifycrosscheck crosschecked.c  -Wl,-rpath' ' '
                                       + LIBRARY_PATH + ' ' ' ' '-L'
                                       + LIBRARY_PATH + ' '  '-lfakechecks', cwd="../build",
                                       shell=True)
CROSS_CHECK_COMPILE.wait()

# Run the cross-check executable
RUN_CROSS_CHECK = subprocess.Popen(
    './verifycrosscheck  > plugin_output.txt 2>&1', cwd="../build", shell=True)
RUN_CROSS_CHECK.wait()

# Comparing the cross checks - function instrumentation of both clang
# plugin and gcc output
if filecmp.cmp('../../cyg-profile/fakecheckoutput.txt', '../build/plugin_output.txt', shallow=False):
    print "Test Passed:crosschecks inserted by both methods match"
else:
    print "Test Failed:crosschecks inserted do not match"
