#!/usr/bin/env -S uv run

"""
Script for extracting defs on which rewriting succeeded.  Currently, this is
specific to lighttpd_rust_amalgamated, but it could be generalized to work on
other projects as well.  The script identifies "working defs" by reading the
log of a `c2rust-analyze` run and collecting defs that have no reported
"analysis failed" errors and are not listed in the `--fixed-defs-list`.  It
then locates each working def in the rewritten code by searching for "start/end
of def" comments and prints the code for each working def to stdout.
"""

from dataclasses import dataclass
import re
import subprocess
import sys

ANALYSIS_FAILED_RE = re.compile('analysis of (DefId\([^)]*\)) failed:.*$')
DEF_SPAN_RE = re.compile('(DefId\([^)]*\)) @ (.*)$')
# src/main.rs:422:1: 422:49 (#0)
SPAN_RE = re.compile('([^:]+):([0-9]+):([0-9]+): ([0-9]+):([0-9]+) \(#[0-9]+\)')

START_OF_DEF_RE = re.compile(' *// [0-9]*: [^:]*: start of def (DefId\([^)]*\))$')
END_OF_DEF_RE = re.compile(' *// [0-9]*: [^:]*: end of def (DefId\([^)]*\))$')

HEADER = '''
#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

extern crate libc;
extern crate core;

pub type __uint32_t = libc::c_uint;
pub type __intmax_t = libc::c_long;
pub type __uintmax_t = libc::c_ulong;
pub type size_t = libc::c_ulong;
pub type uint32_t = __uint32_t;
pub type uint_fast32_t = libc::c_ulong;
pub type intmax_t = __intmax_t;
pub type uintmax_t = __uintmax_t;

#[no_mangle]
#[cold]
#[c2rust_analyze_test::fixed_signature]
pub extern "C" fn ck_assert_failed(
) -> ! {
    panic!();
}
'''

def main():
    fixed_defs_path, log_path = sys.argv[1:]

    error_summary = subprocess.run(('grep', '-A999999', '^error summary:$', log_path),
        check=True, capture_output=True, text=True).stdout
    error_defs = set()
    for line in error_summary.splitlines():
        line = line.strip()
        m = ANALYSIS_FAILED_RE.match(line)
        if m is not None:
            error_defs.add(m.group(1))
        else:
            print('bad line %r' % (line,), file=sys.stderr)

    unfixed_defs = set()
    for line in open(fixed_defs_path):
        line = line.strip()
        if line.startswith('#'):
            line = line[1:].strip()
            print('unfixed def: %r' % line, file=sys.stderr)
            unfixed_defs.add(line)

    rewritten_defs = unfixed_defs - error_defs

    f = open('../../lighttpd-rust/lighttpd_rust_amalgamated/src/main.new.rs')
    lines = list(f)
    start_of_def = {}
    end_of_def = {}
    last_non_prefix_line = 0
    prev_last_non_prefix_line = 0
    for i, line in enumerate(lines):
        m = START_OF_DEF_RE.match(line)
        if m is not None:
            start_of_def[m.group(1)] = prev_last_non_prefix_line + 1
        m = END_OF_DEF_RE.match(line)
        if m is not None:
            end_of_def[m.group(1)] = i
        line = line.strip()
        if not line.startswith('//') and not line.startswith('#'):
            prev_last_non_prefix_line = last_non_prefix_line
            last_non_prefix_line = i


    print(HEADER)

    for did in sorted(rewritten_defs):
        print('\n// BEGIN %s' % did)
        if did not in start_of_def:
            continue
        start = start_of_def[did]
        end = end_of_def[did] + 1
        in_prefix_comments = True
        for line in lines[start:end]:
            if in_prefix_comments:
                if not line.strip().startswith('//'):
                    in_prefix_comments = False
                else:
                    continue
            sys.stdout.write(line)
        print('// END %s' % did)

    print('extracted %d working defs:' % len(rewritten_defs), file=sys.stderr)
    for did in sorted(rewritten_defs):
        print(did, file=sys.stderr)

if __name__ == '__main__':
    main()
