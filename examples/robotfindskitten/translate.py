#!/usr/bin/env -S uv run

import argparse
import json
import hashlib
import os
import shlex
import shutil
import sys
from plumbum.cmd import mv, mkdir, sed, rustc, cargo, rm
from plumbum import local, FG

# Path to the root of the robotfindskitten codebase
RFK_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), 'repo'))
COMPILE_COMMANDS = os.path.join(RFK_DIR, 'compile_commands.json')

sys.path.append(os.path.join(RFK_DIR, '../../../scripts'))
from common import *

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--no-build', default=False, action='store_true',
            help='assume compile_commands.json is already available, '
                'instead of building the project to produce it')
    config.add_args(ap)
    args = ap.parse_args()
    config.update_args(args)

    os.chdir(RFK_DIR)

    # Build robotfindskitten, and produce compile_commands.json
    if not args.no_build:
        print('configuring...')
        local['./configure']()
        print('building...')
        try:
            intercept_build = pb.local['intercept-build']
        except pb.CommandNotFound:
            intercept_build = get_cmd_or_die('compiledb')
        intercept_build['make']()

    assert os.path.isfile(COMPILE_COMMANDS), 'Could not find {}'.format(COMPILE_COMMANDS)

    # Remove object files that will confuse `transpile`
    rm['-f', 'src/robotfindskitten.o']()

    c2rust_bin = get_cmd_or_die(config.C2RUST_BIN)
    print('transpiling...')
    transpile(COMPILE_COMMANDS, emit_build_files=False)

    # Move rust files into rust/src
    mkdir['-vp', 'rust/src']()
    mv['-v', local.path('src') // '*.rs', 'rust/src/']()

    with open('rust/src/robotfindskitten.rs') as f:
        lines = f.read().splitlines(True)
    lines = [l for l in lines if "/*I'm feeling" not in l]
    with open('rust/src/robotfindskitten.rs', 'w') as f:
        f.write(''.join(lines))


if __name__ == '__main__':
    main()
