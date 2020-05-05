#!/usr/bin/env python3

import os
import sys
import json
import subprocess
from typing import List


def dump_ast(cmd):
    args: List[str] = cmd["arguments"]
    args[0] = "clang"
    assert args[1] == "-c"
    args[1] = "-fsyntax-only"
    args.append("-Xclang")
    args.append("-ast-dump")
    cmd_str: str = " ".join(args)

    olddir = os.curdir
    try:
        os.chdir(cmd["directory"])
        subprocess.call(cmd_str, shell=True)
    finally:
        os.chdir(olddir)


def main():
    if not len(sys.argv) == 3:
        print(
            "usage: print_clang_ast.py <file.c> <compile_commands.json>",
            file=sys.stderr)
        exit(1)
    c_file: str = sys.argv[1]
    compile_commands_file: str = sys.argv[2]

    with open(compile_commands_file, "r") as fh:
        commands = json.load(fh)
    commands = filter(
        lambda c: os.path.basename(c["file"]) == c_file,
        commands)

    cmd = next(commands, None)

    if not cmd:
        print(
            f"error: no command to compile {c_file}",
            file=sys.stderr)
        exit(1)
    elif next(commands, None):
        print(
            f"warning: found multiple ({cl}) commands for {c_file}",
            file=sys.stderr)
    
    dump_ast(cmd)


if __name__ == "__main__":
    main()
