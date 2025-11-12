#!/usr/bin/env -S uv run

import os
import sys
import json
import logging
import subprocess
from typing import Any, Dict, List
from common import setup_logging, die, get_cmd_or_die


def dump_ast(cmd: Dict[str, Any]) -> None:
    args: List[str] = cmd["arguments"]
    assert len(args) >= 3 and args[1] == "-c"
    args[0] = "clang"
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


def main() -> None:
    setup_logging()
    if not len(sys.argv) == 3:
        print(
            "usage: print_clang_ast.py <file.c> path/to/compile_commands.json",
            file=sys.stderr)
        exit(1)
    c_file: str = os.path.basename(sys.argv[1])
    compile_commands_path: str = sys.argv[2]

    # do we have clang in path?
    get_cmd_or_die("clang")

    try:
        with open(compile_commands_path, "r") as fh:
            commands = json.load(fh)
    except FileNotFoundError:
        die(f"file not found: " + compile_commands_path)

    commands = filter(
        lambda c: os.path.basename(c["file"]) == c_file,
        commands)

    cmd = next(commands, None)
    if not cmd:
        die(f"no command to compile {c_file}")
    elif next(commands, None):
        logging.warning(f"warning: found multiple commands for {c_file}")
    
    dump_ast(cmd)


if __name__ == "__main__":
    main()
