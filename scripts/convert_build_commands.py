#!/usr/bin/env -S uv run

import bencode
import glob
import json
import os
import sys
from typing import Any, Dict, List, Optional

def get_fake() -> int:
    get_fake.ctr += 1  # type: ignore
    return get_fake.ctr  # type: ignore
get_fake.ctr = -1  # type: ignore

class EntryInfo:
    def __init__(self, e: Dict[str, Any]):
        self.entry = e
        self.new_args: List[str] = []
        self.c_inputs: List[str] = []
        self.rest_inputs: List[str] = []
        self.libs: List[str] = []
        self.lib_dirs: List[str] = []
        self.compile_only = False
        self.shared_lib = False
        self.output = None

def convert_entries(entries: List[Dict[str, Any]],
        out_dir: Optional[str] = None) -> List[Dict[str, Any]]:
    entry_infos = []
    for entry in entries:
        old_args, entry["arguments"] = entry["arguments"], []
        arg_iter = iter(old_args)

        ei = EntryInfo(entry)
        ei.new_args.append(next(arg_iter)) # Copy over old_args[0]
        for arg in arg_iter:
            if arg in {"-D", "-U", "-I", "-include"}:
                # TODO: use the full list of `Separate` options from gcc
                ei.new_args.append(arg)
                ei.new_args.append(next(arg_iter))

            elif arg == "-c":
                ei.compile_only = True

            elif arg == "-o":
                ei.output = next(arg_iter)
                ei.new_args.append(arg)
                assert ei.output is not None
                ei.new_args.append(ei.output)

            elif arg[:2] == "-o":
                ei.output = arg[2:]
                ei.new_args.append(arg)

            elif arg == "-l":
                ei.libs.append(next(arg_iter))

            elif arg[:2] == "-l":
                ei.libs.append(arg[2:])

            # -pthread implicitly adds -lpthread
            elif arg == "-pthread":
                ei.libs.append("pthread")
                ei.new_args.append(arg)

            elif arg == "-L":
                ei.lib_dirs.append(next(arg_iter))

            elif arg[:2] == "-L":
                ei.lib_dirs.append(arg[2:])

            elif arg == "-shared":
                ei.shared_lib = True

            elif arg[0] != '-' and arg[0] != '-':
                if arg[-2:] == ".c":
                    ei.c_inputs.append(arg)
                else:
                    ei.rest_inputs.append(arg)

            else:
                ei.new_args.append(arg)

        entry_infos.append(ei)

    object_map = {}
    new_entries = []
    for ei in entry_infos:
        for inp in ei.c_inputs:
            inp_path = os.path.join(entry["directory"], inp)
            inp_path = os.path.realpath(inp_path)

            # TODO: handle duplicates
            c_object = ei.output or "%s_%d.o" % (inp[:-2], get_fake())
            object_map[inp] = c_object

            new_entry = ei.entry.copy()
            new_entry["arguments"] = ei.new_args + ["-c", inp]
            new_entry["file"] = os.path.relpath(inp_path, out_dir) if out_dir else inp_path
            new_entry["output"] = c_object
            del new_entry["type"]
            new_entries.append(new_entry)

    for ei in filter(lambda ei: not ei.compile_only, entry_infos):
        new_entry = ei.entry.copy()
        c_objects = [object_map[inp] for inp in ei.c_inputs]
        new_entry["arguments"] = ei.new_args
        # Hacky solution: c2rust-tranpile needs an absolute path here,
        # so we add a path-like prefix so that the transpiler can both
        # parse it correctly and recognize it as a bencoded link command
        new_entry["file"] = "/c2rust/link/" + bencode.bencode({
            "inputs": c_objects + ei.rest_inputs, # FIXME: wrong order???
            "libs": ei.libs,
            "lib_dirs": ei.lib_dirs,
            "type": "shared" if ei.shared_lib else "exe",
            # TODO: parse and add in other linker flags
            # for now, we don't do this because rustc doesn't use them
            })
        new_entry["output"] = ei.output or "a.out"
        del new_entry["type"]
        new_entries.append(new_entry)

    return new_entries


def main() -> None:
    if len(sys.argv) != 3:
        sys.exit("Usage: convert_build_commands <build commands directory> <compilation database file>")

    in_dir = sys.argv[1]
    out_file = sys.argv[2]
    out_dir = os.path.dirname(os.path.realpath(out_file))

    entries = []
    for json_file in glob.glob(os.path.join(in_dir, "*.json")):
        with open(json_file, 'r') as f:
            entry = json.load(f)
            if entry["type"] != "cc":
                continue # FIXME
            entries.append(entry)

    new_entries = convert_entries(entries, out_dir)
    with open(out_file, 'w') as f:
        json.dump(new_entries, f, indent=2)

if __name__ == '__main__':
    main()
