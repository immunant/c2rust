import json
import subprocess
from subprocess import CalledProcessError
from typing import Any


def get_functions_from_clang_ast(ast: dict[str, Any]) -> list[dict[str, Any]]:
    """
    Extract function declarations from the Clang AST JSON.
    Args:
        ast (dict): The AST JSON as a dictionary.
    Returns:
        list[dict]: A list of dictionaries, each representing a function declaration.
    """
    return [
        {
            "name": node["name"],
            "loc": node["loc"],
            "range": node["range"],
        }
        for node in ast["inner"]
        if node["kind"] == "FunctionDecl"
    ]


def get_c_ast_as_json(entry: dict[str, Any]) -> dict[str, Any]:
    """
    Get AST as JSON for a translation unit identified by compile commands entry.
    """
    source_file = entry["file"]

    cmd = entry["arguments"]
    cmd[0] = "clang"  # make sure we use clang
    # drop the last four elements which are the output options
    cmd = cmd[:-4]  # TODO: validate that these are the output options
    # add the necessary flags to dump the AST as JSON
    cmd += [
        "-fsyntax-only",
        "-Xclang",
        "-ast-dump=json",
        "-fparse-all-comments",  # NOTE: Clang AST only includes doc comments
        source_file,
    ]
    try:
        # cwd to the directory from the compile_commands.json entry to make sure
        # relative paths in the command work correctly
        result = subprocess.run(
            cmd, capture_output=True, text=True, check=True, cwd=entry["directory"]
        )
        return json.loads(result.stdout)
    except CalledProcessError as e:
        print(f"Error running clang on {source_file}: {e.stderr}")
        raise


def is_entry_from_c_file(entry: dict[str, Any], c_file: str) -> bool:
    """
    Check if the entry is from the specified C file.
    """
    loc = entry["loc"]
    if "file" in loc:
        return loc["file"] == c_file
    elif "spellingLoc" in loc and "includedFrom" in loc["spellingLoc"]:
        return loc["spellingLoc"]["includedFrom"]["file"] == c_file
    elif "expansionLoc" in loc and "includedFrom" in loc["expansionLoc"]:
        return loc["expansionLoc"]["includedFrom"]["file"] == c_file

    # entry was parsed from c_file so by default it is from that file
    return "includedFrom" not in loc
