"""AST parsing and analysis utilities."""

import json
import subprocess
from typing import Dict, List, Any
import jq


class ASTAnalyzer:
    """Handles parsing and analysis of C and Rust ASTs."""
    
    @staticmethod
    def get_compile_commands_entries(path: str) -> List[Dict[str, Any]]:
        """
        Reads compile_commands.json and returns a list of source files.
        """
        with open(path, "r") as f:
            data = json.load(f)
        return [entry for entry in data if "file" in entry]

    @staticmethod
    def get_rustdoc_entries(path: str, rustfile: str) -> List[str]:
        """
        Reads a Rust documentation JSON file and returns a list of function entries.
        """
        with open(path, "r") as f:
            data = json.load(f)
        query_str = f"""
        .index | to_entries[] |
        select(.value.kind=="function" and .value.span.filename == "{rustfile}") |
        .value |
        "\\(.span.begin[0]):\\(.span.end[0]):\\(.name)"
        """
        query = jq.compile(query_str)
        return query.transform(data, multiple_output=True)

    @staticmethod
    def get_functions_from_clang_ast(ast: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Extract function declarations from the Clang AST JSON.

        Args:
            ast (dict): The AST JSON as a dictionary.

        Returns:
            list[dict]: A list of dictionaries, each representing a function declaration.
        """
        query = jq.compile(
            '.inner[] | select(.kind =="FunctionDecl") | {name: .name, loc: .loc, range: .range}'
        )
        return query.transform(ast, multiple_output=True)

    @staticmethod
    def get_c_ast_as_json(entry: Dict[str, Any]) -> Dict[str, Any]:
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
        except subprocess.CalledProcessError as e:
            print(f"Error running clang on {source_file}: {e.stderr}")
            raise

    @staticmethod
    def is_entry_from_c_file(entry: Dict[str, Any], c_file: str) -> bool:
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
        if "includedFrom" not in loc:
            return True  # entry was parsed from c_file so by default it is from that file
        return False
