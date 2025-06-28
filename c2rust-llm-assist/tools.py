"""Main comment transfer logic."""

import os
import re
from difflib import ndiff
from typing import List, Optional, Dict, Any

from agents import function_tool

from compilers import ASTAnalyzer
from models import create_llm_client


class CommentTransferTool:
    """Main tool for transferring comments from C to Rust functions."""

    def __init__(self, model: str, compile_commands_path: str):
        self.model = model
        self.compile_commands_path = compile_commands_path
        self.llm_client = create_llm_client(model)
        self.ast_analyzer = ASTAnalyzer()
        self.rustdoc_json_path = get_rustdoc_json_path(compile_commands_path)

    def process_files(self, file_pattern: Optional[str] = None, function_pattern: Optional[str] = None):
        """Process files matching the given patterns."""
        file_regex = re.compile(file_pattern) if file_pattern else None
        func_regex = re.compile(function_pattern) if function_pattern else None

        entries = self.ast_analyzer.get_compile_commands_entries(self.compile_commands_path)

        for entry in entries:
            if file_regex and not file_regex.search(entry["file"]):
                continue

            self._process_single_file(entry, func_regex)

    def _process_single_file(self, entry: Dict[str, Any], func_pattern: Optional[re.Pattern]):
        """Process a single C file."""
        c_file = entry["file"]
        rust_file = get_rust_file_path(c_file)

        # Remove common relative path prefix with compile_commands.json
        common_prefix = os.path.commonprefix([self.compile_commands_path, rust_file])
        assert common_prefix, f"No common prefix found between compile_commands.json and {rust_file}."
        rust_file = rust_file.replace(common_prefix, "").lstrip(os.sep)
        print(f"Corresponding rust file: {rust_file}")

        ast = self.ast_analyzer.get_c_ast_as_json(entry)
        c_functions = self.ast_analyzer.get_functions_from_clang_ast(ast)
        print(f"Found {len(c_functions)} C functions in {entry['file']}.")

        rust_functions = self.ast_analyzer.get_rustdoc_entries(self.rustdoc_json_path, rust_file)

        for c_func in c_functions:
            if self._should_skip_function(c_func, entry["file"], func_pattern):
                continue

            self._process_function_pair(c_func, rust_functions, c_file, rust_file)


    def _should_skip_function(self, c_func: Dict[str, Any], c_file: str, func_pattern: Optional[re.Pattern]) -> bool:
        """Check if a function should be skipped."""
        if not self.ast_analyzer.is_entry_from_c_file(c_func, c_file):
            return True

        if "expansionLoc" in c_func["loc"]:
            return True

        if func_pattern and not func_pattern.search(c_func["name"]):
            print(f"Skipping function: {c_func['name']}.")
            return True

        return False

    def _process_function_pair(self, c_func: Dict[str, Any], rust_functions: List[str],
                              c_file: str, rust_file: str):
        """Process a matching C and Rust function pair."""
        print(f"Processing: {c_func['name']}: {c_func}")

        rust_func = [func for func in rust_functions if func.endswith(c_func["name"])]

        if not rust_func:
            print(f"c_func {c_func['name']} not found in Rust functions for {rust_file}.")
            return

        rust_func_begin, rust_func_end, _rust_func_name = rust_func[0].split(":")
        c_func_begin, c_func_end = self._get_function_range(c_func)

        c_func_lines = get_lines(c_file, c_func_begin, c_func_end)
        rust_file_path = c_file.replace(".c", ".rs")
        rust_func_lines = get_lines(rust_file_path, int(rust_func_begin), int(rust_func_end))

        prompt = self._create_prompt(c_func["name"], c_func_lines, rust_func_lines)
        rust_func_lines_with_markers = ['```rust\n'] + rust_func_lines + ['```']

        @function_tool
        def validate_rust_function_with_comments(rust_func_with_comments: str) -> bool:
            """
            Ensures that the result contains only comment additions.

            Arguments:
            - rust_func_with_comments: The Rust function with comments added.

            Returns:
            - True if the result is valid, False otherwise.
            """
            return self._validate_result(rust_func_lines_with_markers, rust_func_with_comments)

        print(prompt)

        response = self.llm_client.generate_response(prompt, validate_rust_function_with_comments)

        print(f"Function: {c_func} -> {rust_func[0]}")
        if response:
            print(f"Response:\n{response}")
            diff = ndiff(rust_func_lines_with_markers, response.splitlines(keepends=True))
            print("".join(diff))
            return  # Remove quit() for better organization
        else:
            print("No response from model.")


    def _get_function_range(self, c_func: Dict[str, Any]) -> tuple[int, int]:
        """Get the line range for a C function."""
        c_func_begin = (
            c_func["range"]["begin"]["line"]
            if "line" in c_func["range"]["begin"]
            else c_func["loc"]["line"]
        )
        c_func_end = c_func["range"]["end"]["line"]
        return c_func_begin, c_func_end

    def _create_prompt(self, func_name: str, c_func_lines: List[str], rust_func_lines: List[str]) -> str:
        """Create the prompt for the LLM."""
        return f"""
Please take the comments in the following C function ({func_name}) and insert them
into the corresponding Rust function. Here is the C function containing the comments:

```c
{"".join(c_func_lines)}
```

Here is the corresponding Rust function that needs to have comments inserted:

```rust
{"".join(rust_func_lines)}
```
"""

    def _validate_result(self, original_lines: List[str], result: str) -> bool:
        """Validate that the result contains only comment additions."""
        diff = ndiff(original_lines, result.splitlines(keepends=True))
        for line in diff:
            if line.startswith("+ "):
                # Sometimes the model adds a blank line before a comment, that's fine
                if line.strip() == "+":
                    continue
                # Check if the line contains a comment
                if line.find("//") == -1:
                    print(f"Validation fail. Line does not contain a comment: {line}")
                    return False
        print("Validation passed. Comments were inserted correctly.")
        return True


def get_rustdoc_json_path(compile_commands_path: str) -> str:
    """Get the expected rustdoc JSON path from compile_commands.json path."""
    return os.path.join(
        os.path.dirname(compile_commands_path), "target", "doc", "c2rust_out.json"
    )


def get_rust_file_path(c_file_path: str) -> str:
    """Convert C file path to corresponding Rust file path."""
    return c_file_path.replace(".c", ".rs")


def get_lines(filename: str, start: int, end: int) -> list[str]:
    """
    Return lines start..end (1-based, inclusive) from filename.
    """
    from itertools import islice

    if start < 1 or end < start:
        raise ValueError(f"Invalid range: {start}–{end}")

    with open(filename, "r") as f:
        return list(islice(f, start - 1, end))
