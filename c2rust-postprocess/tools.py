"""Main comment transfer logic."""

import logging
import os
import re
from difflib import ndiff
from typing import List, Optional, Dict, Any

from compilers import ASTAnalyzer
from models import create_llm_client

from pygments import highlight, lex
from pygments.lexer import RegexLexer, bygroups, using
from pygments.formatters import TerminalFormatter
from pygments.lexers.rust import RustLexer
from pygments.token import Text, Generic

LOGGER = logging.getLogger(__name__)


class RustDiffLexer(RegexLexer):
    """
    A Pygments lexer that combines the standard diff metadata
    rules with full Rust highlighting on the diff hunks.
    """

    name = "RustDiff"
    filenames = ["*.diff", "*.patch"]
    flags = re.MULTILINE

    # fmt: off
    tokens = {
        'root': [
            # Diff file headers
            (r'^diff .*\n',            Generic.Heading),
            (r'^index .*\n',           Generic.Heading),
            (r'^--- .*\n',             Generic.Deleted),
            (r'^\+\+\+ .*\n',          Generic.Inserted),
            (r'^@@ .*\n',              Generic.Subheading),

            # Removed lines: '-' marker + Rust content
            (r'^(-)(.*\n)',
                bygroups(Generic.Deleted, using(RustLexer))),

            # Added lines: '+' marker + Rust content
            (r'^(\+)(.*\n)',
                bygroups(Generic.Inserted, using(RustLexer))),

            # Context lines: ' ' marker + Rust content
            (r'^( )(.*\n)',
                bygroups(Text, using(RustLexer))),

            # Anything else (e.g. stray text) as plain text
            (r'.+\n', Text),
        ],
    }
    # fmt: on


class CommentTransferTool:
    """Tool for transferring comments from C to Rust functions."""

    def __init__(self, args):
        self.model = args.model
        self.compile_commands_path = args.path
        self.llm_client = create_llm_client(args)
        self.ast_analyzer = ASTAnalyzer()
        self.rustdoc_json_path = get_rustdoc_json_path(self.compile_commands_path)

    def process_files(
        self, file_pattern: Optional[str] = None, function_pattern: Optional[str] = None
    ):
        """Process files matching the given patterns."""
        file_regex = re.compile(file_pattern) if file_pattern else None
        func_regex = re.compile(function_pattern) if function_pattern else None

        entries = self.ast_analyzer.get_compile_commands_entries(
            self.compile_commands_path
        )

        for entry in entries:
            if file_regex and not file_regex.search(entry["file"]):
                continue

            self._process_single_file(entry, func_regex)

    def _process_single_file(
        self, entry: Dict[str, Any], func_pattern: Optional[re.Pattern]
    ):
        """Process a single C file."""
        c_file = entry["file"]
        rust_file = get_rust_file_path(c_file)

        # Remove common relative path prefix with compile_commands.json
        common_prefix = os.path.commonprefix([self.compile_commands_path, rust_file])
        assert (
            common_prefix
        ), f"No common prefix found between compile_commands.json and {rust_file}."
        rust_file = rust_file.replace(common_prefix, "").lstrip(os.sep)
        print(f"Corresponding rust file: {rust_file}")

        ast = self.ast_analyzer.get_c_ast_as_json(entry)
        c_functions = self.ast_analyzer.get_functions_from_clang_ast(ast)
        print(f"Found {len(c_functions)} C functions in {entry['file']}.")

        rust_functions = self.ast_analyzer.get_rustdoc_entries(
            self.rustdoc_json_path, rust_file
        )

        for c_func in c_functions:
            if self._should_skip_function(c_func, entry["file"], func_pattern):
                continue

            self._process_function_pair(c_func, rust_functions, c_file, rust_file, common_prefix)

    def _get_c_comments(self, code: str) -> List[str]:
        """Extract comments from the given C code."""
        from pygments import lex
        from pygments.lexers import CLexer
        from pygments.token import Comment

        comments = []
        for tok_type, tok_value in lex(code, CLexer()):
            # NOTE: Not including Preproc comments here, as they are not typical comments
            if tok_type in Comment and not tok_type in Comment.Preproc:
                # Keep exactly what appears, including delimiters (//, /* */)
                comments.append(tok_value)
        return comments

    def _should_skip_function(
        self, c_func: Dict[str, Any], c_file: str, func_pattern: Optional[re.Pattern]
    ) -> bool:
        """Check if a function should be skipped."""
        if not self.ast_analyzer.is_entry_from_c_file(c_func, c_file):
            return True

        if "expansionLoc" in c_func["loc"]:
            return True

        if func_pattern and not func_pattern.search(c_func["name"]):
            print(f"Filtering-out function: {c_func['name']}.")
            return True

        return False

    def _process_function_pair(
        self,
        c_func: Dict[str, Any],
        rust_functions: List[str],
        c_file: str,
        rust_file: str,
        common_prefix: str,
    ):
        """Process a matching C and Rust function pair."""
        rust_func = [func for func in rust_functions if func.endswith(c_func["name"])]

        if not rust_func:
            LOGGER.error(f"c_func {c_func['name']} not found in Rust functions for {rust_file}.")
            return

        rust_func_begin, rust_func_end, _rust_func_name = rust_func[0].split(":")
        c_func_begin, c_func_end = self._get_function_range(c_func)

        c_func_lines = get_lines(c_file, c_func_begin, c_func_end)

        c_func_code = "".join(c_func_lines)
        c_comments = self._get_c_comments(c_func_code)
        if not c_comments:
            LOGGER.warning(f"Skipping function without comments: {c_func['name']}.")
            return

        LOGGER.info(f"Processing: {c_func['name']}: {c_func}")

        rust_file_path = c_file.replace(".c", ".rs")
        rust_func_lines_without_comments = get_lines(
            rust_file_path, int(rust_func_begin), int(rust_func_end)
        )

        prompt = self._create_prompt(
            c_func["name"], c_func_lines, rust_func_lines_without_comments
        )

        def validate_rust_function_with_comments(rust_func_with_comments: str) -> bool:
            """
            Ensures that the Rust function has comments correctly inserted.

            Arguments:
            - rust_func_with_comments: The Rust function with comments added.

            Returns:
            - True if comments were inserted correctly. False otherwise.
            """
            return self._validate_result(
                rust_func_lines_without_comments,
                c_comments,
                rust_func_with_comments
            )

        LOGGER.info(prompt)

        response = self.llm_client.generate_response(
            prompt, validate_rust_function_with_comments
        )

        LOGGER.debug(f"Function: {c_func} -> {rust_func[0]}")
        if response:
            rust_func_lines_with_comments = response.splitlines(keepends=True)

            # make sure the last line ends with a newline
            rust_func_lines_with_comments[-1] = (
                rust_func_lines_with_comments[-1].rstrip() + "\n"
            )

            diff = ndiff(
                rust_func_lines_without_comments, rust_func_lines_with_comments
            )
            diff_text = "".join(diff)
            highlighted_diff = highlight(
                diff_text, RustDiffLexer(), TerminalFormatter()
            )
            print(highlighted_diff)

            # TODO: put in its own function
            # create temporary file to write the Rust function with comments
            import tempfile
            with tempfile.NamedTemporaryFile(mode="w+t", delete=True, suffix=".rs") as tmp_file:
                temporary_rust_file = tmp_file.name
                lines_before = get_lines(rust_file_path, 1, int(rust_func_begin) - 1)
                lines_after = get_lines(rust_file_path, int(rust_func_end) + 1, None)
                tmp_file.writelines(lines_before)
                tmp_file.writelines(rust_func_lines_with_comments)
                tmp_file.writelines(lines_after)

                tmp_file.flush()
                # now run git diff on the temporary file
                rust_file_path = rust_file_path.replace(common_prefix, "")
                # change working directory to the common prefix
                curdir = os.getcwd()
                os.chdir(common_prefix)
                os.system(f"git diff --no-index {rust_file_path} {temporary_rust_file} | "
                          f" sed  -e 's|{temporary_rust_file[1:]}|{rust_file_path}|' | "
                          f"tee /tmp/{c_func['name']}_comments.patch")
                os.chdir(curdir)

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

    def _create_prompt(
        self, func_name: str, c_func_lines: List[str], rust_func_lines: List[str]
    ) -> str:
        """Create the prompt for the LLM."""
        return f"""
Inspect the following C function and carefully transfer all comments
into the corresponding Rust function. 

The comments that need to be inserted are:
{"".join(self._get_c_comments("\n".join(c_func_lines)))}

Use this C function to determine where to insert the comments in the Rust function.

<code>
{"".join(c_func_lines)}
</code>

Insert comments into this Rust function (which corresponds to the C function above):

<code>
{"".join(rust_func_lines)}
</code>
"""

    def _validate_result(self, original_lines: List[str], comments: List[str], result: str) -> bool:
        """Validate that the result contains only comment additions."""
        result_lines = result.splitlines(keepends=True)

        if not result_lines:
            print("Validation fail. Result is empty.")
            return False

        # make sure the last line ends with a newline
        result_lines[-1] = result_lines[-1].rstrip() + "\n"

        if result_lines[0].strip() == "```rust" and result_lines[-1].strip() == "```":
            print("warning: stripping ```rust code block markers from result")
            result_lines = result_lines[1:-1]

        if result_lines[0].strip() == "```" and result_lines[-1].strip() == "```":
            print("warning: stripping ``` code block markers from result")
            result_lines = result_lines[1:-1]

        diff = ndiff(original_lines, result_lines)
        for line in diff:
            if line.startswith("+ "):
                # Sometimes the model adds a blank line before a comment, that's fine
                if line.strip() == "+":
                    continue
                # Check if the line contains a comment
                if line.find("//") == -1:
                    print(f"Validation fail. Line does not contain a comment: {line}")
                    return False

        for comment in comments:
            if not comment.startswith("//"):
                continue  # TODO: handle multi-line comments
            if not any(comment in line for line in result_lines):
                print(f"Validation fail. Comment not found in result: {comment}")
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


def get_lines(filename: str, start: int, end: Optional[int]) -> list[str]:
    """
    Return lines start..end (1-based, inclusive) from filename.
    """
    from itertools import islice

    if end is not None and (start < 1 or end < start):
        raise ValueError(f"Invalid range: {start}–{end}")

    with open(filename, "r") as f:
        return list(islice(f, start - 1, end))
