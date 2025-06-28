#!/usr/bin/env -S uv --quiet run --script
from difflib import ndiff
import json
import re
from wsgiref import types
import jq
import os

from itertools import islice
from typing import List


def get_lines(filename: str, start: int, end: int) -> List[str]:
    """
    Return lines start..end (1-based, inclusive) from filename.
    """
    if start < 1 or end < start:
        raise ValueError(f"Invalid range: {start}–{end}")
    # islice’s stop index is exclusive, so we pass `end` directly
    with open(filename, "r") as f:
        return list(islice(f, start - 1, end))


def main(args):
    def is_entry_from_c_file(entry, c_file):
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
            return (
                True  # entry was parsed from c_file so by default it is from that file
            )
        return False

    # TODO: do this properly
    rustdoc_json_path = os.path.join(
        os.path.dirname(args.path), "target", "doc", "c2rust_out.json"
    )

    file_pattern = re.compile(args.file_pattern) if args.file_pattern else None

    entries = get_compile_commands_entries(args.path)
    # print(f"Found {len(entries)} source files in {args.path}")
    for entry in entries:
        cfile = entry["file"]

        if file_pattern and not file_pattern.search(cfile):
            # print(f"Skipping file: {cfile}...")
            continue

        # print(f"Functions in file: {cfile}. ")
        rustfile = cfile.replace(".c", ".rs")
        # remove common relative path prefix with args.path
        common_prefix = os.path.commonprefix([args.path, rustfile])
        assert (
            common_prefix
        ), f"No common prefix found between compile_commands.json and {rustfile}."
        rustfile = rustfile.replace(common_prefix, "").lstrip(os.sep)
        print(f"Corresponding rust file: {rustfile}")

        ast = get_c_ast_as_json(entry)
        c_functions = get_functions_from_clang_ast(ast)
        print(f"Found {len(c_functions)} C functions in {entry['file']}.")
        rust_functions = get_rustdoc_entries(rustdoc_json_path, rustfile)

        func_name_pattern = re.compile(args.function_pattern) if args.function_pattern else None

        for c_func in c_functions:
            if not is_entry_from_c_file(c_func, entry["file"]):
                # print(f"Skipping {c_func} because it is not from {entry['file']}.")
                continue

            if (
                "expansionLoc"
                in c_func["loc"]
                # and "isMacroArgExpansion" in c_func_info["expansionLoc"]
                # and c_func_info["expansionLoc"]["isMacroArgExpansion"] == True
            ):
                continue

            if func_name_pattern and not func_name_pattern.search(c_func["name"]):
                print(f"Skipping function: {c_func['name']}.")
                continue
            print(f"Processing: {c_func['name']}: {c_func}")

            rust_func = [
                func for func in rust_functions if func.endswith(c_func["name"])
            ]
            # TODO: figure out when mismatch is because the C function was from a library
            # vs when we should expect to have a matching Rust function
            if not rust_func:
                print(
                    f"c_func {c_func['name']} not found in Rust functions for {rustfile}."
                )
                quit()

            (rust_func_begin, rust_func_end, _rust_func_name) = rust_func[0].split(":")
            (c_func_begin, c_func_end) = (
                (
                    c_func["range"]["begin"]["line"]
                    if "line" in c_func["range"]["begin"]
                    else c_func["loc"]["line"]
                ),
                c_func["range"]["end"]["line"],
            )

            c_func_lines = get_lines(cfile, c_func_begin, c_func_end)
            rust_func_lines = get_lines(
                cfile.replace(".c", ".rs"), int(rust_func_begin), int(rust_func_end)
            )

            prompt = f"""
Please take the comments in the following C function ({c_func["name"]}) and insert them
into the corresponding Rust function. Here is the C function containing the comments:

```c
{"".join(c_func_lines)}
```

Here is the corresponding Rust function that needs to have comments inserted:

```rust
{"".join(rust_func_lines)}
```
"""
            rust_func_lines = ['```rust\n'] + rust_func_lines + ['```']
            def check_result(rust_func_with_comments: str) -> bool:
                """
                Check if the input Rust function matches the original Rust function
                with comments inserted.

                Args:
                    rust_func_with_comments (str): The Rust function with comments inserted.

                Returns:
                    bool: True if `rust_func_with_comments` had comments inserted correctly, False otherwise.
                """
                diff = ndiff(rust_func_lines, rust_func_with_comments.splitlines(keepends=True))
                for line in diff:
                    if line.startswith("- ") or line.startswith("? "):
                        return False
                    if line.startswith("+ "):
                        # Check if the line is a Rust in-line comment
                        if not line[2:].strip().startswith("//"):
                            return False
                return True

            print(prompt)
            # TODO: clean this up
            match args.model:
                case model if model.startswith(("gpt", "o3", "o4", "codex")):
                    response = call_openai(prompt, model=args.model)
                case model if model.startswith("gemini"):


                    response = call_gemini(prompt, check_result, model=args.model)
                case _:
                    raise ValueError(f"Unknown model: {args.model}")
            print(f"Function: {c_func} -> {rust_func[0]}")
            # print(f"Response:\n{response}")


            diff = ndiff(rust_func_lines, response.splitlines(keepends=True))
            print("".join(diff))
            quit()  # for debugging


def get_rustdoc_entries(path: str, rustfile: str) -> list:
    """
    Reads a Rust documentation JSON file and returns a list of entries.
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


def get_compile_commands_entries(path: str):
    """
    Reads compile_commands.json and returns a list of source files.
    """
    with open(path, "r") as f:
        data = json.load(f)
    return [entry for entry in data if "file" in entry]


COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS = """
You are a code migration assistant that transfers comments from a C function to a corresponding Rust function.
Carefully read all comments from the C function and transfer them to the matching Rust function.
ONLY use inline comments (//) when translating comments to Rust. Do not use doc comments (///).
The comments should be inserted in the same order and at the same locations as they appear in the C function.
The output Rust function must be syntactically correct and identical to the original Rust function other than the added comments.
DO NOT add comments that are not present in the C function.
ONLY return the Rust function with comments inserted! DO NOT touch Rust code that is not related to comments.
Do not return the C function. Do not produce a summary of changes.
Use function calling to check if the comments were inserted correctly before returning the result.
The first line of your reply should be '```rust' and the last line should be '```' (no newline at end).
"""


def call_gemini(prompt: str, validate_reply_fn: callable, model: str = "gemini-2.5-pro", timeout: int = 60) -> str:
    """
    Call the Gemini LLM with the given input.
    """
    from time import sleep
    from google.genai import Client
    from google.genai import types

    client = Client()

    # Gemini 2.5 Flash has a smaller max thinking budget than Pro
    thinking_budget = 32768 if model.endswith("pro") else 24576

    try:
        response = client.models.generate_content(
            model=model,
            contents=prompt,
            config=types.GenerateContentConfig(
                system_instruction=COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
                thinking_config=types.ThinkingConfig(thinking_budget=thinking_budget),
                tools=[validate_reply_fn],
            ),
        )
        return response.text
    except google.genai.errors.ServerError as se:
        if se.code == 503:
            if timeout > 0:
                print("Service overloaded. Retrying shortly...")
                sleep(10)
                return call_gemini(prompt, model=model, timeout=timeout - 10)
            else:
                print("Timeout exceeded while waiting for model to no longer be overloaded.")
        elif se.code == 429:
            print("Rate limit exceeded. Please try again later.")
        else:
            print(f"Unknown error calling Gemini: {se}")
        quit()


# "gpt-4.1-nano"
# o4-mini-latest works well
def call_openai(prompt: str, model: str = "codex-mini-latest") -> str:
    """
    Call the LLM with the given input and model.
    """
    from openai import OpenAI

    client = OpenAI(
        # This is the default and can be omitted
        api_key=os.environ.get("OPENAI_API_KEY"),
    )

    completion = client.chat.completions.create(
        model=model,
        messages=[
            {
                "role": "developer",
                "content": COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
            },
            {"role": "user", "content": prompt},
        ],
    )
    assert completion.choices[0].message.content  # appease type checker
    return completion.choices[0].message.content


def get_functions_from_clang_ast(ast: dict) -> list:
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


def get_c_ast_as_json(entry) -> dict:
    """
    Get AST as JSON for a translation unit identified by compile commands entry.

    TODO:
        - what is the minium required clang version for dumping AST as JSON?
        - add function to look for clang and check its version
        - better validate the entry from compile_commands.json?
    """
    import json
    import subprocess

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
        quit()


def parse_args():
    import argparse

    parser = argparse.ArgumentParser(description="c2rust-llm-assist")
    parser.add_argument(
        "-f",
        "--function-pattern",
        type=str,
        required=False,
        default=None,
        help="Only process functions matching this regex pattern",
    )
    parser.add_argument(
        "-F",
        "--file-pattern",
        type=str,
        required=False,
        default=None,
        help="Only process files matching this regex pattern (applied to the C file name)",
    )
    parser.add_argument(
        "-m",
        "--model",
        type=str,
        required=False,
        default="gemini-2.5-flash",
        help="LLM model to use (default: gemini-2.5-flash)",
    )

    parser.add_argument(
        "-p", "--path", type=str, required=True, help="Path to compile_commands.json"
    )

    args = parser.parse_args()

    if not os.path.exists(args.path):
        raise FileNotFoundError(f"Path {args.path} does not exist.")
    if not os.access(args.path, os.R_OK):
        raise PermissionError(f"Path {args.path} is not readable.")

    return args


if __name__ == "__main__":
    args = parse_args()
    main(args)

# TODOs:
# - improve prompt using the below suggestions
# • Clarify what is meant by 'identical': should import statements, function attributes, or whitespace/style also match the original Rust?
# • Specify if Rust doc comments (///) or inline (//) should match the style of the C comments.
# • Indicate if the code should be formatted to Rust conventions or mirror the C formatting.
# • Recommend stating whether top-level or only in-function comments should be processed.
# • Suggest example expected output for further clarity.
# - Explore adaptable model reasoning effort and model choice? (e.g. try flash then pro)

# CLEANUP
# - Move LLM stuff to separate module
# - Cleanup logging

