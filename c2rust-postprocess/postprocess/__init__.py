"""
c2rust-postprocess: Transfer comments from C functions to Rust functions using LLMs.
"""

import logging
from pathlib import Path
from textwrap import dedent
from typing import Any

from postprocess.cache import AbstractCache
from postprocess.definitions import get_c_comments, get_function_span_pairs
from postprocess.models import get_model_by_id
from postprocess.utils import get_compile_commands, get_rust_files, read_chunk, remove_backticks

from pygments import highlight
from pygments.lexers import RustLexer
from pygments.formatters import TerminalFormatter

# TODO: could also include
# - validation function to check result
# - list of comments to check for
class CommentTransferPrompt:
    c_function: str
    rust_function: str
    prompt_text: str

    __slots__ = ("c_function", "rust_function", "prompt_text")

    def __init__(self, c_function: str, rust_function: str, prompt_text: str) -> None:
        self.c_function = c_function
        self.rust_function = rust_function
        self.prompt_text = prompt_text

    def __str__(self) -> str:
        return self.prompt_text + "\n\n" + \
               "C function:\n```c\n" + self.c_function + "```\n\n" + \
               "Rust function:\n```rust\n" + self.rust_function + "```\n"


def generate_prompts(
    compile_commands: list[dict[str, Any]], rust_file: Path
) -> list[CommentTransferPrompt]:
    pairs = get_function_span_pairs(compile_commands, rust_file)

    prompts = []

    for rust_fn, c_fn in pairs:
        c_def = read_chunk(c_fn["file"], c_fn["start_byte"], c_fn["end_byte"])
        c_comments = get_c_comments(c_def)
        if not c_comments:
            logging.info(f"Skipping C function without comments: {c_fn['name']}")
            continue

        # TODO: log on verbose level
        # print(f"C function {c_fn['name']} definition:\n{c_def}\n")

        rust_def = read_chunk(
            rust_fn["file"], rust_fn["start_byte"], rust_fn["end_byte"]
        )
        # TODO: log on verbose level
        # print(f"Rust function {rust_fn['name']} definition:\n{rust_def}\n")

        # TODO: make this function take a model and get prompt from model
        prompt_text = """
        Transfer the comments from the following C function to the corresponding Rust function.
        Do not add any comments that are not present in the C function.
        Respond with the Rust function definition with the transferred comments; say nothing else.
        """ # noqa: E501
        prompt_text = dedent(prompt_text).strip()

        prompt = CommentTransferPrompt(
            c_function=c_def, rust_function=rust_def, prompt_text=prompt_text
        )

        prompts.append(prompt)

    return prompts


# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
)

def transfer_comments(compile_commands_path: Path, cache: AbstractCache) -> None:
    # TODO: instantiate the model based on command line args
    # TODO: avoid google-specific import here
    from google.genai import types
    model = get_model_by_id(
            "gemini-3-pro-preview",
            generation_config = {"system_instruction": types.Content(
                role="system",
                parts=[types.Part.from_text(text=SYSTEM_INSTRUCTION)]
            )}
    )

    rust_sources = get_rust_files(compile_commands_path.parent)

    compile_commands = get_compile_commands(compile_commands_path)

    for rust_file in rust_sources:
        prompts = generate_prompts(compile_commands, rust_file)

        for prompt in prompts:
            messages = [
                {"role": "user", "content": str(prompt)},
            ]

            if not (response := cache.lookup(messages)):
                response = model.generate_with_tools(messages)
                if response is None:
                    logging.error("Model returned no response")
                    continue
                cache.update(messages, response)

            response = remove_backticks(response)

            if True:  # TODO: detect when terminal supports colors
                highlighted_response = highlight(response, RustLexer(), TerminalFormatter())

                print("Response:\n", highlighted_response)