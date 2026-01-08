import logging
import re
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    get_c_comments,
    get_c_definitions,
    get_rust_comments,
    get_rust_definitions,
    update_rust_definition,
)
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.models import AbstractGenerativeModel
from postprocess.utils import get_highlighted_c, get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
)


class CommentTransferPrompt:
    c_function: str
    rust_function: str
    prompt_text: str
    identifier: str

    __slots__ = ("c_function", "rust_function", "prompt_text", "identifier")

    def __init__(
        self, c_function: str, rust_function: str, prompt_text: str, identifier: str
    ):
        self.c_function = c_function
        self.rust_function = rust_function
        self.prompt_text = prompt_text
        self.identifier = identifier

    def __str__(self) -> str:
        return (
            self.prompt_text
            + "\n\n"
            + "C function:\n```c\n"
            + self.c_function
            + "```\n\n"
            + "Rust function:\n```rust\n"
            + self.rust_function
            + "```\n"
        )


class CommentTransfer:
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        self.cache = cache
        self.model = model

    def transfer_comments(
        self,
        root_rust_source_file: Path,
        rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ) -> None:
        ident_regex = re.compile(ident_filter) if ident_filter else None

        rust_definitions = get_rust_definitions(rust_source_file)
        c_definitions = get_c_definitions(rust_source_file)

        logging.info(f"Loaded {len(rust_definitions)} Rust definitions")
        logging.info(f"Loaded {len(c_definitions)} C definitions")

        prompts: list[CommentTransferPrompt] = []
        for identifier, rust_definition in rust_definitions.items():
            if exclude_list.contains(path=rust_source_file, identifier=identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file}"
                    f"due to exclude file {exclude_list.src_path}"
                )
                continue
            if ident_regex and not ident_regex.search(identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file}"
                    f"due to ident filter {ident_filter}"
                )
                continue

            rust_comments = get_rust_comments(rust_definition)
            if rust_comments:
                logging.info(
                    f"Skipping Rust fn {identifier} with existing comments:\
                    \n{rust_comments} in\
                    \n{rust_definition}"
                )
                continue

            if identifier not in c_definitions:
                logging.warning(f"No corresponding C definition found for {identifier}")
                continue

            c_definition = c_definitions[identifier]

            highlighted_c_definition = get_highlighted_c(c_definition)
            logging.debug(
                f"C function {identifier} definition:\n{highlighted_c_definition}\n"
            )

            # Skip functions without comments in C definition
            c_comments = get_c_comments(c_definition)
            if not c_comments:
                logging.info(f"Skipping C function without comments: {identifier}")
                continue

            # logging.debug(f"C function {identifier} comments:\n{c_comments}\n")

            # TODO: make this function take a model and get prompt from model
            prompt_text = """
            Transfer the comments from the following C function to the corresponding Rust function.
            Do not add any comments that are not present in the C function.
            Use Rust doc comment syntax (///) where appropriate (e.g., for function documentation).
            Respond with the Rust function definition with the transferred comments; say nothing else.
            """  # noqa: E501
            prompt_text = dedent(prompt_text).strip()

            prompts.append(
                CommentTransferPrompt(
                    c_function=c_definition,
                    rust_function=rust_definition,
                    prompt_text=prompt_text,
                    identifier=identifier,
                )
            )

        for prompt in prompts:
            messages = [
                {"role": "user", "content": str(prompt)},
            ]

            transform = self.__class__.__name__
            identifier = prompt.identifier
            model = self.model.id
            if not (
                response := self.cache.lookup(
                    transform=transform,
                    identifier=identifier,
                    model=model,
                    messages=messages,
                )
            ):
                response = self.model.generate_with_tools(messages)
                if response is None:
                    logging.error("Model returned no response")
                    continue
                self.cache.update(
                    transform=transform,
                    identifier=identifier,
                    model=model,
                    messages=messages,
                    response=response,
                )

            rust_fn = remove_backticks(response)

            c_comments = get_c_comments(prompt.c_function)
            logging.debug(f"{c_comments=}")

            rust_comments = get_rust_comments(rust_fn)
            logging.debug(f"{rust_comments=}")

            assert c_comments == rust_comments

            print(get_highlighted_rust(rust_fn))

            if update_rust:
                update_rust_definition(
                    root_rust_source_file=root_rust_source_file,
                    identifier=prompt.identifier,
                    new_definition=rust_fn,
                )

    def transfer_comments_dir(
        self,
        root_rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ):
        """
        Run `self.transfer_comments` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.
        """
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            self.transfer_comments(
                root_rust_source_file=root_rust_source_file,
                rust_source_file=rs_path,
                exclude_list=exclude_list,
                ident_filter=ident_filter,
                update_rust=update_rust,
            )
