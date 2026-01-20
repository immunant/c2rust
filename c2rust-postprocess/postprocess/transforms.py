import logging
import re
from collections.abc import Generator, Iterable
from dataclasses import dataclass
from difflib import unified_diff
from pathlib import Path
from re import Pattern
from textwrap import dedent

import yaml
from rich.console import Console
from rich.syntax import Syntax

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


@dataclass
class CommentTransferPrompt:
    c_function: str
    rust_function: str
    prompt_text: str
    identifier: str

    root_rust_source_file: Path
    rust_source_file: Path

    def __str__(self) -> str:
        return f"""\
{self.prompt_text}

C function:
```c
{self.c_function}
```

Rust function:
```rust
{self.rust_function}
```
"""

    def messages(self) -> list[dict[str, str]]:
        return [
            {
                "role": "user",
                "content": str(self),
            },
        ]


@dataclass
class CommentTransferOptions:
    exclude_list: IdentifierExcludeList
    ident_filter: str | None = None
    update_rust: bool = True
    fail_fast: bool = True

    def ident_regex(self) -> Pattern[str] | None:
        return re.compile(self.ident_filter) if self.ident_filter else None


@dataclass
class CommentTransferFailure:
    options: CommentTransferOptions
    prompt: CommentTransferPrompt
    c_comments: list[str]
    rust_comments: list[str]

    def header(self) -> str:
        return (
            f"comments differ"
            f" in fn {self.prompt.identifier}"
            f" in {self.prompt.rust_source_file}:"
        )

    def diff(self) -> str:
        return "".join(
            unified_diff(
                a=[f"{comment}\n" for comment in self.c_comments],
                b=[f"{comment}\n" for comment in self.rust_comments],
                # TODO this isn't always exactly correct
                fromfile=f"{self.prompt.rust_source_file.with_suffix('.c')}:{self.prompt.identifier}",
                tofile=f"{self.prompt.rust_source_file}:{self.prompt.identifier}",
            )
        ).rstrip()

    def __str__(self) -> str:
        return f"""\
{self.header()}

```diff
{self.diff()}
```"""

    def print(self):
        console = Console()
        console.print(f"""\
{self.header()}

```diff""")
        console.print(Syntax(self.diff(), "diff"))
        console.print("```")

    @staticmethod
    def to_exclude_file(failures: Iterable["CommentTransferFailure"]) -> str:
        path_to_fns: dict[str, list[str]] = {}
        for failure in failures:
            path = str(failure.prompt.rust_source_file)
            fns = path_to_fns.get(path, [])
            if not fns:
                path_to_fns[path] = fns
            fns.append(failure.prompt.identifier)
        return yaml.dump(path_to_fns)


class CommentTransfer:
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        self.cache = cache
        self.model = model

    def _transfer_comments_prompts(
        self,
        root_rust_source_file: Path,
        rust_source_file: Path,
        options: CommentTransferOptions,
    ) -> Generator[CommentTransferPrompt, None, None]:
        """
        Create all of the `CommentTransferPrompt`s for `rust_source_file`.
        """

        ident_regex = options.ident_regex()

        rust_definitions = get_rust_definitions(rust_source_file)
        c_definitions = get_c_definitions(rust_source_file)

        logging.info(f"Loaded {len(rust_definitions)} Rust definitions")
        logging.info(f"Loaded {len(c_definitions)} C definitions")

        for identifier, rust_definition in rust_definitions.items():
            if ident_regex and not ident_regex.search(identifier):
                logging.info(
                    f"Skipping fn {identifier} in {rust_source_file}"
                    f" due to ident filter {options.ident_filter}"
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

            yield CommentTransferPrompt(
                c_function=c_definition,
                rust_function=rust_definition,
                prompt_text=prompt_text,
                identifier=identifier,
                root_rust_source_file=root_rust_source_file,
                rust_source_file=rust_source_file,
            )

    def _transfer_comments_dir_prompts(
        self,
        root_rust_source_file: Path,
        options: CommentTransferOptions,
    ) -> Generator[CommentTransferPrompt, None, None]:
        """
        Create all of the `CommentTransferPrompt`s for
        the parent directory of `root_rust_source_file`.
        """

        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            yield from self._transfer_comments_prompts(
                root_rust_source_file=root_rust_source_file,
                rust_source_file=rs_path,
                options=options,
            )

    def _run_prompts(
        self,
        prompts: Iterable[CommentTransferPrompt],
        options: CommentTransferOptions,
    ) -> Generator[CommentTransferFailure, None, None]:
        """
        Run all of the `CommentTransferPrompt`s.
        """

        prompts = list(prompts)
        logging.info(f"Transferring comments for {len(prompts)} Rust functions")

        transform = self.__class__.__name__
        model = self.model.id

        if not options.fail_fast:
            # If not failing fast, put all of the cached prompts at the beginning
            # so that the progress is easier to follow.
            cached_prompts = []
            uncached_prompts = []
            for prompt in prompts:
                response = self.cache.lookup(
                    transform=transform,
                    identifier=prompt.identifier,
                    model=model,
                    messages=prompt.messages(),
                )
                if response is None:
                    uncached_prompts.append(prompt)
                else:
                    cached_prompts.append(prompt)

            logging.info(
                "Transferred comments for"
                f" {len(cached_prompts)}/{len(prompts)} cached Rust functions"
            )
            logging.info(
                f"Transferring comments for"
                f" {len(uncached_prompts)}/{len(prompts)} uncached Rust functions"
            )
            prompts = cached_prompts + uncached_prompts

        for prompt_num, prompt in enumerate(prompts):
            logging.info(
                f"[{prompt_num}/{len(prompts)}] Transferring comments to"
                f" fn {prompt.identifier} in {prompt.rust_source_file}"
            )
            messages = prompt.messages()
            if not (
                response := self.cache.lookup(
                    transform=transform,
                    identifier=prompt.identifier,
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
                    identifier=prompt.identifier,
                    model=model,
                    messages=messages,
                    response=response,
                )

            rust_fn = remove_backticks(response)

            c_comments = get_c_comments(prompt.c_function)
            logging.debug(f"{c_comments=}")

            rust_comments = get_rust_comments(rust_fn)
            logging.debug(f"{rust_comments=}")

            # Do the exclusion when checking if the comment transfer was successful,
            # but don't skip actually trying the comment transfer and caching it.
            excluded = options.exclude_list.contains(
                path=prompt.rust_source_file, identifier=prompt.identifier
            )
            if excluded:
                logging.info(
                    f"Skipping fn {prompt.identifier} in {prompt.rust_source_file}"
                    f" due to exclude file {options.exclude_list.src_path}"
                )
                continue

            if c_comments != rust_comments:
                yield CommentTransferFailure(
                    options=options,
                    prompt=prompt,
                    c_comments=c_comments,
                    rust_comments=rust_comments,
                )

            logging.info(get_highlighted_rust(rust_fn))

            if options.update_rust:
                update_rust_definition(
                    root_rust_source_file=prompt.root_rust_source_file,
                    identifier=prompt.identifier,
                    new_definition=rust_fn,
                )

    def transfer_comments(
        self,
        root_rust_source_file: Path,
        rust_source_file: Path,
        options: CommentTransferOptions,
    ) -> Generator[CommentTransferFailure, None, None]:
        yield from self._run_prompts(
            self._transfer_comments_prompts(
                root_rust_source_file=root_rust_source_file,
                rust_source_file=rust_source_file,
                options=options,
            ),
            options=options,
        )

    def transfer_comments_dir(
        self,
        root_rust_source_file: Path,
        options: CommentTransferOptions,
    ) -> Generator[CommentTransferFailure, None, None]:
        """
        Run `self.transfer_comments` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.
        """
        yield from self._run_prompts(
            self._transfer_comments_dir_prompts(
                root_rust_source_file=root_rust_source_file,
                options=options,
            ),
            options=options,
        )
