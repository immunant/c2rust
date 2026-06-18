import logging
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    get_c_comments,
    get_rust_code,
    get_rust_comments,
    update_rust_definition,
)
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.transforms.base import AbstractTransform, TransformError
from postprocess.utils import get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
    " Preserve the Rust code exactly except for comments."
)


class CommentsTransformPrompt:
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


class CommentsTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model

    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: str,
        identifier: str,
        update_rust: bool = True,
    ) -> None:
        rust_comments = get_rust_comments(rust_definition)
        if rust_comments:
            logging.info(
                f"Skipping Rust fn {identifier} with existing comments:\
                \n{rust_comments} in\
                \n{rust_definition}"
            )
            return

        # Skip functions without comments in C definition
        c_comments = get_c_comments(c_definition)
        if not c_comments:
            logging.info(f"Skipping C function without comments: {identifier}")
            return

        # TODO: make this function take a model and get prompt from model
        prompt_text = """
        Transfer the comments from the following C function to the corresponding Rust function.
        Rules:
        - Treat the Rust function as the source of truth for all code.
        - The C function is only a source of comment text and approximate comment placement.
        - Do not copy any C code into Rust, including statements, assertions, macros, or preprocessor directives.
        - Do not change any Rust token, identifier, operator, delimiter, literal, or whitespace outside comments.
        - Do not reformat, rewrite, simplify, repair, or restructure the Rust code.
        - Transfer only actual comments from the C function.
        - Do not change comment text.
        - Preserve the order of transferred comments.
        - Keep each comment in the same relative location as in the C source.
        - Keep leading comments leading and trailing comments trailing.
        - Do not move a comment to another statement, expression, item, or line.
        - If a comment is attached to a statement or expression in the function body, keep it as a normal comment and prefer placing it before that statement rather than after it.
        - Prefer // for ordinary comments.
        - Ordinary block comments must use /* ... */, not /** ... */.
        - Use Rust doc comment syntax (/// or /** ... */) only for a comment that is directly and immediately above the Rust function definition.
        - Never use /// or /** ... */ inside a function body.
        - Do not infer that a C /** ... */ or /// comment should become a Rust doc comment.
        - If a C comment is inside a function body, it must become // or /* ... */.
        - Otherwise keep comments as // or /* ... */.
        Respond with the Rust function definition with the transferred comments; say nothing else.
        """  # noqa: E501
        prompt_text = dedent(prompt_text).strip()

        prompt = CommentsTransformPrompt(
            c_function=c_definition,
            rust_function=rust_definition,
            prompt_text=prompt_text,
            identifier=identifier,
        )

        messages = [
            {"role": "user", "content": str(prompt)},
        ]

        transform = self.__class__.__name__
        identifier = prompt.identifier
        model = self.model.id
        if response := self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        ):
            return

        if api_key_from_env(model) is None:
            # No API key set: skip uncached entries instead of failing.
            logging.warning(
                f"Cache miss for {identifier}; skipping since no API key was set..."
            )
            return

        max_code_mismatch_retries = 3
        original_rust_code = get_rust_code(rust_definition)
        response = None
        rust_fn = ""
        updated_rust_code = []
        for attempt in range(max_code_mismatch_retries + 1):
            response = self.model.generate_with_tools(messages)

            if response is None:
                raise TransformError(f"model returned no response for {identifier}")

            rust_fn = remove_backticks(response)
            updated_rust_code = get_rust_code(rust_fn)
            if original_rust_code == updated_rust_code:
                break

            if attempt < max_code_mismatch_retries:
                logging.warning(
                    "Rust code mismatch for %s on attempt %s/%s; retrying",
                    identifier,
                    attempt + 1,
                    max_code_mismatch_retries + 1,
                )
                continue

            raise TransformError(
                f"Rust code changed for {identifier}:"
                f"\nbefore={original_rust_code!r}"
                f"\nafter={updated_rust_code!r}"
            )

        c_comments = get_c_comments(prompt.c_function)
        logging.debug(f"{c_comments=}")

        rust_comments = get_rust_comments(rust_fn)
        logging.debug(f"{rust_comments=}")

        if c_comments != rust_comments:
            raise TransformError(
                f"comments were not transferred verbatim for {identifier}:"
                f"\n{c_comments=}\n{rust_comments=}"
            )

        self.cache.update(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
            response=response,
        )

        print(get_highlighted_rust(rust_fn))

        # TODO: move this to apply_file?
        # the challenge is that not all transforms will update Rust code
        if update_rust:
            try:
                update_rust_definition(
                    root_rust_source_file=rust_source_file,
                    identifier=prompt.identifier,
                    new_definition=rust_fn,
                )
            except RuntimeError as error:
                raise TransformError(str(error)) from error
