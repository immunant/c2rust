import logging
import re
from collections.abc import Callable
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
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.utils import get_highlighted_c, get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
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

    @staticmethod
    def get_validation_fn(c_comments: list[str]) -> Callable[[str], str]:
        def validate_response(rust_fn: str) -> str:
            """Check whether the Rust function's comments match the C source.

            Returns a success message when every comment matches; otherwise
            enumerates missing or extra comments so the model can fix them.

            param: rust_fn: The Rust function definition to validate.
            return: A diagnostic message starting with `SUCCESS` or `FAILURE`.
            """
            nonlocal c_comments
            rust_fn = remove_backticks(rust_fn)
            rust_comments = get_rust_comments(rust_fn)
            logging.debug(f"{rust_comments=}")

            diagnostic = "SUCCESS: Comments transferred correctly!"
            if rust_comments == c_comments:
                logging.debug(diagnostic)
                return diagnostic

            diagnostic = "FAILURE: Comments not transferred correctly."
            rust_comments = set(rust_comments)
            for c_comment in c_comments:
                if c_comment not in rust_comments:
                    diagnostic += (
                        f"\nMissing comment in Rust function: {c_comment}. Insert it!"
                    )

            c_comments_set = set(c_comments)
            for rust_comment in rust_comments:
                if rust_comment not in c_comments_set:
                    diagnostic += (
                        f"\nExtra comment in Rust function: {rust_comment}. Remove it!"
                    )

            logging.debug(diagnostic)
            return diagnostic

        return validate_response

    def apply(
        self, root_rust_source_file: Path, ident_filter: str | None = None
    ) -> None:
        pattern = re.compile(ident_filter) if ident_filter else None

        rust_definitions = get_rust_definitions(root_rust_source_file)
        c_definitions = get_c_definitions(root_rust_source_file)

        logging.info(f"Loaded {len(rust_definitions)} Rust definitions")
        logging.info(f"Loaded {len(c_definitions)} C definitions")

        prompts: list[CommentsTransformPrompt] = []
        for identifier, rust_definition in rust_definitions.items():
            if pattern and not pattern.search(identifier):
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
                CommentsTransformPrompt(
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
                c_comments = get_c_comments(prompt.c_function)
                logging.debug(f"{c_comments=}")

                # TODO: One-size-fits-all validation strategy may not be ideal.
                #       Maybe the validator needs to be customized to the model?
                validate_response = self.get_validation_fn(c_comments)

                response = self.model.generate_with_tools(
                    messages, tools=[validate_response]
                )
                if response is None:
                    logging.error("Model returned no response")
                    continue

                validation_result = validate_response(response)
                assert validation_result.startswith("SUCCESS"), (
                    "Model response failed validation: " + validation_result
                )

                self.cache.update(
                    transform=transform,
                    identifier=identifier,
                    model=model,
                    messages=messages,
                    response=response,
                )

            rust_fn = remove_backticks(response)

            print(get_highlighted_rust(rust_fn))

            update_rust_definition(root_rust_source_file, prompt.identifier, rust_fn)
