import logging
from collections.abc import Callable
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    get_c_comments,
    get_rust_comments,
    update_rust_definition,
)
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.transforms.trim import TrimTransform
from postprocess.utils import get_highlighted_rust, remove_backticks

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
        self.trim_transform = TrimTransform(cache, model)

    # TODO: it is unclear what kind of output is most useful to a given model
    #       we probably want a per-model validation function and a way to figure
    #       out which validation functions are most useful for a given model.
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

    def apply_item(
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

        match self.trim_transform.apply_item(
            rust_source_file=rust_source_file,
            rust_definition=rust_definition,
            c_definition=c_definition,
            identifier=identifier,
            update_rust=False,  # nothing to update here
        ):
            case None:
                logging.error(
                    f"Trim transform failed for {identifier}, "
                    "skipping comments transfer"
                )
                return
            case str() as trimmed_c_definition:
                c_definition = trimmed_c_definition
            case _:
                raise AssertionError(
                    "Unexpected return type from trim transform: expected None or str"
                )

        # TODO: make this function take a model and get prompt from model
        prompt_text = """
        Transfer the comments from the following C function to the corresponding Rust function.
        Do not add any comments that are not present in the C function.
        Use Rust doc comment syntax (///) where appropriate (e.g., for function documentation).
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

        c_comments = get_c_comments(prompt.c_function)
        logging.debug(f"{c_comments=}")

        # TODO: One-size-fits-all validation strategy may not be ideal.
        #       Maybe the validator needs to be customized to the model?
        validate_response = self.get_validation_fn(c_comments)

        response = self.model.generate_with_tools(messages, tools=[validate_response])
        if response is None:
            logging.error("Model returned no response")
            return

        validation_result = validate_response(response)
        # TODO: shouldn't assert
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

        rust_comments = get_rust_comments(rust_fn)
        logging.debug(f"{rust_comments=}")

        # TODO: move this to apply_file?
        # the challenge is that not all transforms will update Rust code
        if update_rust:
            update_rust_definition(
                root_rust_source_file=rust_source_file,
                identifier=prompt.identifier,
                new_definition=rust_fn,
            )
