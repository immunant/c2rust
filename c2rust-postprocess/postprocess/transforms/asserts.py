import logging
from collections.abc import Callable
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import update_rust_definition
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.utils import get_highlighted_rust, remove_backticks

SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that rewrites c2rust-transpiled assert patterns "
    "into idiomatic Rust assert! macros."
)


class AssertsTransformPrompt:
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


class AssertsTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model

    @staticmethod
    def get_validation_fn(expected_assert_count: int) -> Callable[[str], str]:
        def validate_response(rust_fn: str) -> str:
            rust_fn = remove_backticks(rust_fn)

            if "__assert_fail(" in rust_fn:
                return (
                    "FAILURE: Rust function still contains __assert_fail. "
                    "Rewrite those into assert! calls. "
                    "Reply with the full Rust function definition only; "
                    "say nothing else."
                )

            actual_assert_count = rust_fn.count("assert!(")
            if actual_assert_count < expected_assert_count:
                return (
                    "FAILURE: Missing rewritten assert! calls. "
                    f"Expected at least {expected_assert_count}, "
                    f"got {actual_assert_count}. "
                    "Reply with the full Rust function definition only; "
                    "say nothing else."
                )

            return "SUCCESS: Asserts transformed correctly!"

        return validate_response

    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: str,
        identifier: str,
        update_rust: bool = True,
    ) -> str | None:
        expected_assert_count = rust_definition.count("__assert_fail(")
        if expected_assert_count == 0:
            logging.info(
                f"{self.__class__.__name__}: "
                f"Skipping function without transpiled asserts: {identifier}"
            )
            return

        prompt_text = """
        Rewrite the Rust function below by replacing transpiled C assert-macro
        expansions (which call __assert_fail) with idiomatic Rust assert! calls.

        Requirements:
        - Preserve function behavior.
        - Preserve formatting and indentation.
        - Keep all non-assert logic unchanged.
        - Return the full Rust function definition only; say nothing else.
        """
        prompt_text = dedent(prompt_text).strip()

        prompt = AssertsTransformPrompt(
            c_function=c_definition,
            rust_function=rust_definition,
            prompt_text=prompt_text,
            identifier=identifier,
        )

        messages = [{"role": "user", "content": str(prompt)}]

        transform = self.__class__.__name__
        model = self.model.id

        if response := self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        ):
            return

        validate_response = self.get_validation_fn(expected_assert_count)

        # TODO: control attempts from command line args
        for _attempt in range(3):
            response = self.model.generate_with_tools(
                messages, tools=[validate_response]
            )
            if response is None:
                logging.warning("Model returned no response")
                continue

            if response.strip() == "":
                logging.warning("Model returned empty response")
                continue

            validation_result = validate_response(response)
            if not validation_result.startswith("SUCCESS"):
                logging.warning(
                    f"Model response for {identifier} failed validation: "
                    + validation_result
                    + "\nResponse was:\n"
                    + response
                )
                continue

            break
        else:
            logging.error(
                f"Model failed to produce valid response after multiple "
                f"attempts for {identifier}"
            )
            return

        self.cache.update(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
            response=response,
        )

        rust_fn = remove_backticks(response)
        if rust_fn == rust_definition:
            logging.warning(
                f"{self.__class__.__name__}: "
                f"No assert rewrite changes for function: {identifier}"
            )
            return

        print(get_highlighted_rust(rust_fn))

        # TODO: update rust before updating cache as the update step can fail if
        #       LLM output was invalid?
        if update_rust:
            update_rust_definition(
                root_rust_source_file=rust_source_file,
                identifier=identifier,
                new_definition=rust_fn,
            )
