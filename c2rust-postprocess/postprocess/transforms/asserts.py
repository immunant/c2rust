import logging
from collections.abc import Callable
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform, TransformError
from postprocess.utils import remove_backticks

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
            + "\n```\n\n"
            + "Rust function:\n```rust\n"
            + self.rust_function
            + "\n```\n"
        )


class AssertsTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION, cache, model)

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

    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
    ) -> str | None:
        _ = rust_source_file
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
            c_function=c_definition.effective,
            rust_function=rust_definition,
            prompt_text=prompt_text,
            identifier=identifier,
        )

        messages = [{"role": "user", "content": str(prompt)}]
        validate_response = self.get_validation_fn(expected_assert_count)

        def check(response: str) -> str:
            if response.strip() == "":
                raise TransformError("model returned an empty response")

            validation_result = validate_response(response)
            if not validation_result.startswith("SUCCESS"):
                raise TransformError(
                    f"model response for {identifier} failed validation: "
                    f"{validation_result}\nResponse was:\n{response}"
                )
            return remove_backticks(response)

        return self.generate(
            identifier,
            messages,
            check,
            tools=[validate_response],
        )
