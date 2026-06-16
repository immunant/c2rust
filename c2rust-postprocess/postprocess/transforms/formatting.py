import logging
import re
from collections.abc import Callable
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition, MergeRustError
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.transforms.base import (
    AbstractTransform,
    TransformCandidate,
    TransformError,
)
from postprocess.utils import get_highlighted_rust, remove_backticks

SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that conservatively reformats c2rust-transpiled "
    "Rust functions for compactness while preserving idiomatic Rust formatting."
)


class FormattingTransformPrompt:
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


class FormattingTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model

    @staticmethod
    def should_attempt_formatting(c_function: str, rust_function: str) -> bool:
        c_line_count = len([line for line in c_function.splitlines() if line.strip()])
        rust_line_count = len(
            [line for line in rust_function.splitlines() if line.strip()]
        )

        if rust_line_count <= max(c_line_count * 2, c_line_count + 20):
            return False

        compactable_item_lines = 0
        for line in rust_function.splitlines():
            stripped = line.strip()
            if len(stripped) <= 48 and re.fullmatch(r"[^,{};]+,", stripped):
                compactable_item_lines += 1

        return " = [" in rust_function and compactable_item_lines >= 16

    @staticmethod
    def get_validation_fn(identifier: str) -> Callable[[str], str]:
        def validate_response(rust_fn: str) -> str:
            rust_fn = remove_backticks(rust_fn).strip()

            if not rust_fn:
                return (
                    "FAILURE: Empty response. Reply with the full Rust function "
                    "definition only; say nothing else."
                )

            if "```" in rust_fn:
                return (
                    "FAILURE: Response contains Markdown code fences. Reply with the "
                    "full Rust function definition only; say nothing else."
                )

            if f"fn {identifier}" not in rust_fn:
                return (
                    f"FAILURE: Response does not contain function `{identifier}`. "
                    "Reply with the full Rust function definition only; say nothing else."  # noqa: E501
                )

            return "SUCCESS: Function formatted correctly!"

        return validate_response

    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
        attempt: int = 0,
        previous_error: MergeRustError | None = None,
    ) -> TransformCandidate | None:
        _ = (rust_source_file, attempt)
        if not self.should_attempt_formatting(c_definition.effective, rust_definition):
            logging.info(
                f"{self.__class__.__name__}: "
                f"Skipping function without obvious compactness issue: {identifier}"
            )
            return

        prompt_text = """
        Reformat the Rust function below only where the transpiled formatting is
        needlessly verbose compared with the corresponding C function.

        Most Rust functions should stay exactly as rustfmt would format them. Make
        changes only for mechanically expanded tables, arrays, lookup data, or similar
        data-heavy structures where the Rust version is much longer than the C version
        because rustfmt placed one small element per line.

        Requirements:
        - Preserve Rust syntax, behavior, attributes, signature, names, types,
          expressions, comments, and control flow.
        - Do not try to make ordinary Rust statements imitate C brace or indentation
          style. Keep ordinary code idiomatic for Rust.
        - For compacted data structures, take formatting clues from the C version:
          group comparable numbers of elements per line, keep related comments near the
          same data, and preserve useful visual structure.
        - Add #[rustfmt::skip] to the function if needed so rustfmt will not expand the
          compacted data structure again.
        - If there is no clear table/array/data-structure compactness problem, return
          the original Rust function unchanged.
        - Return the full Rust function definition only; say nothing else.
        """
        prompt_text = dedent(prompt_text).strip()

        prompt = FormattingTransformPrompt(
            c_function=c_definition.effective,
            rust_function=rust_definition,
            prompt_text=prompt_text,
            identifier=identifier,
        )

        messages = [{"role": "user", "content": str(prompt)}]
        messages = self.with_merge_retry_message(messages, previous_error)

        transform = self.__class__.__name__
        model = self.model.id

        if response := self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        ):
            rust_fn = remove_backticks(response)
            return TransformCandidate(
                identifier=identifier,
                messages=messages,
                response=response,
                definition=rust_fn,
            )

        validate_response = self.get_validation_fn(identifier)

        for _attempt in range(3):
            response = self.model.generate_with_tools(
                messages, tools=[validate_response]
            )
            if response is None:
                if api_key_from_env(model) is None:
                    logging.warning(
                        f"Cache miss for {identifier}; "
                        "skipping since no API key was set..."
                    )
                    return
                logging.warning("Model returned no response")
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
            raise TransformError(
                f"Model failed to produce valid response after multiple "
                f"attempts for {identifier}"
            )

        rust_fn = remove_backticks(response)
        if rust_fn == rust_definition:
            logging.warning(
                f"{self.__class__.__name__}: "
                f"No formatting changes for function: {identifier}"
            )
        else:
            logging.info(
                f"Formatted Rust fn {identifier}:\
                    \n{get_highlighted_rust(rust_fn)}"
            )

        return TransformCandidate(
            identifier=identifier,
            messages=messages,
            response=response,
            definition=rust_fn,
        )
