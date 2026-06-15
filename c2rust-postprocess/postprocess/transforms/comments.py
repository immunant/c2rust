import logging
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    CDefinition,
    MergeRustError,
    get_c_comments,
    get_rust_comments,
)
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.transforms.base import (
    AbstractTransform,
    TransformCandidate,
    TransformError,
)
from postprocess.transforms.trim import TrimTransform
from postprocess.utils import get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
)


class CommentsTransformPrompt:
    c_function: str
    c_function_preprocessed: str | None
    rust_function: str
    prompt_text: str
    identifier: str

    __slots__ = (
        "c_function",
        "c_function_preprocessed",
        "rust_function",
        "prompt_text",
        "identifier",
    )

    def __init__(
        self,
        c_function: str,
        c_function_preprocessed: str | None,
        rust_function: str,
        prompt_text: str,
        identifier: str,
    ):
        self.c_function = c_function
        self.c_function_preprocessed = c_function_preprocessed
        self.rust_function = rust_function
        self.prompt_text = prompt_text
        self.identifier = identifier

    def __str__(self) -> str:
        prompt = (
            self.prompt_text
            + "\n\n"
            + "C function:\n```c\n"
            + self.c_function
            + "```\n\n"
        )
        if self.c_function_preprocessed is not None:
            prompt += (
                "The same C function after preprocessing; comments that do not"
                " appear here were in inactive preprocessor regions and must"
                " not be transferred:\n```c\n"
                + self.c_function_preprocessed
                + "```\n\n"
            )
        prompt += "Rust function:\n```rust\n" + self.rust_function + "```\n"
        return prompt


class CommentsTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model
        self.trim_transform = TrimTransform(cache, model)

    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
        attempt: int = 0,
        previous_error: MergeRustError | None = None,
    ) -> TransformCandidate | None:
        _ = attempt
        rust_comments = get_rust_comments(rust_definition)
        if rust_comments:
            logging.info(
                f"Skipping Rust fn {identifier} with existing comments:\
                \n{get_highlighted_rust(rust_definition)}"
            )
            return

        # Skip functions without comments that survive preprocessing; comments
        # only present in inactive preprocessor regions must not be transferred.
        c_comments = get_c_comments(c_definition.effective)
        if not c_comments:
            logging.info(f"Skipping C function without comments: {identifier}")
            return

        match self.trim_transform.apply_ident(
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
                # TODO: consider trimming both the definition and the preprocessed
                #       definition instead of possibly replacing the original
                #       definition with the trimmed and preprocessed one.
                c_definition = CDefinition(
                    definition=trimmed_c_definition, preprocessed_definition=None
                )
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
            c_function=c_definition.definition,
            # Only include the preprocessed text when it differs, so prompts
            # (and thus cache keys) are unchanged for directive-free functions.
            c_function_preprocessed=(
                c_definition.preprocessed_definition
                if c_definition.was_changed_by_preprocessing
                else None
            ),
            rust_function=rust_definition,
            prompt_text=prompt_text,
            identifier=identifier,
        )

        messages = [
            {"role": "user", "content": str(prompt)},
        ]
        messages = self.with_merge_retry_message(messages, previous_error)

        transform = self.__class__.__name__
        identifier = prompt.identifier
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

        response = self.model.generate_with_tools(messages)

        if response is None:
            if api_key_from_env(model) is None:
                # No API key set: skip uncached entries instead of failing.
                logging.warning(
                    f"Cache miss for {identifier}; skipping since no API key was set..."
                )
                return
            raise TransformError(f"model returned no response for {identifier}")

        rust_fn = remove_backticks(response)

        c_comments = get_c_comments(c_definition.effective)
        logging.debug(f"{c_comments=}")

        rust_comments = get_rust_comments(rust_fn)
        logging.debug(f"{rust_comments=}")

        if c_comments != rust_comments:
            raise TransformError(
                f"comments were not transferred verbatim for {identifier}:"
                f"\n{c_comments=}\n{rust_comments=}"
            )

        logging.info(
            f"Comments transferred to Rust fn {identifier}:\
                \n{get_highlighted_rust(rust_fn)}"
        )

        return TransformCandidate(
            identifier=identifier,
            messages=messages,
            response=response,
            definition=rust_fn,
        )
