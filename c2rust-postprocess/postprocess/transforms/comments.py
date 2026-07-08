import logging
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    CDefinition,
    get_c_comments,
    get_rust_comments,
    update_rust_definition,
)
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.transforms.base import AbstractTransform, TransformError
from postprocess.utils import get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
)


@dataclass(slots=True)
class CommentsTransformPrompt:
    c_function: str
    c_function_preprocessed: str | None
    rust_function: str
    prompt_text: str
    identifier: str

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
                "The same C function after preprocessing; comments that are"
                " absent here may have been in inactive preprocessor regions"
                " and must not be transferred. Comments on preprocessor"
                " directive lines in the original C function may be absent"
                " here even when they annotate active code:\n```c\n"
                + self.c_function_preprocessed
                + "```\n\n"
            )
        prompt += "Rust function:\n```rust\n" + self.rust_function + "```\n"
        return prompt


def _get_directive_line_comments(code: str) -> list[str]:
    return get_c_comments(
        "\n".join(line for line in code.splitlines() if line.lstrip().startswith("#"))
    )


def _get_transferable_c_comments(c_definition: CDefinition) -> list[str]:
    if c_definition.preprocessed_definition is None:
        return get_c_comments(c_definition.definition)

    wanted_comments = Counter(get_c_comments(c_definition.preprocessed_definition))
    wanted_comments.update(_get_directive_line_comments(c_definition.definition))

    comments = []
    for comment in get_c_comments(c_definition.definition):
        if wanted_comments[comment] > 0:
            comments.append(comment)
            wanted_comments[comment] -= 1
    return comments


class CommentsTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model

    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
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

        c_comments = _get_transferable_c_comments(c_definition)
        if not c_comments:
            logging.info(f"Skipping C function without comments: {identifier}")
            return

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
            c_function_preprocessed=c_definition.preprocessed_if_changed,
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
        response = self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        )
        cache_hit = response is not None

        if response is None:
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

        logging.debug(f"{c_comments=}")

        rust_comments = get_rust_comments(rust_fn)
        logging.debug(f"{rust_comments=}")

        if c_comments != rust_comments:
            raise TransformError(
                f"comments were not transferred verbatim for {identifier}:"
                f"\n{c_comments=}\n{rust_comments=}"
            )

        if not cache_hit:
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
            update_rust_definition(
                root_rust_source_file=rust_source_file,
                identifier=prompt.identifier,
                new_definition=rust_fn,
            )
