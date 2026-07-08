import logging
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    CDefinition,
    demote_misplaced_doc_comments,
    get_c_comments,
    get_rust_comments,
    rust_parse_has_errors,
    update_rust_definition,
)
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.transforms.base import AbstractTransform, TransformError
from postprocess.transforms.trim import TrimTransform
from postprocess.utils import get_highlighted_rust, remove_backticks

# TODO: get from model
SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that transfers comments from C code to Rust code."
)


@dataclass(slots=True)
class CommentsTransformPrompt:
    c_function: str
    c_comments: list[str]
    rust_function: str
    prompt_text: str
    identifier: str

    def __str__(self) -> str:
        return (
            self.prompt_text
            + "\n\n"
            + "Comment lines to transfer:\n```\n"
            + "\n".join(self.c_comments)
            + "\n```\n\n"
            + "C function:\n```c\n"
            + self.c_function
            + "```\n\n"
            + "Rust function:\n```rust\n"
            + self.rust_function
            + "```\n"
        )


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
        self.trim_transform = TrimTransform(cache, model)

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
                \n{get_highlighted_rust(rust_definition)}"
            )
            return

        c_comments = _get_transferable_c_comments(c_definition)
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
                logging.info(
                    f"Trim transform produced no trimmed C definition for "
                    f"{identifier}; continuing with the original definition"
                )
            case str() as trimmed_c_definition:
                # Keep the (untrimmed) preprocessed definition: it is only used
                # as a comment multiset when computing transferable comments,
                # so trimming it is unnecessary.
                c_definition = CDefinition(
                    definition=trimmed_c_definition,
                    preprocessed_definition=c_definition.preprocessed_definition,
                )
                c_comments = _get_transferable_c_comments(c_definition)
                if not c_comments:
                    logging.info(f"Skipping C function without comments: {identifier}")
                    return
            case _:
                raise AssertionError(
                    "Unexpected return type from trim transform: expected None or str"
                )

        # TODO: make this function take a model and get prompt from model
        prompt_text = """
        Transfer the comments from the following C function to the corresponding Rust function.
        Transfer exactly the comment lines listed below, in order; do not transfer any other
        comments in the C function (they may come from inactive preprocessor regions) and do
        not add new ones.
        Use Rust doc comment syntax (///) only for comments placed before the function signature;
        inside the function body use plain // comments (/// before a statement does not compile).
        Respond with the Rust function definition with the transferred comments; say nothing else.
        """  # noqa: E501
        prompt_text = dedent(prompt_text).strip()

        prompt = CommentsTransformPrompt(
            c_function=c_definition.definition,
            c_comments=c_comments,
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

        if rust_parse_has_errors(rust_fn):
            raise TransformError(
                f"model response for {identifier} is not syntactically valid Rust"
            )

        rust_fn = demote_misplaced_doc_comments(rust_fn)

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

        logging.info(
            f"Comments transferred to Rust fn {identifier}:\
                \n{get_highlighted_rust(rust_fn)}"
        )

        # TODO: move this to apply_file?
        # the challenge is that not all transforms will update Rust code
        if update_rust:
            update_rust_definition(
                root_rust_source_file=rust_source_file,
                identifier=prompt.identifier,
                new_definition=rust_fn,
            )
