import logging
from pathlib import Path
from textwrap import dedent

from pygments import lex
from pygments.lexers.c_cpp import CLexer

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition, get_c_comments, is_comment_token
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.utils import remove_backticks

SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that removes top-of-file prologues and"
    " other unnecessary content such as preprocessor definitions and"
    " comments that preceede and are unrelated to the definition of a C function."
)


def _scan_lines(code: str) -> list[tuple[bool, bool]]:
    """Classify each line of C code as `(has_comment, has_code)`."""
    n_lines = len(code.splitlines())
    flags = [[False, False] for _ in range(n_lines)]
    line = 0
    for tok_type, tok_value in lex(code, CLexer()):
        for i, part in enumerate(tok_value.split("\n")):
            if i:
                line += 1
            if line >= n_lines or not part.strip():
                continue
            if is_comment_token(tok_type):
                flags[line][0] = True  # has_comment
            else:
                flags[line][1] = True  # has_code
    return [(has_comment, has_code) for has_comment, has_code in flags]


def _prefix_needs_trim(definition: str, decl_line: int) -> bool:
    """
    Whether the front matter before the decl contains comments beyond a single
    comment block adjacent to the decl (i.e. beyond its doc comment).
    """
    lines = _scan_lines("\n".join(definition.splitlines()[:decl_line]))
    i = len(lines) - 1
    while i >= 0 and lines[i] == (False, False):
        i -= 1  # blank lines between the doc comment and the decl
    while i >= 0 and lines[i] == (True, False):
        i -= 1  # the doc comment itself
    return any(has_comment for has_comment, _ in lines[: i + 1])


def _is_suffix_ignoring_whitespace(trimmed: str, original: str) -> bool:
    squashed_trimmed = "".join(trimmed.split())
    return bool(squashed_trimmed) and "".join(original.split()).endswith(
        squashed_trimmed
    )


class TrimTransform(AbstractTransform):
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
    ) -> str | None:
        definition = c_definition.definition

        # Decide deterministically whether trimming is needed at all; only the
        # judgment of which prefix comments belong to the function is left to
        # the model. Without `decl_line` (older c_decls.json), always attempt.
        if c_definition.decl_line is not None and not _prefix_needs_trim(
            definition, c_definition.decl_line
        ):
            logging.info(
                f"{self.__class__.__name__}: "
                f"Skipping C function without front matter to trim: {identifier}"
            )
            return None

        if not get_c_comments(definition):
            logging.info(
                f"{self.__class__.__name__}: "
                f"Skipping C function without comments: {identifier}"
            )
            return None

        prompt = """
        Remove any prologues, preprocessor definitions, and comments that are unrelated to the definition
        of the C function `{identifier}`. Respond with the trimmed C function definition; say nothing else.

        C function:
        ```c
        {c_definition}
        ```
        """  # noqa: E501
        prompt = dedent(
            prompt
        ).strip()  # note: dedent then format since the C definition isn't indented
        prompt = prompt.format(identifier=identifier, c_definition=definition)

        messages = [
            {"role": "user", "content": prompt},
        ]

        transform = self.__class__.__name__
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
            logging.error("Model returned no response")
            return None

        trimmed = remove_backticks(response)

        # A correct trim is a trailing slice of its input. This also keeps a
        # response with hallucinated comment edits from becoming the ground
        # truth that comment transfer is later validated against.
        if not _is_suffix_ignoring_whitespace(trimmed, definition):
            logging.warning(
                f"{self.__class__.__name__}: "
                f"response is not a trailing slice of the input for {identifier};"
                " ignoring it"
            )
            return None

        if not cache_hit:
            self.cache.update(
                transform=transform,
                identifier=identifier,
                model=model,
                messages=messages,
                response=response,
            )

        return trimmed
