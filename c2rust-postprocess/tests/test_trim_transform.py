from pathlib import Path
from typing import Any

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms.trim import (
    TrimTransform,
    _is_suffix_ignoring_whitespace,
    _prefix_needs_trim,
)

DOC_COMMENT_ONLY = """\
/*
** doc comment for f
*/
int f(void) {
    return 0;
}
"""

LICENSE_THEN_DOC = """\
/* Copyright (c) 2026 Example Corp. */

/* doc comment for f */
int f(void) {
    return 0;
}
"""


def test_prefix_without_comments_needs_no_trim() -> None:
    definition = "int f(void) {\n    return 0;\n}\n"
    assert not _prefix_needs_trim(definition, 0)

    definition = "#include <stdio.h>\n\nint f(void) {\n    return 0;\n}\n"
    assert not _prefix_needs_trim(definition, 2)


def test_adjacent_doc_comment_needs_no_trim() -> None:
    assert not _prefix_needs_trim(DOC_COMMENT_ONLY, 3)


def test_blank_separated_doc_comment_needs_no_trim() -> None:
    definition = "/* doc comment for f */\n\nint f(void) {\n    return 0;\n}\n"
    assert not _prefix_needs_trim(definition, 2)


def test_directives_above_doc_comment_need_no_trim() -> None:
    definition = "#include <stdio.h>\n\n/* doc */\nint f(void) {}\n"
    assert not _prefix_needs_trim(definition, 3)


def test_license_above_doc_comment_needs_trim() -> None:
    assert _prefix_needs_trim(LICENSE_THEN_DOC, 3)


def test_commented_directive_needs_trim() -> None:
    definition = "#define MAX 100 /* max entries */\nint f(void) {}\n"
    assert _prefix_needs_trim(definition, 1)

    definition = "/* max entries */\n#define MAX 100\nint f(void) {}\n"
    assert _prefix_needs_trim(definition, 2)


def test_is_suffix_ignoring_whitespace() -> None:
    original = LICENSE_THEN_DOC
    trimmed = "/* doc comment for f */\nint f(void) {\n    return 0;\n}"
    assert _is_suffix_ignoring_whitespace(trimmed, original)
    assert _is_suffix_ignoring_whitespace(original, original)
    # reformatted whitespace is fine, reworded text is not
    assert _is_suffix_ignoring_whitespace("int f(void) { return 0; }", original)
    assert not _is_suffix_ignoring_whitespace("/* doc for f */ int f(void)", original)
    assert not _is_suffix_ignoring_whitespace("", original)


class RecordingCache(AbstractCache):
    def __init__(self, response: str | None):
        super().__init__(Path())
        self.response = response
        self.lookups = 0
        self.updates = 0

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        self.lookups += 1
        return self.response

    def update(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
        response: str,
    ) -> None:
        self.updates += 1


def apply_trim(c_definition: CDefinition, cache: AbstractCache) -> str | None:
    transform = TrimTransform(cache=cache, model=MockGenerativeModel())
    return transform.apply_ident(
        rust_source_file=Path("unused.rs"),
        rust_definition="fn f() {}",
        c_definition=c_definition,
        identifier="f",
        update_rust=False,
    )


def test_gate_skips_model_when_no_trim_needed() -> None:
    cache = RecordingCache("should not be used")
    c_definition = CDefinition(
        definition=DOC_COMMENT_ONLY, preprocessed_definition=None, decl_line=3
    )
    assert apply_trim(c_definition, cache) is None
    assert cache.lookups == 0


def test_trim_accepts_trailing_slice_response() -> None:
    trimmed = "/* doc comment for f */\nint f(void) {\n    return 0;\n}"
    cache = RecordingCache(f"```c\n{trimmed}\n```")
    c_definition = CDefinition(
        definition=LICENSE_THEN_DOC, preprocessed_definition=None, decl_line=3
    )
    assert apply_trim(c_definition, cache) == trimmed


def test_trim_rejects_non_trailing_slice_response() -> None:
    cache = RecordingCache("/* doc for f */\nint f(void) {\n    return 0;\n}")
    c_definition = CDefinition(
        definition=LICENSE_THEN_DOC, preprocessed_definition=None, decl_line=3
    )
    assert apply_trim(c_definition, cache) is None
