from pathlib import Path
from typing import Any

import pytest

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms.base import TransformError
from postprocess.transforms.comments import CommentsTransform


class StaticCache(AbstractCache):
    """Cache with a single response for `CommentsTransform` lookups."""

    def __init__(self, response: str):
        super().__init__(Path())
        self.response = response
        self.lookups: list[tuple[str, list[dict[str, Any]]]] = []

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        self.lookups.append((transform, messages))
        if transform == "CommentsTransform":
            return self.response
        return None

    def update(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
        response: str,
    ) -> None:
        raise AssertionError("cached response should not be updated")


def test_directive_line_comment_survives_preprocessed_check() -> None:
    c_definition = CDefinition(
        definition="""\
int enabled(void) {
#ifdef FEATURE
    return 1;
#endif /* enabled path */
}
""",
        preprocessed_definition="""\
int enabled(void) {

    return 1;

}
""",
    )
    rust_definition = """\
pub unsafe extern "C" fn enabled() -> libc::c_int {
    return 1 as libc::c_int;
}
"""
    response = """\
/// enabled path
pub unsafe extern "C" fn enabled() -> libc::c_int {
    return 1 as libc::c_int;
}
"""
    cache = StaticCache(response)
    transform = CommentsTransform(cache=cache, model=MockGenerativeModel())

    transform.apply_ident(
        rust_source_file=Path("unused.rs"),
        rust_definition=rust_definition,
        c_definition=c_definition,
        identifier="enabled",
        update_rust=False,
    )

    transforms = [transform for transform, _ in cache.lookups]
    assert transforms == ["TrimTransform", "CommentsTransform"]
    prompt = cache.lookups[-1][1][0]["content"]
    assert "Comment lines to transfer:\n```\nenabled path\n```" in prompt


C_DEFINITION_BODY_COMMENT = CDefinition(
    definition="""\
int f(void) {
    /// @note a body comment
    return 1;
}
""",
    preprocessed_definition=None,
)

RUST_DEFINITION_NO_COMMENTS = """\
pub unsafe extern "C" fn f() -> libc::c_int {
    return 1 as libc::c_int;
}
"""


def test_syntactically_invalid_response_is_rejected() -> None:
    response = """\
pub unsafe extern "C" fn f( -> libc::c_int {
    /// @note a body comment
    return 1 as libc::c_int;
"""
    cache = StaticCache(response)
    transform = CommentsTransform(cache=cache, model=MockGenerativeModel())

    with pytest.raises(TransformError, match="not syntactically valid"):
        transform.apply_ident(
            rust_source_file=Path("unused.rs"),
            rust_definition=RUST_DEFINITION_NO_COMMENTS,
            c_definition=C_DEFINITION_BODY_COMMENT,
            identifier="f",
            update_rust=False,
        )
