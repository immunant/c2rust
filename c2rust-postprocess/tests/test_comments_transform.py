from pathlib import Path
from typing import Any

import pytest

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms import base
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

    def invalidate(self, *, transform: str, identifier: str) -> None:
        raise AssertionError("no invalidation expected")


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
    decl_line=0,  # no front matter, so the trim transform skips the model
)

RUST_DEFINITION_NO_COMMENTS = """\
pub unsafe extern "C" fn f() -> libc::c_int {
    return 1 as libc::c_int;
}
"""


def test_body_doc_comments_are_demoted(monkeypatch) -> None:
    response = """\
pub unsafe extern "C" fn f() -> libc::c_int {
    /// @note a body comment
    return 1 as libc::c_int;
}
"""
    cache = StaticCache(response)
    transform = CommentsTransform(cache=cache, model=MockGenerativeModel())

    merged: dict[str, str] = {}

    def fake_update(*, root_rust_source_file, identifier, new_definition):
        merged[identifier] = new_definition

    monkeypatch.setattr(base, "update_rust_definition", fake_update)

    transform.apply_ident(
        rust_source_file=Path("unused.rs"),
        rust_definition=RUST_DEFINITION_NO_COMMENTS,
        c_definition=C_DEFINITION_BODY_COMMENT,
        identifier="f",
        update_rust=True,
    )

    assert "/// @note" not in merged["f"]
    assert "// @note a body comment" in merged["f"]


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


class RecordingCache(AbstractCache):
    """Cache with a fixed `CommentsTransform` response that records updates."""

    def __init__(self, response: str | None):
        super().__init__(Path())
        self.response = response
        self.updates: list[tuple[list[dict[str, Any]], str]] = []

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        if transform != "CommentsTransform":
            return None
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
        self.updates.append((messages, response))

    def invalidate(self, *, transform: str, identifier: str) -> None:
        raise AssertionError("no invalidation expected")


class QueuedModel(MockGenerativeModel):
    """Mock model that returns queued responses."""

    def __init__(self, responses: list[str]):
        super().__init__()
        self.responses = responses
        self.calls = 0

    def generate_with_tools(self, messages, tools=(), max_tool_loops=5):
        self.calls += 1
        return self.responses.pop(0)


BAD_RESPONSE = """\
pub unsafe extern "C" fn f( -> libc::c_int {
    /// @note a body comment
    return 1 as libc::c_int;
"""
GOOD_RESPONSE = """\
pub unsafe extern "C" fn f() -> libc::c_int {
    /// @note a body comment
    return 1 as libc::c_int;
}
"""


def apply_to_body_comment_fn(
    cache: AbstractCache, model: MockGenerativeModel
) -> str | None:
    transform = CommentsTransform(cache=cache, model=model)
    return transform.apply_ident(
        rust_source_file=Path("unused.rs"),
        rust_definition=RUST_DEFINITION_NO_COMMENTS,
        c_definition=C_DEFINITION_BODY_COMMENT,
        identifier="f",
        update_rust=False,
    )


def test_rejected_response_is_regenerated(monkeypatch) -> None:
    monkeypatch.setattr(base, "api_key_from_env", lambda model_id: "test-key")
    cache = RecordingCache(None)
    model = QueuedModel([BAD_RESPONSE, GOOD_RESPONSE])

    result = apply_to_body_comment_fn(cache, model)

    assert result is not None
    assert "// @note a body comment" in result
    assert model.calls == 2
    # Only the validated response is cached, keyed by the unchanged prompt.
    [(messages, response)] = cache.updates
    assert len(messages) == 1
    assert response == GOOD_RESPONSE


def test_invalid_cached_response_is_regenerated(monkeypatch) -> None:
    monkeypatch.setattr(base, "api_key_from_env", lambda model_id: "test-key")
    cache = RecordingCache(BAD_RESPONSE)
    model = QueuedModel([GOOD_RESPONSE])

    result = apply_to_body_comment_fn(cache, model)

    assert result is not None
    assert model.calls == 1
    # The stale entry is overwritten under the same key.
    [(messages, response)] = cache.updates
    assert len(messages) == 1
    assert response == GOOD_RESPONSE
