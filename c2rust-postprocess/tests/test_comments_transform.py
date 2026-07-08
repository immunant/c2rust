from pathlib import Path
from typing import Any

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms.comments import CommentsTransform


class StaticCache(AbstractCache):
    def __init__(self, response: str):
        super().__init__(Path())
        self.response = response
        self.lookups = 0
        self.messages: list[list[dict[str, Any]]] = []

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        self.lookups += 1
        self.messages.append(messages)
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

    assert cache.lookups == 1
    prompt = cache.messages[0][0]["content"]
    assert "preprocessor directive lines" in prompt
