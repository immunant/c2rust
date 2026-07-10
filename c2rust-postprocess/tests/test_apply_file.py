import json
from pathlib import Path
from typing import Any

import pytest
from test_validate import make_crate, needs_cargo

from postprocess.cache import AbstractCache
from postprocess.definitions import CDefinition
from postprocess.exclude_list import IdentifierExcludeList, format_exclude_entries
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms import base
from postprocess.transforms.base import AbstractTransform, TransformError
from postprocess.utils import get_tool_path
from postprocess.validate import BatchValidator, CargoChecker

C_DEFINITION = CDefinition(
    definition="""\
/* file prologue */

/* doc for enabled */
int enabled(void) {
    return 1; /* one */
}
""",
    preprocessed_definition=None,
    decl_line=3,
)

TRIM_RESPONSE = """\
/* doc for enabled */
int enabled(void) {
    return 1; /* one */
}
"""

RUST_DEFINITION = """\
pub unsafe extern "C" fn enabled() -> libc::c_int {
    return 1 as libc::c_int;
}
"""

COMMENTS_RESPONSE = """\
/// doc for enabled
pub unsafe extern "C" fn enabled() -> libc::c_int {
    return 1 as libc::c_int; // one
}
"""


class CannedCache(AbstractCache):
    """Canned per-transform responses; records invalidations."""

    def __init__(self, responses: dict[str, str]):
        super().__init__(Path())
        self.responses = responses
        self.invalidations: list[tuple[str, str]] = []

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        return self.responses.get(transform)

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
        self.invalidations.append((transform, identifier))


def make_comments_transform():
    from postprocess.transforms.comments import CommentsTransform

    cache = CannedCache(
        {"TrimTransform": TRIM_RESPONSE, "CommentsTransform": COMMENTS_RESPONSE}
    )
    return cache, CommentsTransform(cache=cache, model=MockGenerativeModel())


@pytest.fixture
def rust_file(tmp_path: Path) -> Path:
    path = tmp_path / "lib.rs"
    path.write_text("original\n")
    return path


@pytest.fixture
def patched_io(monkeypatch, rust_file: Path) -> None:
    monkeypatch.setattr(
        base, "get_rust_definitions", lambda path: {"enabled": RUST_DEFINITION}
    )
    monkeypatch.setattr(
        base, "get_c_definitions", lambda path: {"enabled": C_DEFINITION}
    )

    def fake_update(*, root_rust_source_file, identifier, new_definition):
        root_rust_source_file.write_text(new_definition)

    monkeypatch.setattr(base, "update_rust_definition", fake_update)


def test_accepted_batch_is_applied(patched_io, rust_file: Path) -> None:
    cache, transform = make_comments_transform()

    result = transform.apply_file(
        rust_source_file=rust_file,
        exclude_list=IdentifierExcludeList(None),
        keep_going=True,
        validator=BatchValidator(lambda: None),
    )

    assert result.failed == []
    assert "/// doc for enabled" in rust_file.read_text()
    assert cache.invalidations == []


def test_rejection_counts_failure_and_invalidates_final_response(
    patched_io, rust_file: Path
) -> None:
    cache, transform = make_comments_transform()

    result = transform.apply_file(
        rust_source_file=rust_file,
        exclude_list=IdentifierExcludeList(None),
        keep_going=True,
        validator=BatchValidator(lambda: "type error"),
    )

    assert result.failed == [(rust_file, "enabled", "rejected by cargo check")]
    assert rust_file.read_text() == "original\n"
    # Only the final rewrite's entry is invalidated; the trim helper's
    # cached C-only result stays valid.
    assert cache.invalidations == [("CommentsTransform", "enabled")]


def test_rejection_raises_without_keep_going(patched_io, rust_file: Path) -> None:
    cache, transform = make_comments_transform()

    with pytest.raises(TransformError, match="cargo check rejected"):
        transform.apply_file(
            rust_source_file=rust_file,
            exclude_list=IdentifierExcludeList(None),
            keep_going=False,
            validator=BatchValidator(lambda: "type error"),
        )

    assert rust_file.read_text() == "original\n"
    assert cache.invalidations == [("CommentsTransform", "enabled")]


def test_no_update_rust_is_purely_generative(patched_io, rust_file: Path) -> None:
    cache, transform = make_comments_transform()

    def exploding_check() -> str | None:
        raise AssertionError("validator must not run")

    result = transform.apply_file(
        rust_source_file=rust_file,
        exclude_list=IdentifierExcludeList(None),
        update_rust=False,
        keep_going=True,
        validator=BatchValidator(exploding_check),
    )

    assert result.failures == 0
    assert rust_file.read_text() == "original\n"


def test_without_validator_candidates_are_applied(patched_io, rust_file: Path) -> None:
    cache, transform = make_comments_transform()

    result = transform.apply_file(
        rust_source_file=rust_file,
        exclude_list=IdentifierExcludeList(None),
        keep_going=True,
        validator=None,
    )

    assert result.failures == 0
    assert "/// doc for enabled" in rust_file.read_text()


def test_format_exclude_entries_groups_by_file(tmp_path: Path) -> None:
    entries = [
        (tmp_path / "repo" / "src" / "a.rs", "f", "rejected by cargo check"),
        (tmp_path / "repo" / "src" / "b.rs", "g", "failed to transform"),
        (tmp_path / "repo" / "src" / "a.rs", "h", "failed to transform"),
    ]
    assert format_exclude_entries(entries, tmp_path) == (
        "repo/src/a.rs:\n"
        "  - f # rejected by cargo check\n"
        "  - h # failed to transform\n"
        "repo/src/b.rs:\n"
        "  - g # failed to transform"
    )


class CannedTransform(AbstractTransform):
    """Returns canned rewrites without calling a model."""

    def __init__(self, rewrites: dict[str, str]):
        super().__init__("", CannedCache({}), MockGenerativeModel())
        self.rewrites = rewrites

    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
    ) -> str | None:
        return self.rewrites.get(identifier)


def split_merge_tools_available() -> bool:
    try:
        get_tool_path("split_rust")
        get_tool_path("merge_rust")
        return True
    except (FileNotFoundError, PermissionError):
        return False


@needs_cargo
def test_end_to_end_type_invalid_rewrite_is_isolated(tmp_path: Path) -> None:
    """
    A syntactically valid but type-invalid rewrite is rejected by cargo and
    rolled back while a neighboring valid rewrite remains applied.
    """
    if not split_merge_tools_available():
        pytest.skip("split_rust/merge_rust binaries not built")

    manifest = make_crate(
        tmp_path,
        "pub fn good() -> i32 { 1 }\n\npub fn bad() -> i32 { 2 }\n",
    )
    lib_rs = tmp_path / "lib.rs"
    (tmp_path / "lib.c_decls.json").write_text(
        json.dumps(
            {
                "definitions": {
                    "good": {"definition": "int good(void) { return 1; }"},
                    "bad": {"definition": "int bad(void) { return 2; }"},
                }
            }
        )
    )

    transform = CannedTransform(
        {
            "good": "// validated comment\npub fn good() -> i32 { 1 }",
            "bad": 'pub fn bad() -> i32 { "oops" }',
        }
    )
    checker = CargoChecker(manifest)
    assert checker() is None  # baseline

    result = transform.apply_file(
        rust_source_file=lib_rs,
        exclude_list=IdentifierExcludeList(None),
        keep_going=True,
        validator=BatchValidator(checker),
    )

    content = lib_rs.read_text()
    assert "// validated comment" in content
    assert "oops" not in content
    assert "pub fn bad() -> i32 { 2 }" in content
    assert result.failed == [(lib_rs, "bad", "rejected by cargo check")]
    assert checker() is None  # final state still compiles
