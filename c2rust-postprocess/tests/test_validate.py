import shutil
import textwrap
from pathlib import Path

import pytest

from postprocess.validate import (
    BatchValidator,
    Candidate,
    CargoChecker,
    find_manifest,
)


def append_candidate(path: Path, line: str) -> Candidate:
    def apply() -> None:
        path.write_text(path.read_text() + line + "\n")

    return Candidate(
        identifier=line, files=(path,), apply=apply, invalidate=lambda: None
    )


class ContentCheck:
    """Fails when the bad marker is present in the file."""

    def __init__(self, path: Path, bad: str = "BAD"):
        self.path = path
        self.bad = bad
        self.calls = 0

    def __call__(self) -> str | None:
        self.calls += 1
        if self.bad in self.path.read_text():
            return f"found {self.bad}"
        return None


@pytest.fixture
def source(tmp_path: Path) -> Path:
    path = tmp_path / "lib.rs"
    path.write_text("baseline\n")
    return path


def test_valid_batch_is_retained_with_one_check(source: Path) -> None:
    check = ContentCheck(source)
    candidates = [append_candidate(source, "a"), append_candidate(source, "b")]

    accepted, rejected = BatchValidator(check).validate(candidates)

    assert accepted == candidates
    assert rejected == []
    assert check.calls == 1
    assert source.read_text() == "baseline\na\nb\n"


def test_mixed_batch_retains_only_valid_candidates(source: Path) -> None:
    check = ContentCheck(source)
    candidates = [
        append_candidate(source, "a"),
        append_candidate(source, "BAD"),
        append_candidate(source, "c"),
        append_candidate(source, "d"),
    ]

    accepted, rejected = BatchValidator(check).validate(candidates)

    assert [c.identifier for c in accepted] == ["a", "c", "d"]
    assert [(c.identifier, error) for c, error in rejected] == [("BAD", "found BAD")]
    assert source.read_text() == "baseline\na\nc\nd\n"


def test_single_rejected_candidate_restores_baseline_exactly(source: Path) -> None:
    accepted, rejected = BatchValidator(ContentCheck(source)).validate(
        [append_candidate(source, "BAD")]
    )

    assert accepted == []
    assert [c.identifier for c, _ in rejected] == ["BAD"]
    assert source.read_text() == "baseline\n"


def test_interacting_candidates_reject_only_one(source: Path) -> None:
    # x and y are individually fine but invalid together; the retained x
    # becomes part of the baseline that isolates y.
    def check() -> str | None:
        content = source.read_text()
        if "x" in content and "y" in content:
            return "x and y conflict"
        return None

    accepted, rejected = BatchValidator(check).validate(
        [append_candidate(source, "x"), append_candidate(source, "y")]
    )

    assert [c.identifier for c in accepted] == ["x"]
    assert [c.identifier for c, _ in rejected] == ["y"]
    assert source.read_text() == "baseline\nx\n"


def test_check_exception_restores_file(source: Path) -> None:
    def check() -> str | None:
        raise RuntimeError("cargo crashed")

    with pytest.raises(RuntimeError, match="cargo crashed"):
        BatchValidator(check).validate([append_candidate(source, "a")])
    assert source.read_text() == "baseline\n"


def test_apply_exception_restores_file(source: Path) -> None:
    def broken_apply() -> None:
        source.write_text("garbage")
        raise RuntimeError("merge failed")

    candidate = Candidate(
        identifier="a", files=(source,), apply=broken_apply, invalidate=lambda: None
    )
    with pytest.raises(RuntimeError, match="merge failed"):
        BatchValidator(ContentCheck(source)).validate([candidate])
    assert source.read_text() == "baseline\n"


def test_empty_batch_runs_no_check(source: Path) -> None:
    check = ContentCheck(source)
    assert BatchValidator(check).validate([]) == ([], [])
    assert check.calls == 0


def test_find_manifest_returns_nearest(tmp_path: Path) -> None:
    (tmp_path / "Cargo.toml").write_text("")
    src_dir = tmp_path / "src"
    src_dir.mkdir()
    lib_rs = src_dir / "lib.rs"
    lib_rs.write_text("")
    assert find_manifest(lib_rs) == (tmp_path / "Cargo.toml").resolve()


needs_cargo = pytest.mark.skipif(
    shutil.which("cargo") is None, reason="cargo not installed"
)


def make_crate(crate_dir: Path, lib_rs: str) -> Path:
    (crate_dir / "Cargo.toml").write_text(
        textwrap.dedent("""\
            [package]
            name = "validate-test"
            version = "0.0.0"
            edition = "2021"

            [lib]
            path = "lib.rs"
        """)
    )
    (crate_dir / "lib.rs").write_text(lib_rs)
    return crate_dir / "Cargo.toml"


@needs_cargo
def test_cargo_checker_passes_valid_crate(tmp_path: Path) -> None:
    manifest = make_crate(tmp_path, "pub fn f() -> i32 { 1 }\n")
    assert CargoChecker(manifest)() is None


@needs_cargo
def test_cargo_checker_reports_type_error(tmp_path: Path) -> None:
    manifest = make_crate(tmp_path, 'pub fn f() -> i32 { "oops" }\n')
    error = CargoChecker(manifest)()
    assert error is not None
    assert "mismatched types" in error

