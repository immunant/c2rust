from pathlib import Path

import pytest

from postprocess.exclude_list import IdentifierExcludeList


@pytest.mark.parametrize("absolute_exclude", [False, True])
@pytest.mark.parametrize("absolute_source", [False, True])
def test_matches_relative_and_absolute_paths(
    tmp_path: Path, monkeypatch, absolute_exclude: bool, absolute_source: bool
) -> None:
    monkeypatch.chdir(tmp_path)
    Path("project").mkdir()
    exclude_path = Path("project/exclude.yml")
    exclude_path.write_text("src/lib.rs:\n  - excluded\n")
    source_path = Path("project/src/lib.rs")
    if absolute_exclude:
        exclude_path = exclude_path.resolve()
    if absolute_source:
        source_path = source_path.resolve()

    exclude_list = IdentifierExcludeList(exclude_path)

    assert exclude_list.contains(source_path, "excluded")
    assert not exclude_list.contains(source_path, "included")


def test_paths_resolve_relative_to_exclude_file(tmp_path: Path) -> None:
    config_dir = tmp_path / "config"
    config_dir.mkdir()
    exclude_path = config_dir / "exclude.yml"
    exclude_path.write_text("../src/lib.rs:\n  - excluded\n")
    exclude_list = IdentifierExcludeList(exclude_path)

    assert exclude_list.contains(tmp_path / "src/lib.rs", "excluded")
    assert not exclude_list.contains(tmp_path / "other/lib.rs", "excluded")


def test_combines_identifiers_for_equivalent_paths(tmp_path: Path) -> None:
    exclude_path = tmp_path / "exclude.yml"
    exclude_path.write_text("src/lib.rs:\n  - first\n./src/lib.rs:\n  - second\n")
    exclude_list = IdentifierExcludeList(exclude_path)
    source_path = tmp_path / "src/lib.rs"

    assert exclude_list.contains(source_path, "first")
    assert exclude_list.contains(source_path, "second")
    assert not exclude_list.contains(source_path, "s")
