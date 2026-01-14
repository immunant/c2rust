import argparse
from pathlib import Path

import pytest
from conftest import EXAMPLES_ROOT

from postprocess.utils import check_isinstance, existing_file, get_rust_files


def test_existing_file_returns_path(tmp_path: Path) -> None:
    target = tmp_path / "compile_commands.json"
    target.write_text("{}")

    result = existing_file(str(target))

    assert result == target
    result = check_isinstance(result, Path)


def test_existing_file_rejects_missing_file(tmp_path: Path) -> None:
    missing = tmp_path / "missing.json"

    with pytest.raises(argparse.ArgumentTypeError) as excinfo:
        existing_file(str(missing))

    assert "is not a readable file" in str(excinfo.value)


def test_existing_file_rejects_directory(tmp_path: Path) -> None:
    with pytest.raises(argparse.ArgumentTypeError):
        existing_file(str(tmp_path))


def test_get_rust_files_raises_on_missing_path():
    """
    Verifies that FileNotFoundError is raised when the path does not exist.
    """
    bad_path = Path("/path/to/nowhere/ghost_dir")

    # Use pytest.raises as a context manager
    with pytest.raises(FileNotFoundError, match="does not exist"):
        get_rust_files(bad_path)


def test_get_rust_files_happy_path(tmp_path):
    (tmp_path / "main.rs").touch()
    (tmp_path / "ignored.txt").touch()  # Should be ignored

    results = get_rust_files(tmp_path)

    assert len(results) == 1
    assert results[0].name == "main.rs"


def test_test_rust_files_finds_qsort_rs(transpile_qsort):
    files = get_rust_files(EXAMPLES_ROOT / "qsort")

    assert len(files) == 1
    assert files[0].is_file()
    assert files[0].name == "qsort.rs"
