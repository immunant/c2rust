import json
import os
import shutil
import subprocess
from pathlib import Path

import pytest

TEST_ROOT = Path(__file__).resolve().parents[1]
EXAMPLES_ROOT = TEST_ROOT.parent / "examples"


@pytest.fixture
def ensure_bear():
    if shutil.which("bear") is None:
        pytest.skip("bear not installed")


@pytest.fixture
def generate_compile_commands_for_qsort(ensure_bear):
    qsort_dir = EXAMPLES_ROOT / "qsort"
    compile_commands = qsort_dir / "compile_commands.json"

    # compile to /dev/null, we just want compile_commands.json
    subprocess.run(
        ["bear", "--", "cc", "-c", qsort_dir / "qsort.c", "-o", "/dev/null"],
        cwd=qsort_dir,
        check=True,
        capture_output=True,
    )

    commands = json.loads(compile_commands.read_text())

    # check that compile commands contains one entry
    assert compile_commands.exists()
    assert isinstance(commands, list)
    assert len(commands) == 1

    yield commands

    compile_commands.unlink(missing_ok=True)


@pytest.fixture
def transpile_qsort(generate_compile_commands_for_qsort):
    qsort_dir = EXAMPLES_ROOT / "qsort"
    qsort_rs = qsort_dir / "qsort.rs"

    c2rust = TEST_ROOT.parent / "target" / "release" / "c2rust"
    if not (c2rust.is_file() and os.access(c2rust, os.X_OK)):
        pytest.skip(f"{c2rust}: binary not found or not executable")

    compile_commands = qsort_dir / "compile_commands.json"

    subprocess.run(
        [
            c2rust,
            "transpile",
            "--overwrite-existing",
            "--emit-c-decl-map",
            compile_commands,
        ],
        check=True,
    )

    qsort_rs = qsort_dir / "qsort.rs"
    assert qsort_rs.exists()

    yield qsort_rs

    qsort_rs.unlink(missing_ok=True)
