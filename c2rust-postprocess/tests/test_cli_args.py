import pytest

from postprocess import build_arg_parser


@pytest.fixture
def rust_file(tmp_path):
    """The parser validates root_rust_source_file with existing_file(),
    so tests must pass a path to a real file."""
    p = tmp_path / "main.rs"
    p.write_text("fn main() {}\n")
    return str(p)


def test_help_does_not_crash(capsys, rust_file):
    with pytest.raises(SystemExit) as e:
        build_arg_parser().parse_args([rust_file, "--help"])
    assert e.value.code == 0
    out = capsys.readouterr().out
    assert "--update-rust" in out
    assert "--no-update-rust" in out


def test_update_rust_defaults_to_true(rust_file):
    assert build_arg_parser().parse_args([rust_file]).update_rust is True


def test_update_rust_flags_parse(rust_file):
    p = build_arg_parser()
    assert p.parse_args([rust_file, "--update-rust"]).update_rust is True
    assert p.parse_args([rust_file, "--no-update-rust"]).update_rust is False
