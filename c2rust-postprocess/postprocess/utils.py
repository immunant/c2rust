import argparse
import json
import os
from pathlib import Path
from shutil import which
from typing import Any

from pygments import highlight
from pygments.formatters.terminal import TerminalFormatter
from pygments.lexer import RegexLexer
from pygments.lexers.c_cpp import CLexer
from pygments.lexers.rust import RustLexer
from requests.exceptions import JSONDecodeError


def check_isinstance[T](value: object, typ: type[T]) -> T:
    if not isinstance(value, typ):
        raise TypeError(f"Expected {typ}, got {type(value)}")
    return value


def get_tool_path(tool_name: str) -> Path:
    """Get the path to the given tool in the system PATH."""

    tool_path = which(tool_name)
    if tool_path is None:
        root = Path(__file__).resolve().parents[2]
        tool_path = root / f"tools/{tool_name}/target/release/{tool_name}"
        if not tool_path.exists():
            raise FileNotFoundError(
                f"{tool_path} not in path; nor in any expected location; "
                "did you built it?"
            )
    else:
        tool_path = Path(tool_path)

    # check that tool_path is executable
    if not os.access(tool_path, os.X_OK):
        raise PermissionError(f"{tool_path} is not executable")

    return tool_path


def existing_file(value: str) -> Path:
    path = Path(value)
    if path.is_file():
        return path
    raise argparse.ArgumentTypeError(f"{value!r} is not a readable file")


# TODO: test
def get_compile_commands(compile_commands_path: Path) -> list[dict[str, Any]]:
    try:
        compile_commands = json.loads(compile_commands_path.read_text())
    except JSONDecodeError as exc:
        raise RuntimeError(
            f"Failed to parse JSON from {compile_commands_path}: {exc}"
        ) from exc
    except OSError as exc:
        raise RuntimeError(
            f"Failed to read compile commands from {compile_commands_path}: {exc}"
        ) from exc

    if not isinstance(compile_commands, list):
        raise ValueError(
            f"Expected compile commands to be a list, got {type(compile_commands)}"
        )

    return compile_commands


def get_rust_files(path: Path) -> list[Path]:
    # `Path.glob` doesn't error if the path doesn't exist.
    if not path.exists():
        raise FileNotFoundError(f"{path} does not exist")
    if not path.is_dir():
        raise NotADirectoryError(f"{path} is not a directory")
    return list(path.glob("**/*.rs"))


# TODO: test
def read_chunk(filepath: Path, start_offset: int, end_offset: int, encoding="utf-8"):
    if start_offset < 0 or end_offset < start_offset:
        raise ValueError(f"Invalid range: {start_offset}–{end_offset}")

    length = end_offset - start_offset + 1  # inclusive range

    with open(filepath, "rb") as f:  # Only byte mode supports seeking to byte offset
        f.seek(start_offset)
        return f.read(length).decode(encoding)


# TODO: test
def remove_backticks(text: str) -> str:
    """Remove surrounding backticks from a model response."""
    text = text.strip()
    if text.startswith("```") and text.endswith("```"):
        return "\n".join(text.split("\n")[1:-1])

    if text.startswith("`"):
        text = text[1:]
    if text.endswith("`"):
        text = text[:-1]

    return text


def get_highlighted_c(c_code: str, bg="dark") -> str:
    return get_highlighted_code(c_code, CLexer(), bg=bg)


def get_highlighted_rust(rust_code: str, bg="dark") -> str:
    return get_highlighted_code(rust_code, RustLexer(), bg=bg)


def get_highlighted_code(code: str, lexer: RegexLexer, bg: str) -> str:
    # TODO: detect when terminal supports colors
    return highlight(code, lexer, TerminalFormatter(bg=bg))
