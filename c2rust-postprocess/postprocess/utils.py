import argparse
from pathlib import Path
from typing import Any


def existing_file(value: str) -> Path:
    path = Path(value)
    if path.is_file():
        return path
    raise argparse.ArgumentTypeError(f"{value!r} is not a readable file")


# TODO: test
def get_compile_commands(compile_commands_path: Path) -> list[dict[str, Any]]:
    import json

    try:
        with open(compile_commands_path, encoding="utf-8") as f:
            compile_commands = json.load(f)
    except OSError as exc:
        raise RuntimeError(
            f"Failed to read compile commands from {compile_commands_path}: {exc}"
        ) from exc
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"Failed to parse JSON from {compile_commands_path}: {exc}"
        ) from exc

    if not isinstance(compile_commands, list):
        raise ValueError(
            f"Expected compile commands to be a list, got {type(compile_commands)}"
        )

    return compile_commands


def get_rust_files(path: Path) -> list[Path]:
    rust_files = []

    if not path.exists():
        raise FileNotFoundError(f"{path} does not exist")
    if not path.is_dir():
        raise NotADirectoryError(f"{path} is not a directory")

    for file in path.glob("**/*.rs"):
        rust_files.append(file)
    return rust_files


# TODO: test
def read_chunk(filepath: Path, start_offset: int, end_offset: int, encoding='utf-8'):
    if start_offset < 0 or end_offset < start_offset:
        raise ValueError(f"Invalid range: {start_offset}–{end_offset}")

    length = end_offset - start_offset + 1 # inclusive range

    with open(filepath, 'rb') as f: # Only byte mode supports seeking to byte offset
        f.seek(start_offset)
        return f.read(length).decode(encoding)

# TODO: test
def remove_backticks(text: str) -> str:
    """Remove surrounding backticks from a model response."""
    text = text.strip()
    if text.startswith("```") and text.endswith("```"):
        return "\n".join(text.split("\n")[1:-1])
    elif text.startswith("`") and text.endswith("`"):
        return text[1:-1]
    return text