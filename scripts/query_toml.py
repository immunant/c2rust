#!/usr/bin/env -S uv run

from argparse import ArgumentParser
from pathlib import Path
from typing import Any, Iterable
import toml


def query_toml(path: Path, query: Iterable[str]) -> Any:
    result = toml.load(path)
    for field in query:
        if isinstance(result, list):
            field = int(field)
        result = result[field]
    return result


def main() -> None:
    parser = ArgumentParser()
    parser.add_argument("query", type=str, help="the TOML query, with fields separated by .")
    parser.add_argument("path", type=Path, help="the path to a TOML file")
    args = parser.parse_args()
    print(query_toml(path=args.path, query=args.query.split(".")))


if __name__ == "__main__":
    main()
