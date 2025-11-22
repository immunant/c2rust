import argparse
import logging
import sys
from collections.abc import Sequence

from postprocess import transfer_comments
from postprocess.cache import DirectoryCache
from postprocess.utils import existing_file


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Transfer C function comments to Rust using LLMs.",
    )
    parser.add_argument(
        "compile_commands",
        type=existing_file,
        help="Path to compile_commands.json.",
    )

    parser.add_argument(
        "--log-level",
        type=str,
        required=False,
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Logging level (default: INFO)",
    )

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    logging.basicConfig(level=logging.getLevelName(args.log_level.upper()))

    cache = DirectoryCache()

    transfer_comments(args.compile_commands, cache)

    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt as e:
        logging.warning("Interrupted by user, terminating...")
