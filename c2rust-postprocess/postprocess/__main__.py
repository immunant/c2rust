import argparse
import logging
import sys
from collections.abc import Sequence

from postprocess.cache import DirectoryCache, FrozenCache
from postprocess.models import get_model_by_id
from postprocess.transforms import SYSTEM_INSTRUCTION, CommentTransfer
from postprocess.utils import existing_file


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Postprocess c2rust transpiler output using LLMs",
    )

    parser.add_argument(
        "root_rust_source_file",
        type=existing_file,
        help="Path to Rust source file referenced by Cargo.toml",
    )

    parser.add_argument(
        "--log-level",
        type=str,
        required=False,
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Logging level (default: INFO)",
    )

    parser.add_argument(
        "--ident-filter",
        type=str,
        required=False,
        default=None,
        help="Regular expression to filter function identifiers to process",
    )

    parser.add_argument(
        "--model-id",
        type=str,
        required=False,
        default="gemini-3-pro-preview",
        help="ID of the LLM model to use (default: gemini-3-pro-preview)",
    )

    parser.add_argument(
        "--update-cache",
        type=bool,
        required=False,
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Enable or disable caching of LLM responses (default: enabled)",
    )

    # TODO: add option to select model
    # TODO: add option to configure cache
    # TODO: add option to select what transforms to apply

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    try:
        parser = build_arg_parser()
        args = parser.parse_args(argv)

        logging.basicConfig(level=logging.getLevelName(args.log_level.upper()))

        cache = DirectoryCache()
        if not args.update_cache:
            cache = FrozenCache(cache)

        from google.genai import types
        model = get_model_by_id(
                args.model_id,
                generation_config = {"system_instruction": types.Content(
                    role="system",
                    parts=[types.Part.from_text(text=SYSTEM_INSTRUCTION)]
                )}
        )

        # TODO: instantiate transform(s) based on command line args
        xform = CommentTransfer(cache, model)
        xform.transfer_comments(args.root_rust_source_file, args.ident_filter)

        return 0
    except KeyboardInterrupt:
            logging.warning("Interrupted by user, terminating...")
            sys.exit(130)  # 128 + SIGINT(2)


if __name__ == "__main__":
        sys.exit(main())

