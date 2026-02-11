"""
c2rust-postprocess: Transfer comments from C functions to Rust functions using LLMs.
"""

import argparse
import logging
from argparse import BooleanOptionalAction
from collections.abc import Sequence
from pathlib import Path

from google.genai import types

from postprocess.cache import DirectoryCache, FrozenCache
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.models import api_key_from_env, get_model_by_id
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms import get_transform_by_id
from postprocess.transforms.comments import (
    SYSTEM_INSTRUCTION,
    AbstractGenerativeModel,
)
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
        help="Regular expression to filter function identifiers to process"
        " (see README for more info)",
    )

    parser.add_argument(
        "--exclude-file",
        type=Path,
        required=False,
        default=None,
        help="A YAML file of file paths and identifiers within them to exclude/skip"
        " (see README for more info)",
    )

    parser.add_argument(
        "--model-id",
        type=str,
        required=False,
        default="gemini-3-flash-preview",
        help="ID of the LLM model to use (default: gemini-3-pro-preview)",
    )

    parser.add_argument(
        "--update-cache",
        required=False,
        default=True,
        action=argparse.BooleanOptionalAction,
        help="Enable or disable caching of LLM responses (default: enabled)",
    )

    parser.add_argument(
        "--cache-scope",
        type=str,
        required=False,
        default="repo",  # TODO: change default to user
        choices=["repo", "user", "system"],
        help="Scope for cache location (default: repo)",
    )

    parser.add_argument(
        "--update-rust",
        required=False,
        default=True,
        action=BooleanOptionalAction,
        help="Update the Rust in-place",
    )

    parser.add_argument(
        "--transform",
        type=str,
        required=False,
        action="append",
        default=["asserts", "comments"],
        help=(
            "Transform to apply; pass multiple times to apply multiple transforms "
            "in sorted order (default: comments)"
        ),
    )

    # TODO: add option to select model
    # TODO: add option to configure cache

    return parser


def get_model(model_id: str) -> AbstractGenerativeModel:
    api_key = api_key_from_env(model_id)
    if api_key is None:
        logging.warning(
            f"API key for model {model_id} not found in env; "
            "using cached responses only."
        )
        return MockGenerativeModel()

    # TODO: remove google specific API bits
    return get_model_by_id(
        model_id,
        generation_config={
            "system_instruction": types.Content(
                role="system", parts=[types.Part.from_text(text=SYSTEM_INSTRUCTION)]
            )
        },
    )


def main(argv: Sequence[str] | None = None):
    try:
        parser = build_arg_parser()
        args = parser.parse_args(argv)

        logging.basicConfig(level=logging.getLevelName(args.log_level.upper()))

        cache = getattr(DirectoryCache, args.cache_scope)()
        if not args.update_cache:
            cache = FrozenCache(cache)

        model = get_model(args.model_id)

        # sort transform IDs to transforms always run in the same order to
        # maximize cache hits even if the user passed them in a different order
        transform_ids = sorted(
            transform_id.strip()
            for transform_id in set(args.transform)
            if transform_id.strip()
        )

        transforms = [
            get_transform_by_id(transform_id, cache=cache, model=model)
            for transform_id in transform_ids
        ]

        for transform in transforms:
            transform.apply_dir(
                root_rust_source_file=args.root_rust_source_file,
                exclude_list=IdentifierExcludeList(src_path=args.exclude_file),
                ident_filter=args.ident_filter,
                update_rust=args.update_rust,
            )

        return 0
    except KeyboardInterrupt:
        logging.warning("Interrupted by user, terminating...")
        return 130  # 128 + SIGINT(2)
