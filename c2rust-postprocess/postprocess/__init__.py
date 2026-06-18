"""
c2rust-postprocess: Transfer comments from C functions to Rust functions using LLMs.
"""

import argparse
import logging
import os
from argparse import BooleanOptionalAction
from collections.abc import Sequence
from pathlib import Path

from google.genai import types

from postprocess.cache import DirectoryCache, FrozenCache
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.models import api_key_from_env, get_model_by_id
from postprocess.models.gpt import GPTModel
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms import get_transform_by_id
from postprocess.transforms.base import TransformError
from postprocess.transforms.comments import (
    SYSTEM_INSTRUCTION,
    AbstractGenerativeModel,
)
from postprocess.utils import existing_file

DEFAULT_LLM_MODEL = "gemini-3.5-flash"


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
        "--llm-model",
        type=str,
        required=False,
        default=DEFAULT_LLM_MODEL,
        help=f"ID of the LLM model to use (default: {DEFAULT_LLM_MODEL})",
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
        "--on-error",
        type=str,
        required=False,
        default="keep-going",
        choices=["abort", "keep-going", "warn"],
        help="Handle per-function transform failures: abort at first failure,"
        " keep going with exit 1, or warn and exit 0 (default: keep-going)",
    )

    parser.add_argument(
        "--transform",
        type=str,
        required=False,
        action="append",
        default=["comments"],
        help=(
            "Transform to apply; pass multiple times to apply multiple transforms "
            "in sorted order (default: comments)"
        ),
    )

    # TODO: add option to select model
    # TODO: add option to configure cache

    return parser


def get_model(model_id: str) -> AbstractGenerativeModel:
    # CRISP_API_MODEL/_KEY/_BASE (see github.com/GaloisInc/Tractor-Crisp)
    # override CLI model selection; CRISP endpoints are OpenAI-compatible.
    if crisp_model_id := os.getenv("CRISP_API_MODEL"):
        if not (crisp_api_key := os.getenv("CRISP_API_KEY")):
            raise RuntimeError(
                "`CRISP_API_MODEL` is set but `CRISP_API_KEY` is not; "
                "set both or neither."
            )
        logging.info(
            "CLI model selection overridden by `CRISP_API_MODEL` env var; "
            f"using model {crisp_model_id}."
        )
        return GPTModel(
            id=crisp_model_id,
            api_key=crisp_api_key,
            base_url=os.getenv("CRISP_API_BASE"),
        )

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

        model = get_model(args.llm_model)

        # sort transform IDs to transforms always run in the same order to
        # maximize cache hits even if the user passed them in a different order
        transform_ids = sorted(
            transform_id.strip()
            for transform_id in set(args.transform)
            if transform_id.strip()
        )
        transforms = [
            get_transform_by_id(
                transform_id,
                cache=cache,
                model=model,
            )
            for transform_id in transform_ids
        ]

        failures = 0
        failure_log_level = (
            logging.WARNING if args.on_error == "warn" else logging.ERROR
        )
        for transform in transforms:
            (
                transform_rewrites,
                transform_cache_hits,
                transform_skips,
                transform_failures,
            ) = transform.apply_dir(
                root_rust_source_file=args.root_rust_source_file,
                exclude_list=IdentifierExcludeList(src_path=args.exclude_file),
                ident_filter=args.ident_filter,
                update_rust=args.update_rust,
                keep_going=args.on_error != "abort",
                failure_log_level=failure_log_level,
            )
            transform_name = transform.__class__.__name__.removesuffix(
                "Transform"
            ).lower()
            if any(
                (
                    transform_rewrites,
                    transform_cache_hits,
                    transform_skips,
                    transform_failures,
                )
            ):
                logging.info(
                    f"{transform_name} transform stats: "
                    f"rewrites {transform_rewrites}, "
                    f"cache hits {transform_cache_hits}, "
                    f"skips {transform_skips}, "
                    f"fails {transform_failures}"
                )
            failures += transform_failures

        if failures and args.on_error != "warn":
            return 1

        return 0
    except TransformError as error:
        logging.exception(f"Aborting at first transform failure: {error}")
        return 1
    except KeyboardInterrupt:
        logging.warning("Interrupted by user, terminating...")
        return 130  # 128 + SIGINT(2)
