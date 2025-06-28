#!/usr/bin/env -S uv --quiet run --script
"""
c2rust-llm-assist: Transfer comments from C functions to Rust functions using LLMs.
"""

import argparse
import os
import sys
from tools import CommentTransferTool
from config import DEFAULT_GEMINI_MODEL

def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Transfer comments from C functions to Rust functions using LLMs"
    )

    parser.add_argument(
        "-f",
        "--function-pattern",
        type=str,
        required=False,
        default=None,
        help="Only process functions matching this regex pattern",
    )

    parser.add_argument(
        "-F",
        "--file-pattern",
        type=str,
        required=False,
        default=None,
        help="Only process files matching this regex pattern (applied to the C file name)",
    )

    parser.add_argument(
        "-m",
        "--model",
        type=str,
        required=False,
        default=DEFAULT_GEMINI_MODEL,
        help=f"LLM model to use (default: {DEFAULT_GEMINI_MODEL})",
    )

    parser.add_argument(
        "-p",
        "--path",
        type=str,
        required=True,
        help="Path to compile_commands.json"
    )

    args = parser.parse_args()

    if not os.path.exists(args.path):
        print(f"Error: Path {args.path} does not exist.", file=sys.stderr)
        sys.exit(1)
    if not os.access(args.path, os.R_OK):
        print(f"Error: Path {args.path} is not readable.", file=sys.stderr)
        sys.exit(1)

    return args


def main():
    """Main entry point."""
    args = parse_args()

    try:
        tool = CommentTransferTool(args.model, args.path)
        tool.process_files(args.file_pattern, args.function_pattern)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
