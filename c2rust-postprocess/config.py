"""Configuration constants for c2rust-llm-assist."""

COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS = """
You are a code migration assistant that transfers comments from a C function to a corresponding Rust function.
Carefully read all comments from the C function and transfer them to the matching Rust function.
ONLY USE C++ style comments (`//`) when translating comments to Rust. Do not use doc comments (`///`) or block comments (`/*...*/`).
The comments should be inserted in the same order and at the same locations as they appear in the C function.
The output Rust function must be syntactically correct and identical to the original Rust function other than the added comments.
DO NOT add comments that are not present in the C function.
Do not return the C function. Do not produce a summary of changes. Do not put triple backticks around the code block or use any other markup.
Call the function `validate_rust_function_with_comments` to validate whether comments were inserted correctly. Return the Rust function if `validate_rust_function_with_comments` returns True.
ONLY return the Rust function with comments inserted! Preserve (don't change) Rust code that is not related to comments.
"""

# Default models
DEFAULT_GEMINI_MODEL = "gemini-2.5-flash"
DEFAULT_OPENAI_MODEL = "codex-mini-latest"

# Gemini thinking budgets
GEMINI_PRO_THINKING_BUDGET = 32768
GEMINI_FLASH_THINKING_BUDGET = 24576

# Retry settings
DEFAULT_TIMEOUT = 60
RETRY_DELAY = 10
