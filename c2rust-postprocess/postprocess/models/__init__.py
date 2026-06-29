import os
import shutil
from pathlib import Path

from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.codex import CodexModel
from postprocess.models.gemini import GoogleGenerativeModel
from postprocess.models.gpt import GPTModel


def codex_model_name(model_id: str) -> str:
    """Resolve the codex model from a `codex` / `codex:<model>` identifier."""
    return model_id.split(":", 1)[1] if ":" in model_id else "gpt-5.1-codex"


def api_key_from_env(model_id: str) -> str | None:
    """Retrieve API key from environment variable based on model ID."""
    if model_id.startswith("gemini"):
        return os.getenv("GEMINI_API_KEY")
    elif model_id.startswith("gpt"):
        return os.getenv("OPENAI_API_KEY")
    return None


def codex_available() -> bool:
    """Whether the `codex` CLI is installed and has cached credentials.

    `codex exec` reuses the auth saved by `codex login` (ChatGPT subscription
    or API key), so availability is the binary plus `~/.codex/auth.json`
    rather than an environment variable.
    """
    if shutil.which("codex") is None:
        return False
    return (Path.home() / ".codex" / "auth.json").exists()


def is_model_available(model_id: str) -> bool:
    """Whether the backend for `model_id` can actually generate responses.

    Used to decide between a real model and the cache-only `MockGenerativeModel`
    fallback. Codex authenticates via the CLI, not an env var, so it gets its
    own check.
    """
    if model_id.startswith("codex"):
        return codex_available()
    return api_key_from_env(model_id) is not None


def get_model_by_id(id: str, generation_config: dict | None) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if id.startswith("gemini"):
        return GoogleGenerativeModel(id=id, generation_config=generation_config)
    elif id.startswith("codex"):
        return CodexModel(id=id, codex_model=codex_model_name(id))
    elif id.startswith("gpt"):
        return GPTModel(id=id)

    raise ValueError(f"Unsupported model identifier: {id}")
