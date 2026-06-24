import json
import os
from pathlib import Path

from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.gemini import GoogleGenerativeModel
from postprocess.models.gpt import GPTModel


def api_key_from_env(model_id: str) -> str | None:
    """Retrieve API key from environment variable based on model ID."""
    if model_id.startswith("gemini"):
        return os.getenv("GEMINI_API_KEY")
    elif model_id.startswith("gpt"):
        return os.getenv("OPENAI_API_KEY")
    return None


def codex_login_api_key() -> str | None:
    """Return the Codex CLI session token from the local login state."""
    codex_home = Path(os.getenv("CODEX_HOME") or (Path.home() / ".codex"))
    auth_path = codex_home / "auth.json"

    if not auth_path.is_file():
        return None

    try:
        auth_data = json.loads(auth_path.read_text())
    except json.JSONDecodeError as error:
        raise RuntimeError(f"failed to parse {auth_path}: {error}") from error

    tokens = auth_data.get("tokens", {})
    api_key = tokens.get("access_token")
    if not isinstance(api_key, str) or not api_key:
        raise RuntimeError(
            "found Codex auth.json, but it does not contain a valid access token"
        )

    return api_key


def get_model_by_id(id: str, generation_config: dict | None) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if id.startswith("gemini"):
        return GoogleGenerativeModel(id=id, generation_config=generation_config)
    elif id.startswith("gpt"):
        return GPTModel(id=id)

    raise ValueError(f"Unsupported model identifier: {id}")
