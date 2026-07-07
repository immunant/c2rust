import os

from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.gemini import GoogleGenerativeModel
from postprocess.models.gpt import GPTModel

OPENROUTER_BASE_URL = "https://openrouter.ai/api/v1"


def api_key_from_env(model_id: str) -> str | None:
    """Retrieve API key from environment variable based on model ID."""
    if model_id.startswith("gemini"):
        return os.getenv("GEMINI_API_KEY")
    elif model_id.startswith("gpt"):
        return os.getenv("OPENAI_API_KEY")
    # OpenRouter ids are namespaced, e.g. "anthropic/claude-sonnet-4.5"
    elif "/" in model_id:
        return os.getenv("OPENROUTER_API_KEY")
    return None


def get_model_by_id(id: str) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if id.startswith("gemini"):
        return GoogleGenerativeModel(id=id)
    elif id.startswith("gpt"):
        return GPTModel(id=id)
    elif "/" in id:
        return GPTModel(
            id=id,
            api_key=os.getenv("OPENROUTER_API_KEY"),
            base_url=OPENROUTER_BASE_URL,
        )

    raise ValueError(f"Unsupported model identifier: {id}")
