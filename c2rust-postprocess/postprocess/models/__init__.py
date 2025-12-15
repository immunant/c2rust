import os

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


def get_model_by_id(id: str, generation_config: dict | None) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if id.startswith("gemini"):
        return GoogleGenerativeModel(id=id, generation_config=generation_config)
    elif id.startswith("gpt"):
        return GPTModel(id=id)

    raise ValueError(f"Unsupported model identifier: {id}")
