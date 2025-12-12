from typing import Any

from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.gemini import GoogleGenerativeModel
from postprocess.models.gpt import GPTModel


def get_model_by_id(id: str, **kwargs: Any) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if id.startswith("gemini"):
        return GoogleGenerativeModel(id=id, **kwargs)
    elif id.startswith("gpt"):
        return GPTModel(id=id, **kwargs)

    raise ValueError(f"Unsupported model identifier: {id}")
