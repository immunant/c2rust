from typing import Any

from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.gemini import GoogleGenerativeModel


def get_model_by_id(model_id: str, **kwargs: Any) -> AbstractGenerativeModel:
    """Factory function to get model instance by ID."""
    if model_id.startswith("gemini"):
        return GoogleGenerativeModel(model_id=model_id, **kwargs)

    raise ValueError(f"Unsupported model identifier: {model_id}")
