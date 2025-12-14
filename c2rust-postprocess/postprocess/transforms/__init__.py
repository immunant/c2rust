from typing import Any

from postprocess.transforms.base import AbstractTransform
from postprocess.transforms.comments import CommentsTransform


def get_transform_by_id(id: str, **kwargs: Any) -> AbstractTransform:
    """Factory function to get model instance by ID."""
    # TODO: support named groups of transforms
    #       (e.g. "stable", "experimental", "all")?
    match id.lower():
        case "comments":
            return CommentsTransform(**kwargs)
        case _:
            raise ValueError(f"Unsupported model identifier: {id}")
