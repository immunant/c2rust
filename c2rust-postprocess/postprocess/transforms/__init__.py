from postprocess.cache import AbstractCache
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.asserts import AssertsTransform
from postprocess.transforms.base import AbstractTransform
from postprocess.transforms.comments import CommentsTransform


def get_transform_by_id(
    id: str, cache: AbstractCache, model: AbstractGenerativeModel
) -> AbstractTransform:
    """Factory function to get transform instance by ID."""
    # TODO: support named groups of transforms
    #       (e.g. "stable", "experimental", "all")?
    match id.lower():
        case "comments":
            return CommentsTransform(cache=cache, model=model)
        case "asserts":
            return AssertsTransform(cache=cache, model=model)
        case _:
            raise ValueError(f"Unsupported transform: {id}")
