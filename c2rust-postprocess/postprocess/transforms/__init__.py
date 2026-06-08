from postprocess.cache import AbstractCache
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.transforms.comments import CommentsTransform
from postprocess.transforms.variadic import FixVaListDriftTransform


def get_transform_by_id(
    id: str,
    cache: AbstractCache | None = None,
    model: AbstractGenerativeModel | None = None,
) -> AbstractTransform:
    """Factory function to get transform instance by ID."""
    # TODO: support named groups of transforms
    #       (e.g. "stable", "experimental", "all")?
    match id.lower():
        case "comments":
            assert cache is not None
            assert model is not None
            return CommentsTransform(cache=cache, model=model)
        case "fix-va-list-drift" | "variadic" | "va-list" | "valist":
            return FixVaListDriftTransform()
        case _:
            raise ValueError(f"Unsupported transform: {id}")
