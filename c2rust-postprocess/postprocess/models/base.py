from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable
from typing import Any


class AbstractGenerativeModel(ABC):
    """
    Abstract base class for LLM clients using Native Function Calling.
    """

    def __init__(self, id: str):
        self._id = id

    @property
    def id(self) -> str:
        return self._id

    @abstractmethod
    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str | None:
        pass
