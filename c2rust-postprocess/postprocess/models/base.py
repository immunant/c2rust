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
    async def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str | None:
        pass

    async def aclose(self) -> None:
        """Release any resources held by the model client."""
        return None
