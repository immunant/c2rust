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

    # @abstractmethod
    # async def agenerate_with_tools(
    #     self,
    #     messages: list[dict[str, Any]],
    #     tools: list[Callable] | None = None,
    #     max_tool_loops: int = 5
    # ) -> Any:
    #     """
    #     Generate a response using native automatic function calling.

    #     Args:
    #         messages: Chat history.
    #         tools: List of Python functions.
    #         max_tool_loops: Maximum number of times the model can call tools
    #                         consecutively.

    #     Returns:
    #         The final natural language response from the model.
    #     """
    #     pass

    # def generate_with_tools(
    #     self,
    #     messages: list[dict[str, Any]],
    #     tools: Iterable[Callable[..., Any]] = (),
    #     max_tool_loops: int = 5
    # ) -> str | None:
    #     """Synchronous wrapper for agenerate_response."""
    #     return asyncio.run(self.agenerate_with_tools(messages, tools, max_tool_loops))

    @abstractmethod
    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str | None:
        pass
