from collections.abc import Callable, Iterable
from typing import Any

from postprocess.models import AbstractGenerativeModel


class MockGenerativeModel(AbstractGenerativeModel):
    """
    Mock generative model used when no API key is available.
    Generates no responses, so callers fall back to cached responses only.
    """

    def __init__(self):
        super().__init__(id="mock-llm")

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> None:
        return None
