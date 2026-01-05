from collections.abc import Callable, Iterable
from typing import Any, Never

from postprocess.models import AbstractGenerativeModel


class MockGenerativeModel(AbstractGenerativeModel):
    """
    Mock generative model for testing without an actual LLM backend.
    Raises NotImplementedError on generation attempts.
    """

    def __init__(self):
        super().__init__(id="mock-llm")

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> Never:
        raise NotImplementedError(
            "MockGenerativeModel, by design, does not generate responses.\n"
            "If this is unexpected, check if you forgot to add an API key "
            "to your environment (e.g. `export GEMINI_API_KEY=...`)."
        )
