from collections.abc import Callable
from typing import Any

from postprocess.models import AbstractGenerativeModel


class MockGenerativeModel(AbstractGenerativeModel):
    """
    Mock generative model for testing without an actual LLM backend.
    Raises NotImplementedError on generation attempts.
    """

    def __init__(self, **kwargs: Any):
        super().__init__(id="mock-llm", **kwargs)

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: list[Callable] | None = None,
        max_tool_loops: int = 5,
    ) -> Any:
        raise NotImplementedError(
            "MockGenerativeModel, by design, does not generate responses.\n"
            "If this is unexpected, check if you forgot to add an API key "
            "to your environment (e.g. `export GEMINI_API_KEY=...`)."
        )
