from collections.abc import Callable
from typing import Any

from openai import OpenAI

from postprocess.models import AbstractGenerativeModel


class GPTModel(AbstractGenerativeModel):

    def __init__(
        self,
        id: str = "gpt-5.1",
        api_key: str | None = None,
        **kwargs: Any
    ):
        super().__init__(id, **kwargs)
        self.client = OpenAI(api_key=api_key)
        self.kwargs = kwargs

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: list[Callable] | None = None,
        max_tool_loops: int = 5
    ) -> Any:

        # TODO: implement tool calling support
        assert tools is None, "Tool calling not yet implemented for GPTModel"

        response = self.client.responses.create(
            model=self.id,
            input=messages[0]["content"],
            max_tool_calls=max_tool_loops,
        )

        return response.output_text
