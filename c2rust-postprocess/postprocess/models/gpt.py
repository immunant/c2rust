from collections.abc import Callable, Iterable
from typing import Any

from openai import AsyncOpenAI

from postprocess.models import AbstractGenerativeModel


class GPTModel(AbstractGenerativeModel):
    def __init__(
        self,
        id: str = "gpt-5.1",
        api_key: str | None = None,
        base_url: str | None = None,
    ):
        super().__init__(id)
        self.client = AsyncOpenAI(api_key=api_key, base_url=base_url)

    async def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str:
        # TODO: implement tool calling support
        assert not tools, "Tool calling not yet implemented for GPTModel"

        response = await self.client.responses.create(
            model=self.id,
            input=messages[0]["content"],
            max_tool_calls=max_tool_loops,
        )

        return response.output_text

    async def aclose(self) -> None:
        await self.client.close()
