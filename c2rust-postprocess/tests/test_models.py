import asyncio
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
from google.genai import types

from postprocess.models import gemini, gpt


def test_gpt_awaits_response_and_closes_client(monkeypatch) -> None:
    create = AsyncMock(return_value=SimpleNamespace(output_text="rewritten function"))
    close = AsyncMock()
    client = SimpleNamespace(responses=SimpleNamespace(create=create), close=close)
    constructor = Mock(return_value=client)
    monkeypatch.setattr(gpt, "AsyncOpenAI", constructor)

    async def run() -> str:
        model = gpt.GPTModel(
            id="test/model", api_key="test-key", base_url="https://example.test/v1"
        )
        try:
            return await model.generate_with_tools(
                [{"role": "user", "content": "function prompt"}], max_tool_loops=2
            )
        finally:
            await model.aclose()

    assert asyncio.run(run()) == "rewritten function"
    constructor.assert_called_once_with(
        api_key="test-key", base_url="https://example.test/v1"
    )
    create.assert_awaited_once_with(
        model="test/model", input="function prompt", max_tool_calls=2
    )
    close.assert_awaited_once_with()


@pytest.mark.parametrize("response_text", ["rewritten function", None])
def test_gemini_awaits_response_and_closes_both_clients(
    monkeypatch, response_text: str | None
) -> None:
    generate = AsyncMock(return_value=SimpleNamespace(text=response_text))
    async_close = AsyncMock()
    sync_close = Mock()
    client = SimpleNamespace(
        aio=SimpleNamespace(
            models=SimpleNamespace(generate_content=generate), aclose=async_close
        ),
        close=sync_close,
    )
    constructor = Mock(return_value=client)
    monkeypatch.setattr(gemini.genai, "Client", constructor)

    def validate(response: str) -> str:
        return response

    async def run() -> str | None:
        model = gemini.GoogleGenerativeModel(id="gemini-test", api_key="test-key")
        try:
            return await model.generate_with_tools(
                [
                    {"role": "user", "content": "function prompt"},
                    {"role": "assistant", "content": "previous response"},
                ],
                tools=[validate],
                max_tool_loops=2,
            )
        finally:
            await model.aclose()

    assert asyncio.run(run()) == response_text
    constructor.assert_called_once_with(api_key="test-key")
    generate.assert_awaited_once_with(
        model="gemini-test",
        contents=[
            types.Content(
                role="user", parts=[types.Part.from_text(text="function prompt")]
            ),
            types.Content(
                role="model", parts=[types.Part.from_text(text="previous response")]
            ),
        ],
        config=types.GenerateContentConfig(
            tools=[validate],
            automatic_function_calling=types.AutomaticFunctionCallingConfig(
                disable=False, maximum_remote_calls=2
            ),
        ),
    )
    async_close.assert_awaited_once_with()
    sync_close.assert_called_once_with()
