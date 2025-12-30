from collections.abc import Callable, Iterable
from typing import Any

from google import genai
from google.genai import types

from postprocess.models.base import AbstractGenerativeModel


class GoogleGenerativeModel(AbstractGenerativeModel):
    """
    Concrete implementation using the modern 'google-genai' SDK.
    Enables native automatic function calling with loop limits.
    """

    def __init__(
        self,
        id: str = "gemini-2.5-flash",
        api_key: str | None = None,
        generation_config: dict[str, Any] | None = None,
    ):
        super().__init__(id)
        self.client = genai.Client(api_key=api_key)
        if generation_config is None:
            generation_config = {}
        self._generation_config = generation_config

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str | None:
        contents = self._convert_messages(messages)

        config = types.GenerateContentConfig(
            # SDK automatically generates schemas from python functions
            tools=list(tools),
            automatic_function_calling=types.AutomaticFunctionCallingConfig(
                disable=False,
                maximum_remote_calls=max_tool_loops,  # Enforces the loop limit natively
            ),
            **self._generation_config,
        )

        response = self.client.models.generate_content(
            model=self._id, contents=contents, config=config
        )

        return response.text

    def _convert_messages(self, messages: list[dict[str, Any]]) -> list[types.Content]:
        """
        Converts standard list of dicts to Google GenAI 'Content' objects.
        """
        gemini_contents = []

        for msg in messages:
            role = msg.get("role")
            content = msg.get("content")

            # Map standard roles to Gemini roles (user/model)
            if role == "assistant":
                gemini_role = "model"
            elif role == "system":
                raise ValueError(
                    "System messages are not supported in messages to Gemini models."
                )
            else:
                gemini_role = "user"

            gemini_contents.append(
                types.Content(
                    role=gemini_role, parts=[types.Part.from_text(text=str(content))]
                )
            )

        return gemini_contents
