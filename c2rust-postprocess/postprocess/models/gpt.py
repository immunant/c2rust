import inspect
import json
from collections.abc import Callable, Iterable
from typing import Any, Protocol, cast

from openai import OpenAI
from openai.types.responses import (
    FunctionToolParam,
    ResponseFunctionToolCall,
    ResponseInputParam,
)
from openai.types.responses.response_input_param import FunctionCallOutput

from postprocess.models import AbstractGenerativeModel


class NamedCallable(Protocol):
    __name__: str

    def __call__(self, *args: Any, **kwargs: Any) -> Any: ...


class GPTModel(AbstractGenerativeModel):
    def __init__(
        self,
        id: str = "gpt-5.1",
        api_key: str | None = None,
        base_url: str | None = None,
    ):
        super().__init__(id)
        self.client = OpenAI(api_key=api_key, base_url=base_url)

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str:
        tools = [self._named_tool(tool) for tool in tools]
        tool_schemas = [self._tool_schema(tool) for tool in tools]
        tool_by_name = {tool.__name__: tool for tool in tools}

        if tool_schemas:
            response = self.client.responses.create(
                model=self.id,
                input=messages[0]["content"],
                max_tool_calls=max_tool_loops,
                tools=tool_schemas,
            )
        else:
            response = self.client.responses.create(
                model=self.id,
                input=messages[0]["content"],
                max_tool_calls=max_tool_loops,
            )

        for _ in range(max_tool_loops):
            tool_calls = [
                cast(ResponseFunctionToolCall, item)
                for item in response.output
                if getattr(item, "type", None) == "function_call"
            ]
            if not tool_calls:
                return response.output_text

            tool_outputs: ResponseInputParam = [
                FunctionCallOutput(
                    type="function_call_output",
                    call_id=tool_call.call_id,
                    output=self._call_tool(tool_call, tool_by_name),
                )
                for tool_call in tool_calls
            ]
            response = self.client.responses.create(
                model=self.id,
                input=tool_outputs,
                previous_response_id=response.id,
                max_tool_calls=max_tool_loops,
                tools=tool_schemas,
            )

        return response.output_text

    def _named_tool(self, tool: Callable[..., Any]) -> NamedCallable:
        if not hasattr(tool, "__name__"):
            raise TypeError(f"Tool must be a named function: {tool!r}")
        return cast(NamedCallable, tool)

    def _tool_schema(self, tool: NamedCallable) -> FunctionToolParam:
        signature = inspect.signature(tool)
        properties: dict[str, object] = {}
        required: list[str] = []
        for name, parameter in signature.parameters.items():
            properties[name] = {
                "type": self._json_schema_type(parameter.annotation),
            }
            if parameter.default is inspect.Parameter.empty:
                required.append(name)

        return {
            "type": "function",
            "name": tool.__name__,
            "description": inspect.getdoc(tool) or f"Call `{tool.__name__}`.",
            "parameters": {
                "type": "object",
                "properties": properties,
                "required": required,
                "additionalProperties": False,
            },
            "strict": False,
        }

    def _json_schema_type(self, annotation: Any) -> str:
        if annotation is bool:
            return "boolean"
        if annotation is int:
            return "integer"
        if annotation is float:
            return "number"
        return "string"

    def _call_tool(
        self,
        tool_call: ResponseFunctionToolCall,
        tool_by_name: dict[str, NamedCallable],
    ) -> str:
        if tool_call.name not in tool_by_name:
            raise ValueError(f"Unknown tool call: {tool_call.name}")

        arguments = json.loads(tool_call.arguments or "{}")
        if not isinstance(arguments, dict):
            raise ValueError(f"Tool call arguments must be an object: {arguments}")

        result = tool_by_name[tool_call.name](**arguments)
        return result if isinstance(result, str) else json.dumps(result)
