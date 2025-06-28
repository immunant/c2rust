"""LLM client implementations."""

import os
from abc import ABC, abstractmethod
from time import sleep
from typing import Callable, Optional

from agents import Agent, FunctionTool, Runner

from config import (
    COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
    GEMINI_PRO_THINKING_BUDGET,
    GEMINI_FLASH_THINKING_BUDGET,
    DEFAULT_TIMEOUT,
    RETRY_DELAY,
)


class LLMClient(ABC):
    """Abstract base class for LLM clients."""

    @abstractmethod
    def generate_response(self, prompt: str, validate_fn: Optional[Callable] = None) -> str:
        """Generate a response from the LLM."""
        pass


class GeminiClient(LLMClient):
    """Client for Google Gemini models."""

    def __init__(self, model: str = "gemini-2.5-flash"):
        self.model = model
        self._client = None

    @property
    def client(self):
        """Lazy load the Gemini client."""
        if self._client is None:
            from google.genai import Client
            self._client = Client()
        return self._client

    def generate_response(self, prompt: str, validate_fn: Optional[Callable | FunctionTool] = None, timeout: int = DEFAULT_TIMEOUT) -> str:
        """
        Call the Gemini LLM with the given input.
        """
        from google.genai import types, errors

        # Gemini 2.5 Flash has a smaller max thinking budget than Pro
        thinking_budget = GEMINI_PRO_THINKING_BUDGET if self.model.endswith("pro") else GEMINI_FLASH_THINKING_BUDGET

        config_kwargs = {
            "system_instruction": COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
            "thinking_config": types.ThinkingConfig(thinking_budget=thinking_budget),
            "temperature": 0.0,
        }

        if validate_fn:
            config_kwargs["tools"] = [validate_fn]

        try:
            response = self.client.models.generate_content(
                model=self.model,
                contents=prompt,
                config=types.GenerateContentConfig(**config_kwargs),
            )
            return response.text or ""
        except errors.ServerError as se:
            if se.code == 503:
                if timeout > 0:
                    print("Service overloaded. Retrying shortly...")
                    sleep(RETRY_DELAY)
                    return self.generate_response(prompt, validate_fn, timeout - RETRY_DELAY)
                else:
                    print("Timeout exceeded while waiting for model to no longer be overloaded.")
                    raise
            elif se.code == 429:
                print("Rate limit exceeded. Please try again later.")
                raise
            else:
                print(f"Unknown error calling Gemini: {se}")
                raise


class OpenAIClient(LLMClient):
    """Client for OpenAI models."""

    def __init__(self, model: str = "codex-mini-latest"):
        self.model = model
        self._client = None

    @property
    def client(self):
        """Lazy load the OpenAI client."""
        if self._client is None:
            from openai import OpenAI
            self._client = OpenAI()
        return self._client

    def generate_response(self, prompt: str, validate_fn: Optional[Callable | FunctionTool] = None) -> str:
        """
        Call the OpenAI Model with the given input.
        """
        match validate_fn:
            case FunctionTool():
                return self._generate_response_with_validation(prompt, validate_fn)
            case None:
                return self._generate_response_without_validation(prompt)
            case _:
                raise ValueError("validate_fn must be a FunctionTool or None")

    def _generate_response_with_validation(self, prompt: str, validate_fn: FunctionTool) -> str:
        """
        Generate a response and validate it using the provided function.
        """

        agent = Agent(
            name="Comment bot",
            model=self.model,
            instructions=COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
            tools=[validate_fn],
        )

        return Runner.run_sync(agent, prompt).final_output

    def _generate_response_without_validation(self, prompt: str) -> str:
        """Generate a response without a validation callback."""
        completion = self.client.chat.completions.create(
            model=self.model,
            messages=[
                {
                    "role": "system",
                    "content": COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
                },
                {"role": "user", "content": prompt},
            ],
        )
        assert completion.choices[0].message.content  # appease type checker
        return completion.choices[0].message.content


def create_llm_client(model: str) -> LLMClient:
    """Factory function to create the appropriate LLM client."""
    if model.startswith(("gpt", "o3", "o4", "codex")):
        return OpenAIClient(model)
    elif model.startswith("gemini"):
        return GeminiClient(model)
    else:
        raise ValueError(f"Unknown model: {model}")
