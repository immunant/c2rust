"""LLM client implementations."""

import logging

from abc import ABC, abstractmethod
import os
from time import sleep
from typing import Callable, Optional

from agents import Agent, Runner, function_tool

from config import (
    COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
    GEMINI_PRO_THINKING_BUDGET,
    GEMINI_FLASH_THINKING_BUDGET,
    DEFAULT_TIMEOUT,
    RETRY_DELAY,
)

from cache import SQLiteCache

VALIDATE_FN_TYPE = Callable[[str], bool]
LOGGER = logging.getLogger(__name__)


class LLMClient(ABC):
    """Abstract base class for LLM clients."""

    def __init__(self, model: str):
        self._model = model

    @property
    def model(self) -> str:
        return self._model

    @abstractmethod
    def generate_response(
        self, prompt: str, validate_fn: Optional[VALIDATE_FN_TYPE] = None
    ) -> Optional[str]:
        """Generate a response from the LLM."""
        pass


class CachedLLMClient(LLMClient):
    """Decorator/wrapper that adds caching to another LLMClient."""

    def __init__(self, base: LLMClient, cache: SQLiteCache):
        super().__init__(model=base.model)
        self.base = base
        self.cache = cache

    def generate_response(
        self, prompt: str, validate_fn: Optional[VALIDATE_FN_TYPE] = None
    ) -> Optional[str]:
        # cache lookup
        cached = self.cache.get(prompt, self.base.model)
        if cached is not None:
            return cached

        LOGGER.info(f"Cache miss, sending query to {self.base.model}...")
        # prompt LLM on cache miss
        resp = self.base.generate_response(prompt, validate_fn)

        # cache its response if valid or no validate function provided
        if validate_fn is None or validate_fn(resp):
            LOGGER.info("Caching response...")
            self.cache.set(prompt, self.base.model, resp)
        elif validate_fn is not None:
            LOGGER.info("Response failed validation; not cached...")

        return resp


class GeminiClient(LLMClient):
    """Client for Google Gemini models."""

    def __init__(self, model: str = "gemini-2.5-flash"):
        super().__init__(model)
        self._client = None

    @property
    def client(self):
        """Lazy load the Gemini client."""
        if self._client is None:
            from google.genai import Client

            self._client = Client()
        return self._client

    def generate_response(
        self,
        prompt: str,
        validate_fn: Optional[VALIDATE_FN_TYPE] = None,
        timeout: int = DEFAULT_TIMEOUT,
    ) -> Optional[str]:
        """
        Call the Gemini LLM with the given input.
        """
        from google.genai import types, errors

        # Gemini 2.5 Flash has a smaller max thinking budget than Pro
        thinking_budget = (
            GEMINI_PRO_THINKING_BUDGET
            if self.model.endswith("pro")
            else GEMINI_FLASH_THINKING_BUDGET
        )

        config_kwargs = {
            "system_instruction": COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
            "thinking_config": types.ThinkingConfig(thinking_budget=thinking_budget),
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
                    return self.generate_response(
                        prompt, validate_fn, timeout - RETRY_DELAY
                    )
                else:
                    print(
                        "Timeout exceeded while waiting for model to no longer be overloaded."
                    )
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
        super().__init__(model)
        self._client = None

    @property
    def client(self):
        """Lazy load the OpenAI client."""
        if self._client is None:
            from openai import OpenAI

            self._client = OpenAI()
        return self._client

    def generate_response(
        self, prompt: str, validate_fn: Optional[VALIDATE_FN_TYPE] = None
    ) -> Optional[str]:
        """
        Call the OpenAI Model with the given input.
        """
        if validate_fn:
            return self._generate_response_with_validation(prompt, validate_fn)
        else:
            return self._generate_response_without_validation(prompt)

    def _generate_response_with_validation(
        self, prompt: str, validate_fn: Callable
    ) -> Optional[str]:
        """
        Generate a response and validate it using the provided function.
        """

        # Creating a FunctionTool here rather than using @function_tool decorator lets us
        # use a single type signature for the validate function for different LLM clients.
        validate_fn_tool = function_tool(
            func=validate_fn,
        )

        agent = Agent(
            name="OpenAI Code Migration Tool",
            model=self.model,
            instructions=COMMENT_TRANSFER_SYSTEM_INSTRUCTIONS,
            tools=[validate_fn_tool],
        )

        return Runner.run_sync(agent, prompt).final_output

    def _generate_response_without_validation(self, prompt: str) -> Optional[str]:
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


# Client for OpenRouter models
# TODO: merge this with OpenAIClient
class OpenRouterClient(LLMClient):
    """Client for OpenRouter models."""

    def __init__(self, model: str = "openrouter/horizon-beta"):
        super().__init__(model)
        self._client = None

    @property
    def client(self):
        """Lazy load the OpenAI client."""
        if self._client is None:
            from openai import OpenAI

            self._client = OpenAI(
                base_url="https://openrouter.ai/api/v1",
                api_key=os.getenv("OPENROUTER_API_KEY"),
            )
        return self._client

    def generate_response(
        self, prompt: str, validate_fn: Optional[VALIDATE_FN_TYPE] = None,
        retries: int = 10
    ) -> Optional[str]:
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
        content = completion.choices[0].message.content
        if content and validate_fn and not validate_fn(content) and retries > 0:
            LOGGER.warning("Response failed validation; retrying...")
            return self.generate_response(prompt, validate_fn, retries - 1)

        return content


def create_llm_client(args) -> LLMClient:
    """Factory function to create the appropriate LLM client."""
    model = args.model.lower()

    def _create_llm_client(model: str) -> LLMClient:
        if model.startswith(("gpt", "o3", "o4", "codex")):
            return OpenAIClient(model)
        elif model.startswith("gemini"):
            return GeminiClient(model)
        elif model.startswith(("openrouter/", "qwen/", "anthropic/claude")):
            return OpenRouterClient(model)
        else:
            raise ValueError(f"Unknown model: {model}")

    client = _create_llm_client(model)
    if args.cache:
        cache = SQLiteCache()
        return CachedLLMClient(client, cache)
    return client
