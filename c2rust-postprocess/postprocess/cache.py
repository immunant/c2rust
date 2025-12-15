import json
import logging
from abc import ABC, abstractmethod
from pathlib import Path
from tempfile import gettempdir
from typing import Any, Self, Union

import tomli
import tomlkit
from platformdirs import user_cache_dir
from tomlkit.items import String


class AbstractCache(ABC):
    """
    Abstract base class for caching of LLM interactions.
    """

    def __init__(self, path: Path):
        self._path = path

    @property
    def path(self) -> Path:
        return self._path

    @abstractmethod
    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        """Lookup a cached response for the given messages.

        Args:
            messages: The list of messages representing the conversation history.
        """
        pass

    @abstractmethod
    def update(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
        response: str,
    ) -> None:
        """Store a response in the cache for the given messages.

        Args:
            messages: The list of messages representing the conversation history.
            response: The response text to cache.
            model: Optional model identifier.
            identifier: Optional identifier for the cache entry.
            transform: Optional transform type for the cache entry.
        """
        pass

    @abstractmethod
    def clear(self) -> None:
        """Clear the entire cache."""
        pass

    def flush(self) -> None:  # noqa: B027
        """
        Optional: Persist cache to disk.
        Not abstract because not all implementations need it.
        """
        pass


TomlValue = Union[None, str, int, float, bool, "TomlList", "TomlDict"]
TomlList = list[TomlValue]
TomlDict = dict[str, TomlValue]


def to_multiline_toml(value: TomlDict) -> str:
    """
    Convert a `TomlDict` to a multiline toml str.
    Any multiline value becomes a multiline string.
    `tomli-w` and `tomlkit` don't do this natively.
    `None` values are also skipped.
    """

    def convert_value(value: TomlValue) -> TomlValue | String:
        if isinstance(value, str) and "\n" in value:
            return tomlkit.string(value, multiline=True)
        elif isinstance(value, dict):
            return {k: convert_value(v) for k, v in value.items() if v is not None}
        elif isinstance(value, list):
            return [convert_value(e) for e in value if e is not None]
        else:
            assert value is not None
            return value

    converted_value = convert_value(value)
    assert isinstance(converted_value, dict)
    doc = tomlkit.document()
    for k, v in converted_value.items():
        doc[k] = v

    return tomlkit.dumps(doc)


class DirectoryCache(AbstractCache):
    """
    Cache that stores cached responses in a directory.
    If no path is specified, a temporary directory is used.
    """

    def __init__(self, path: Path):
        super().__init__(path)
        self._path.mkdir(parents=True, exist_ok=True)

        logging.debug(f"Using cache directory: {self._path}")

    @classmethod
    def system(cls) -> Self:
        """
        Use the system temporary cache.
        """
        path = Path(gettempdir()) / "c2rust-postprocess"
        return cls(path=path)

    @classmethod
    def user(cls) -> Self:
        """
        Use the user's cache.
        """
        path = Path(user_cache_dir(appname="c2rust-postprocess"))
        return cls(path=path)

    @classmethod
    def repo(cls) -> Self:
        """
        Use a cache that is checked into the git repo.
        This is intended to be used by CI.
        """
        path = Path(__file__).parent / "../tests/llm-cache"
        return cls(path=path)

    def get_message_digest(self, messages: list[dict[str, Any]]) -> str:
        import hashlib

        messages_str = json.dumps(messages, sort_keys=True)
        return hashlib.sha256(messages_str.encode()).hexdigest()

    def cache_dir(
        self,
        *,
        transform: str,
        identifier: str,
        messages: list[dict[str, Any]],
    ) -> Path:
        message_digest = self.get_message_digest(messages)
        return self._path / transform / identifier / message_digest

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        cache_dir = self.cache_dir(
            transform=transform, identifier=identifier, messages=messages
        )
        cache_file = cache_dir / "metadata.toml"
        try:
            toml = cache_file.read_text()
        except FileNotFoundError:
            logging.debug(f"Cache miss: {cache_file}: {messages}")
            return None
        logging.debug(f"Cache hit: {cache_file}")
        data = tomli.loads(toml)

        return data["response"]

    def update(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
        response: str,
    ) -> None:
        data = {
            "transform": transform,
            "identifier": identifier,
            "model": model,
            "messages": messages,
            "response": response,
        }
        toml = to_multiline_toml(data)

        cache_dir = self.cache_dir(
            transform=transform, identifier=identifier, messages=messages
        )
        cache_dir.mkdir(parents=True, exist_ok=True)
        metadata_path = cache_dir / "metadata.toml"
        response_path = cache_dir / "response.txt"
        metadata_path.write_text(toml)
        response_path.write_text(response)
        logging.debug(f"Cache updated: {cache_dir}")

    def clear(self) -> None:
        self._path.unlink(missing_ok=True)


class FrozenCache(AbstractCache):
    """
    Cache that does not allow updates of an inner cache.
    """

    def __init__(self, inner_cache: AbstractCache):
        super().__init__(Path("/dev/null"))
        self._inner_cache = inner_cache

    @property
    def inner_cache(self) -> AbstractCache:
        return self._inner_cache

    def lookup(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
    ) -> str | None:
        return self.inner_cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        )

    def update(
        self,
        *,
        transform: str,
        identifier: str,
        model: str,
        messages: list[dict[str, Any]],
        response: str,
    ) -> None:
        pass

    def clear(self) -> None:
        pass
