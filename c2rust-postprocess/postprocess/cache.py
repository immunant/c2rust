import json
import logging
from abc import ABC, abstractmethod
from pathlib import Path
from tempfile import gettempdir
from typing import Any, Self

from platformdirs import user_cache_dir


class AbstractCache(ABC):
    """
    Abstract base class for caching of LLM interactions.
    """

    def __init__(self, path: Path, **kwargs: Any):
        self._path = path
        self._config = kwargs

    @property
    def path(self) -> Path:
        return self._path

    @abstractmethod
    def lookup(
        self,
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
        messages: list[dict[str, Any]],
        response: str,
        model: str | None = None,
        identifier: str | None = None,
        transform: str | None = None,
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


class DirectoryCache(AbstractCache):
    """
    Cache that stores cached responses in a directory.
    If no path is specified, a temporary directory is used.
    """

    def __init__(self, path: Path, **kwargs: Any):
        super().__init__(path, **kwargs)
        self._path.mkdir(parents=True, exist_ok=True)

        logging.debug(f"Using cache directory: {self._path}")

    @classmethod
    def system(cls, **kwargs: Any) -> Self:
        """
        Use the system temporary cache.
        """
        path = Path(gettempdir()) / "c2rust-postprocess"
        return cls(path=path, **kwargs)

    @classmethod
    def user(cls, **kwargs: Any) -> Self:
        """
        Use the user's cache.
        """
        path = Path(user_cache_dir(appname="c2rust-postprocess"))
        return cls(path=path, **kwargs)

    @classmethod
    def repo(cls, **kwargs: Any) -> Self:
        """
        Use a cache that is checked into the git repo.
        This is intended to be used by CI.
        """
        path = Path(__file__).parent / "../tests/llm-cache"
        return cls(path=path, **kwargs)

    def get_message_digest(self, messages: list[dict[str, Any]]) -> str:
        import hashlib

        messages_str = json.dumps(messages, sort_keys=True)
        return hashlib.sha256(messages_str.encode()).hexdigest()

    def lookup(
        self,
        messages: list[dict[str, Any]],
    ) -> str | None:
        message_digest = self.get_message_digest(messages)
        cache_file = self._path / f"{message_digest}" / "response.txt"

        if cache_file.exists():
            logging.debug(f"Cache hit: {cache_file}")
            with open(cache_file, encoding="utf-8") as f:
                return f.read()
        return None

    def update(
        self,
        messages: list[dict[str, Any]],
        response: str,
        model: str | None = None,
        identifier: str | None = None,
        transform: str | None = None,
    ) -> None:
        message_digest = self.get_message_digest(messages)
        cache_file = self._path / f"{message_digest}" / "response.txt"

        cache_file.parent.mkdir(parents=True, exist_ok=True)

        with open(cache_file, "w", encoding="utf-8") as f:
            f.write(response)

        cache_file = self._path / f"{message_digest}" / "messages.txt"
        messages_str = json.dumps(messages, sort_keys=True)
        with open(cache_file, "w", encoding="utf-8") as f:
            f.write(messages_str)

        if model:
            model_file = self._path / f"{message_digest}" / "model.txt"
            with open(model_file, "w", encoding="utf-8") as f:
                f.write(model)

        if identifier:
            identifier_file = self._path / f"{message_digest}" / "identifier.txt"
            with open(identifier_file, "w", encoding="utf-8") as f:
                f.write(identifier)

        if transform:
            transform_file = self._path / f"{message_digest}" / "transform.txt"
            with open(transform_file, "w", encoding="utf-8") as f:
                f.write(transform)

        logging.debug(f"Cache updated: {cache_file}")

    def clear(self) -> None:
        self._path.unlink(missing_ok=True)


class FrozenCache(AbstractCache):
    """
    Cache that does not allow updates of an inner cache.
    """

    def __init__(self, inner_cache: AbstractCache, **kwargs: Any):
        super().__init__(Path("/dev/null"), **kwargs)
        self._inner_cache = inner_cache

    @property
    def inner_cache(self) -> AbstractCache:
        return self._inner_cache

    def lookup(
        self,
        messages: list[dict[str, Any]],
    ) -> str | None:
        return self.inner_cache.lookup(messages)

    def update(
        self,
        messages: list[dict[str, Any]],
        response: str,
        model: str | None = None,
        identifier: str | None = None,
        transform: str | None = None,
    ) -> None:
        pass

    def clear(self) -> None:
        pass
