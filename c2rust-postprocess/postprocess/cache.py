from abc import ABC, abstractmethod
from pathlib import Path
from tempfile import gettempdir
from typing import Any


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
        response: str
    ) -> None:
        """Store a response in the cache for the given messages.

        Args:
            messages: The list of messages representing the conversation history.
            response: The response text to cache.
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

    def __init__(self, path: Path | None = None, **kwargs: Any):
        if path is None:
            path = Path(gettempdir()) / "c2rust_postprocess"
        super().__init__(path, **kwargs)
        self._path.mkdir(parents=True, exist_ok=True)

    def get_cache_file_name(self, messages: list[dict[str, Any]]) -> Path:
        import hashlib
        import json

        messages_str = json.dumps(messages, sort_keys=True)
        hash_digest = hashlib.sha256(messages_str.encode()).hexdigest()
        return self._path / f"{hash_digest}.txt"

    def lookup(
        self,
        messages: list[dict[str, Any]],
    ) -> str | None:
        cache_file = self.get_cache_file_name(messages)

        if cache_file.exists():
            with open(cache_file, encoding='utf-8') as f:
                return f.read()
        return None

    def update(
        self,
        messages: list[dict[str, Any]],
        response: str
    ) -> None:
        cache_file = self.get_cache_file_name(messages)

        with open(cache_file, 'w', encoding='utf-8') as f:
            f.write(response)

    def clear(self) -> None:
        self._path.unlink(missing_ok=True)
