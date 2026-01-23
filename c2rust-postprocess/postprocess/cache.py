import json
import logging
from abc import ABC, abstractmethod
from errno import ENOTEMPTY
from hashlib import sha256
from pathlib import Path
from tempfile import gettempdir
from typing import Any, Self, Union

import tomli
import tomlkit
from platformdirs import user_cache_dir
from tomlkit.items import String

from postprocess.utils import check_isinstance


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

    def gc_sweep(self) -> None:
        """
        Garbage collect everything in the cache that
        hasn't been used by `lookup` or `update` since the last `gc_sweep`.
        """
        raise NotImplementedError


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
        elif value is None:
            raise TypeError("top-level `None` `TomlValue`s are not allowed")
        else:
            return value

    converted_value = convert_value(value)
    converted_value = check_isinstance(converted_value, dict)
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
        messages_str = json.dumps(messages, sort_keys=True)
        return sha256(messages_str.encode()).hexdigest()

    def cache_dir(
        self,
        *,
        transform: str,
        identifier: str,
        messages: list[dict[str, Any]],
    ) -> Path:
        message_digest = self.get_message_digest(messages)
        return self._path / transform / identifier / message_digest

    def gc_mark_file(self) -> Path:
        """
        `.gc`, containing paths to not be swept/deleted.
        """

        return self._path / ".gc"

    def gc_mark(self, paths: list[Path]):
        """
        Mark paths to not be swept/deleted by updating their mtime.
        """

        for path in paths:
            path.touch()
        self.gc_mark_file().touch()

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
        metadata_path = cache_dir / "metadata.toml"
        response_path = cache_dir / "response.txt"
        try:
            toml = metadata_path.read_text()
        except FileNotFoundError:
            data = {
                "transform": transform,
                "identifier": identifier,
                "model": model,
                "messages": messages,
            }
            toml = to_multiline_toml(data)
            logging.debug(f"Cache miss: {metadata_path}:\n{toml}")
            return None

        logging.debug(f"Cache hit: {metadata_path}:\n{toml}")
        self.gc_mark([metadata_path, response_path])
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

        logging.debug(f"Cache updated: {cache_dir}:\n{toml}")
        # The `.write_text`s above updated the mtimes already,
        # so no need to call `self.gc_mark`.
        self.gc_mark([])

    def clear(self) -> None:
        self._path.unlink(missing_ok=True)

    def gc_sweep(self) -> None:
        """
        Sweep/delete everything in the cache with an mtime older than `.gc`'s ctime.
        """

        gc_mark_file = self.gc_mark_file()
        try:
            oldest_allowed = gc_mark_file.stat().st_ctime_ns
        except FileNotFoundError:
            return  # No `.gc` file; nothing to sweep.

        def walk(dir: Path) -> bool:
            """
            Walk `dir`, removing any files older than `oldest_allowed`.
            If `dir` wasn't empty before and is now empty, remove it, too.
            Return if `dir` was removed or not.
            """

            # First remove all files in the dir and recurse into subdirs.
            removed_any = False
            for path in dir.iterdir():
                if path.is_dir():
                    if walk(dir):
                        removed_any = True
                else:
                    if path.stat().st_mtime_ns < oldest_allowed:
                        try:
                            path.unlink()
                            removed_any = True
                        except OSError as e:
                            logging.warning(f"gc_sweep: failed to unlink {path}: {e}")

            # If we haven't removed anything in the dir,
            # then there's no reason to remove it, even if it's empty.
            if not removed_any:
                return False

            # If we have removed something, try to delete the dir.
            # This only succeeds if the dir is empty, which is what we want.
            try:
                dir.rmdir()
                return True
            except OSError as e:
                if e.errno == ENOTEMPTY:
                    pass
                else:
                    logging.warning(f"gc_sweep: failed to rmdir {dir}: {e}")
                return False

        walk(self._path)

        # Shouldn't have been unlinked above,
        # as its mtime can't be older than its ctime.
        # Similarly, because this wasn't deleted,
        # the cache dir shouldn't have been deleted either.
        # Delete this at the end, so that if the sweep is interrupted,
        # we still have the ctime to try again.
        gc_mark_file.unlink()


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
