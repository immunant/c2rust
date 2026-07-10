import os
import time
from pathlib import Path

from postprocess.cache import AbstractCache, DirectoryCache, FrozenCache

MESSAGES = [{"role": "user", "content": "hello"}]


def add_entry(cache: DirectoryCache, identifier: str) -> Path:
    cache.update(
        transform="CommentTransfer",
        identifier=identifier,
        model="test-model",
        messages=MESSAGES,
        response=f"response for {identifier}",
    )
    return cache.cache_dir(
        transform="CommentTransfer", identifier=identifier, messages=MESSAGES
    )


def lookup(cache: AbstractCache, identifier: str) -> str | None:
    return cache.lookup(
        transform="CommentTransfer",
        identifier=identifier,
        model="test-model",
        messages=MESSAGES,
    )


def set_age_days(entry_dir: Path, days: int) -> None:
    mtime = time.time() - days * 24 * 60 * 60
    os.utime(entry_dir / "metadata.toml", (mtime, mtime))


def test_lookup_returns_cached_response(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    add_entry(cache, "foo")
    assert lookup(cache, "foo") == "response for foo"


def test_lookup_miss_returns_none(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    assert lookup(cache, "absent") is None


def test_lookup_marks_entry_as_recently_used(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    entry = add_entry(cache, "foo")
    set_age_days(entry, 30)
    before = (entry / "metadata.toml").stat().st_mtime
    lookup(cache, "foo")
    assert (entry / "metadata.toml").stat().st_mtime > before


def test_prune_removes_only_stale_entries(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    stale = add_entry(cache, "stale")
    fresh = add_entry(cache, "fresh")
    set_age_days(stale, 91)
    cache.prune(max_age_days=90)
    assert not stale.exists()
    assert fresh.exists()


def invalidate(cache: AbstractCache, identifier: str) -> None:
    cache.invalidate(transform="CommentTransfer", identifier=identifier)


def test_invalidate_removes_updated_entry(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    entry = add_entry(cache, "foo")
    invalidate(cache, "foo")
    assert not entry.exists()
    assert lookup(cache, "foo") is None


def test_invalidate_removes_looked_up_entry(tmp_path: Path) -> None:
    entry = add_entry(DirectoryCache(tmp_path), "foo")
    # A fresh cache instance has no memory of past use...
    cache = DirectoryCache(tmp_path)
    invalidate(cache, "foo")
    assert entry.exists()
    # ...until the entry is looked up.
    lookup(cache, "foo")
    invalidate(cache, "foo")
    assert not entry.exists()


def test_invalidate_unused_entry_is_noop(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    invalidate(cache, "absent")


def test_frozen_cache_ignores_invalidate(tmp_path: Path) -> None:
    inner = DirectoryCache(tmp_path)
    entry = add_entry(inner, "foo")
    frozen = FrozenCache(inner)
    assert lookup(frozen, "foo") == "response for foo"
    invalidate(frozen, "foo")
    assert entry.exists()
    assert lookup(frozen, "foo") == "response for foo"


def test_lookup_keeps_entry_alive_across_prune(tmp_path: Path) -> None:
    cache = DirectoryCache(tmp_path)
    entry = add_entry(cache, "foo")
    set_age_days(entry, 91)
    lookup(cache, "foo")
    cache.prune(max_age_days=90)
    assert entry.exists()
