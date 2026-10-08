import asyncio
from collections.abc import Awaitable, Callable
from pathlib import Path

import pytest
from test_apply_file import CannedCache

from postprocess.cache import DirectoryCache, FrozenCache
from postprocess.definitions import CDefinition
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.models.base import AbstractGenerativeModel
from postprocess.models.mock import MockGenerativeModel
from postprocess.transforms import base
from postprocess.transforms.base import AbstractTransform, TransformError

IDENTIFIERS = ["first", "second", "third", "fourth"]


class AsyncTransform(AbstractTransform):
    def __init__(self, generate: Callable[[str], Awaitable[str | None]]):
        super().__init__("", CannedCache({}), MockGenerativeModel())
        self.generate_definition = generate

    async def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
    ) -> str | None:
        return await self.generate_definition(identifier)


@pytest.fixture
def source(monkeypatch, tmp_path: Path) -> tuple[Path, list[str]]:
    path = tmp_path / "lib.rs"
    path.write_text("original\n")
    writes: list[str] = []
    monkeypatch.setattr(
        base,
        "get_rust_definitions",
        lambda path: {identifier: "original" for identifier in IDENTIFIERS},
    )
    monkeypatch.setattr(
        base,
        "get_c_definitions",
        lambda path: {
            identifier: CDefinition(
                definition="void f(void) {}", preprocessed_definition=None
            )
            for identifier in IDENTIFIERS
        },
    )

    def update(*, root_rust_source_file, identifier, new_definition):
        writes.append(identifier)
        root_rust_source_file.write_text(new_definition)

    monkeypatch.setattr(base, "update_rust_definition", update)
    return path, writes


@pytest.mark.parametrize("jobs", [1, 2])
def test_jobs_bounds_concurrent_transformations(source, jobs: int) -> None:
    path, writes = source

    async def run() -> None:
        started: asyncio.Queue[str] = asyncio.Queue()
        release: asyncio.Queue[None] = asyncio.Queue()
        active = 0
        peak = 0

        async def generate(identifier: str) -> str:
            nonlocal active, peak
            active += 1
            peak = max(peak, active)
            started.put_nowait(identifier)
            try:
                await release.get()
                return identifier
            finally:
                active -= 1

        task = asyncio.create_task(
            AsyncTransform(generate).apply_file(
                path, IdentifierExcludeList(None), jobs=jobs
            )
        )
        for _ in range(jobs):
            await asyncio.wait_for(started.get(), timeout=2)
        assert active == jobs
        assert writes == []
        for _ in IDENTIFIERS:
            release.put_nowait(None)
        result = await asyncio.wait_for(task, timeout=2)
        assert result.failures == 0
        assert peak == jobs
        assert active == 0

    asyncio.run(run())
    assert writes == IDENTIFIERS


def test_out_of_order_responses_are_applied_in_source_order(source) -> None:
    path, writes = source

    async def run() -> None:
        started: asyncio.Queue[str] = asyncio.Queue()
        completed: asyncio.Queue[str] = asyncio.Queue()
        release = {identifier: asyncio.Event() for identifier in IDENTIFIERS}

        async def generate(identifier: str) -> str:
            started.put_nowait(identifier)
            await release[identifier].wait()
            completed.put_nowait(identifier)
            return identifier

        task = asyncio.create_task(
            AsyncTransform(generate).apply_file(
                path, IdentifierExcludeList(None), jobs=4
            )
        )
        for _ in IDENTIFIERS:
            await asyncio.wait_for(started.get(), timeout=2)
        for identifier in reversed(IDENTIFIERS):
            assert writes == []
            release[identifier].set()
            assert await asyncio.wait_for(completed.get(), timeout=2) == identifier
        await asyncio.wait_for(task, timeout=2)

    asyncio.run(run())
    assert writes == IDENTIFIERS


def test_keep_going_applies_successes_and_reports_failure(source) -> None:
    path, writes = source

    async def generate(identifier: str) -> str:
        await asyncio.sleep(0)
        if identifier == "second":
            raise TransformError("rejected")
        return identifier

    async def run():
        return await asyncio.wait_for(
            AsyncTransform(generate).apply_file(
                path, IdentifierExcludeList(None), jobs=2, keep_going=True
            ),
            timeout=2,
        )

    result = asyncio.run(run())
    assert result.failed == [(path, "second", "failed to transform")]
    assert writes == ["first", "third", "fourth"]


def test_abort_cancels_pending_transformations_before_writing(source) -> None:
    path, writes = source

    async def run() -> None:
        first_started = asyncio.Event()
        cancelled = asyncio.Event()
        pending = asyncio.Event()
        active: set[str] = set()

        async def generate(identifier: str) -> str:
            active.add(identifier)
            try:
                if identifier == "second":
                    await first_started.wait()
                    raise TransformError("rejected")
                if identifier == "first":
                    first_started.set()
                await pending.wait()
                return identifier
            except asyncio.CancelledError:
                if identifier == "first":
                    cancelled.set()
                raise
            finally:
                active.remove(identifier)

        with pytest.raises(TransformError, match="rejected"):
            await asyncio.wait_for(
                AsyncTransform(generate).apply_file(
                    path, IdentifierExcludeList(None), jobs=2
                ),
                timeout=2,
            )
        assert cancelled.is_set()
        assert not active
        assert writes == []
        assert path.read_text() == "original\n"

    asyncio.run(run())


def test_parallel_responses_are_reused_by_serial_frozen_run(
    source, tmp_path: Path, monkeypatch
) -> None:
    path, writes = source

    class CountingModel(AbstractGenerativeModel):
        def __init__(self):
            super().__init__("test-model")
            self.calls = 0

        async def generate_with_tools(self, messages, tools=(), max_tool_loops=5):
            self.calls += 1
            await asyncio.sleep(0)
            return messages[0]["content"]

    class CachedTransform(AbstractTransform):
        async def try_apply_ident(
            self, rust_source_file, rust_definition, c_definition, identifier
        ):
            return await self.generate(
                identifier,
                [{"role": "user", "content": identifier}],
                lambda response: response,
            )

    cache = DirectoryCache(tmp_path / "cache")
    model = CountingModel()
    result = asyncio.run(
        CachedTransform("", cache, model).apply_file(
            path, IdentifierExcludeList(None), jobs=4
        )
    )
    assert result.failures == 0
    assert writes == IDENTIFIERS
    assert model.calls == len(IDENTIFIERS)
    assert len(list(cache.path.glob("*/*/*/metadata.toml"))) == len(IDENTIFIERS)
    first_output = path.read_text()

    path.write_text("original\n")
    writes.clear()
    frozen = FrozenCache(DirectoryCache(cache.path))

    def unexpected_update(**kwargs):
        raise AssertionError("cache hits must not update responses")

    monkeypatch.setattr(frozen, "update", unexpected_update)
    result = asyncio.run(
        CachedTransform("", frozen, model).apply_file(
            path, IdentifierExcludeList(None), jobs=1
        )
    )
    assert result.failures == 0
    assert writes == IDENTIFIERS
    assert path.read_text() == first_output
    assert model.calls == len(IDENTIFIERS)
