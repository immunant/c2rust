"""Transactional validation of applied rewrites via `cargo check`."""

import json
import logging
import subprocess
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Candidate:
    """
    One atomic rewrite: applies itself and declares the files it touches.
    Bisection splits batches only at candidate boundaries, so a candidate
    spanning multiple files stays atomic.
    """

    identifier: str
    files: tuple[Path, ...]
    apply: Callable[[], None]
    invalidate: Callable[[], None]


class BatchValidator:
    """
    Applies candidate batches, keeping only those that pass `check`.
    `check` returns None if the current on-disk state is valid,
    or an error description otherwise.

    Bisection assumes a candidate that fails against the current validated
    state cannot be repaired by applying another candidate later.
    """

    def __init__(self, check: Callable[[], str | None]):
        self._check = check

    def validate(
        self, candidates: Sequence[Candidate]
    ) -> tuple[list[Candidate], list[tuple[Candidate, str]]]:
        """
        Return `(accepted, rejected)`; rejected candidates are paired with
        their error. Accepted candidates remain applied, rejected ones are
        rolled back, so the files are always left in the last state that
        passed `check`.
        """
        if not candidates:
            return [], []

        snapshots = {
            path: path.read_bytes()
            for candidate in candidates
            for path in candidate.files
        }
        ok = False
        try:
            for candidate in candidates:
                candidate.apply()
            error = self._check()
            ok = error is None
        finally:
            # `finally` rather than `except Exception` so KeyboardInterrupt
            # also restores the last validated state.
            if not ok:
                for path, data in snapshots.items():
                    path.write_bytes(data)

        if ok:
            return list(candidates), []

        if len(candidates) == 1:
            return [], [(candidates[0], error)]

        # Each half is checked against the state left by previously accepted
        # candidates, so interacting candidates are isolated correctly.
        logging.info(f"Check failed for batch of {len(candidates)}; bisecting")
        mid = len(candidates) // 2
        accepted, rejected = self.validate(candidates[:mid])
        right_accepted, right_rejected = self.validate(candidates[mid:])
        return accepted + right_accepted, rejected + right_rejected


class CargoChecker:
    """
    Checks a crate with `cargo check --release`.
    """

    def __init__(self, manifest_path: Path):
        self.manifest_path = manifest_path

    def __call__(self) -> str | None:
        result = subprocess.run(
            [
                "cargo",
                "check",
                "--release",
                "--message-format=json",
                "--manifest-path",
                str(self.manifest_path),
            ],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode == 0:
            return None

        errors = []
        for line in result.stdout.splitlines():
            try:
                message = json.loads(line)
            except json.JSONDecodeError:
                continue
            if message.get("reason") != "compiler-message":
                continue
            compiler_message = message.get("message") or {}
            if compiler_message.get("level") == "error":
                rendered = compiler_message.get("rendered")
                if rendered:
                    errors.append(rendered.rstrip("\n"))
        # No parsed errors means cargo itself failed (e.g. a manifest error).
        return "\n".join(errors) if errors else result.stderr


def find_manifest(rust_source_file: Path) -> Path | None:
    """Return the nearest Cargo.toml at or above the file's directory."""
    for directory in rust_source_file.resolve().parents:
        manifest = directory / "Cargo.toml"
        if manifest.is_file():
            return manifest
    return None


class BaselineError(Exception):
    """The crate failed cargo check before any rewrites were applied."""


def make_validator(rust_source_file: Path) -> BatchValidator | None:
    """
    Build a validator for the crate containing `rust_source_file`, checking
    first that the baseline compiles so a broken crate is not misattributed
    to the rewrites. Returns None when there is no Cargo.toml to check
    against; raises BaselineError when the baseline does not compile.
    """
    manifest_path = find_manifest(rust_source_file)
    if manifest_path is None:
        logging.warning(
            f"No Cargo.toml found above {rust_source_file}; "
            "applying rewrites without cargo validation"
        )
        return None

    check = CargoChecker(manifest_path)
    logging.info(f"Running baseline cargo check for {manifest_path}...")
    error = check()
    if error is not None:
        raise BaselineError(
            "Crate does not compile before postprocessing; "
            f"aborting without applying rewrites:\n{error}"
        )
    return BatchValidator(check)
