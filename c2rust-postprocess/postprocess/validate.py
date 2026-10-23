"""Transactional validation of applied rewrites via `cargo check`."""

import json
import logging
import shlex
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


class CommandChecker:
    """
    Runs a user-provided validation command (e.g. `cargo build`, `cargo test`,
    or a custom script) in the crate's root directory. The command must exit
    0 for the current state to be considered valid.
    """

    def __init__(self, command: Sequence[str], cwd: Path | None = None):
        self.command = list(command)
        self.cwd = cwd

    def __call__(self) -> str | None:
        result = subprocess.run(
            self.command,
            cwd=self.cwd,
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode == 0:
            return None
        output = "\n".join(
            part for part in (result.stdout.strip(), result.stderr.strip()) if part
        )
        return (
            f"validation command `{' '.join(self.command)}` failed with exit "
            f"code {result.returncode}" + (f":\n{output}" if output else "")
        )


def find_manifest(rust_source_file: Path) -> Path | None:
    """Return the nearest Cargo.toml at or above the file's directory."""
    for directory in rust_source_file.resolve().parents:
        manifest = directory / "Cargo.toml"
        if manifest.is_file():
            return manifest
    return None


class BaselineError(Exception):
    """The crate failed validation before any rewrites were applied."""


def make_validator(
    rust_source_file: Path, validate_cmds: Sequence[str] = ()
) -> BatchValidator | None:
    """
    Build a validator for the crate containing `rust_source_file`, checking
    first that the baseline passes so a broken crate is not misattributed to
    the rewrites. `validate_cmds` are extra shell commands (split with
    `shlex`) run in the crate's root directory after `cargo check`; every
    command must exit 0 for a state to be considered valid. Returns None
    when there is no Cargo.toml to check against; raises BaselineError when
    the baseline does not pass.
    """
    manifest_path = find_manifest(rust_source_file)
    if manifest_path is None:
        logging.warning(
            f"No Cargo.toml found above {rust_source_file}; "
            "applying rewrites without cargo validation"
        )
        return None

    checks: list[Callable[[], str | None]] = [CargoChecker(manifest_path)]
    checks.extend(
        CommandChecker(shlex.split(cmd), cwd=manifest_path.parent)
        for cmd in validate_cmds
    )
    combined = _first_error(checks)

    logging.info(f"Running baseline checks for {manifest_path}...")
    error = combined()
    if error is not None:
        raise BaselineError(
            "Crate does not pass baseline validation before postprocessing; "
            f"aborting without applying rewrites:\n{error}"
        )
    return BatchValidator(combined)


def _first_error(
    checks: Sequence[Callable[[], str | None]],
) -> Callable[[], str | None]:
    """Run each check in order, returning the first failure's description."""

    def check() -> str | None:
        for run in checks:
            error = run()
            if error is not None:
                return error
        return None

    return check
