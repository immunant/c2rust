import json
import logging
import tempfile
from collections.abc import Callable, Iterable
from pathlib import Path
from typing import Any

from postprocess.models.base import AbstractGenerativeModel

# Force a clean, parseable result so the chatty coding agent can't wrap the
# Rust function in prose. `codex exec --output-schema` validates the final
# message against this schema.
_OUTPUT_SCHEMA: dict[str, Any] = {
    "type": "object",
    "properties": {
        "rust_function": {
            "type": "string",
            "description": (
                "The transformed Rust function definition, as raw Rust source "
                "with no Markdown code fences or surrounding prose."
            ),
        },
    },
    "required": ["rust_function"],
    "additionalProperties": False,
}


class CodexModel(AbstractGenerativeModel):
    """Runs the Codex coding agent non-interactively via `codex exec`.

    Uses Codex's saved CLI auth (``~/.codex/auth.json``), so it works with a
    ChatGPT subscription login established via ``codex login`` -- no
    ``OPENAI_API_KEY`` required.
    """

    def __init__(
        self,
        id: str = "codex",
        *,
        codex_model: str = "gpt-5.1-codex",
        sandbox: str = "read-only",
        system_instruction: str | None = None,
        timeout_s: int = 600,
    ):
        super().__init__(id)
        self._codex_model = codex_model
        self._sandbox = sandbox
        self._system_instruction = system_instruction
        self._timeout_s = timeout_s

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,  # codex manages its own loop; unused
    ) -> str | None:
        # Import lazily so the module imports even where `codex` isn't needed.
        import subprocess

        assert not tools, "external tools unsupported; Codex brings its own"

        parts = [self._system_instruction] if self._system_instruction else []
        parts += [str(m["content"]) for m in messages]
        prompt = "\n\n".join(parts)

        # Run in an empty temp dir so the agent can't wander the real repo; the
        # prompt is self-contained (C function + Rust function inline).
        with tempfile.TemporaryDirectory() as tmp:
            tmp_dir = Path(tmp)
            schema_path = tmp_dir / "schema.json"
            out_path = tmp_dir / "out.json"
            schema_path.write_text(json.dumps(_OUTPUT_SCHEMA))

            cmd = [
                "codex",
                "exec",
                "--model",
                self._codex_model,
                "--sandbox",
                self._sandbox,  # read-only: no edits
                "--skip-git-repo-check",
                "--ephemeral",  # don't persist sessions under ~/.codex
                "--output-schema",
                str(schema_path),
                "-o",
                str(out_path),  # write the final message only
                "--",
                prompt,
            ]

            try:
                proc = subprocess.run(
                    cmd,
                    text=True,
                    capture_output=True,
                    cwd=tmp_dir,
                    timeout=self._timeout_s,
                )
            except FileNotFoundError:
                logging.error("`codex` binary not found on PATH")
                return None
            except subprocess.TimeoutExpired:
                logging.error(f"codex exec timed out after {self._timeout_s}s")
                return None

            if proc.returncode != 0:
                logging.error(
                    f"codex exec failed (rc={proc.returncode}): "
                    f"{proc.stderr.strip()}"
                )
                return None

            try:
                raw = out_path.read_text()
            except FileNotFoundError:
                logging.error("codex exec produced no output file")
                return None

            try:
                return json.loads(raw)["rust_function"]
            except (json.JSONDecodeError, KeyError, TypeError) as error:
                logging.error(f"could not parse codex output: {error}\n{raw[:500]}")
                return None
