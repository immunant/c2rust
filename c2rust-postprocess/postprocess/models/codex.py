import subprocess
import tempfile
from collections.abc import Callable, Iterable
from pathlib import Path
from typing import Any

from postprocess.models import codex_login_api_key
from postprocess.models.base import AbstractGenerativeModel


class CodexModel(AbstractGenerativeModel):
    """
    Concrete implementation that uses the local Codex CLI login session.
    """

    def __init__(self, id: str):
        super().__init__(id)
        if codex_login_api_key() is None:
            raise RuntimeError(
                "Codex login session not found; run `codex login` first"
            )
        self._has_credentials = True

    @property
    def cache_key(self) -> str:
        return f"codex-{self.id}"

    def generate_with_tools(
        self,
        messages: list[dict[str, Any]],
        tools: Iterable[Callable[..., Any]] = (),
        max_tool_loops: int = 5,
    ) -> str | None:
        assert not tools, "Tool calling not yet implemented for CodexModel"
        del max_tool_loops

        prompt = self._format_messages(messages)
        with tempfile.NamedTemporaryFile(delete=False) as last_message_file:
            last_message_path = Path(last_message_file.name)

        cmd = [
            "codex",
            "--ask-for-approval",
            "never",
            "exec",
            "--skip-git-repo-check",
            "--sandbox",
            "read-only",
            "--model",
            self.id,
            "--output-last-message",
            str(last_message_path),
            "-",
        ]

        try:
            result = subprocess.run(
                cmd,
                input=prompt,
                text=True,
                capture_output=True,
                check=False,
            )
            if result.returncode != 0:
                raise RuntimeError(
                    "codex exec failed: "
                    f"exit code {result.returncode}"
                    " (details omitted to avoid leaking credentials)"
                )

            response = last_message_path.read_text().strip()
            if not response:
                raise RuntimeError("codex exec returned an empty final message")
            return response
        finally:
            last_message_path.unlink(missing_ok=True)

    def _format_messages(self, messages: list[dict[str, Any]]) -> str:
        """
        Convert a chat transcript into a single Codex prompt.
        """
        parts = []
        for message in messages:
            role = str(message.get("role", "user")).upper()
            content = str(message.get("content", ""))
            parts.append(f"{role}:\n{content}")
        return "\n\n".join(parts)
