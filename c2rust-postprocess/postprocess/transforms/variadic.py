import re
from pathlib import Path

from postprocess.exclude_list import IdentifierExcludeList
from postprocess.transforms.base import AbstractTransform
from postprocess.utils import get_rust_files


_VA_LIST_IMPL_RE = re.compile(r"\bVaListImpl\b")
_AS_VA_LIST_RE = re.compile(r"\.as_va_list\(\)")


def rewrite_variadic_compat(rust_source: str) -> tuple[str, bool]:
    """Rewrite c2rust varargs output to the newer Rust stdlib API shape."""
    rewritten = _VA_LIST_IMPL_RE.sub("VaList", rust_source)
    rewritten = _AS_VA_LIST_RE.sub(".clone()", rewritten)
    return rewritten, rewritten != rust_source


class FixVaListDriftTransform(AbstractTransform):
    """Deterministic fix for c2rust VaList API drift."""

    def __init__(self):
        super().__init__("Fix c2rust VaList API drift")

    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: str,
        identifier: str,
        update_rust: bool = True,
    ) -> None:
        # This transform operates on whole files rather than per-definition spans.
        return

    def apply_dir(
        self,
        root_rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ):
        root_dir = root_rust_source_file.parent
        for rust_source_file in get_rust_files(root_dir):
            if "target" in rust_source_file.parts:
                continue
            source = rust_source_file.read_text()
            rewritten, changed = rewrite_variadic_compat(source)
            if not changed:
                continue

            if update_rust:
                rust_source_file.write_text(rewritten)
