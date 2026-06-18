import logging
import re
from abc import ABC, abstractmethod
from enum import Enum
from pathlib import Path

from postprocess.definitions import (
    get_c_definitions,
    get_rust_definitions,
)
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.utils import get_highlighted_c


class TransformError(Exception):
    """A transform failed to process a single definition."""


class TransformOutcome(Enum):
    REWRITE = "rewrite"
    CACHE = "cache"
    SKIP = "skip"


class AbstractTransform(ABC):
    """
    Abstract base class for LLM-driven transforms of c2rust transpiler output.
    """

    def __init__(self, system_instruction: str):
        self._system_instruction = system_instruction

    @property
    def system_instruction(self) -> str:
        return self._system_instruction

    @abstractmethod
    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: str,
        identifier: str,
        update_rust: bool = True,
    ) -> TransformOutcome:
        """
        Implementations should apply transform to a single Rust definition
        with the given identifier.
        """
        pass

    def apply_dir(
        self,
        root_rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
    ) -> tuple[int, int, int, int]:
        """
        Run `self.apply_file` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.

        Returns the number of rewrites, cache hits, skips, and failures.
        """
        rewrites = 0
        cache_hits = 0
        skips = 0
        failures = 0
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            file_rewrites, file_cache_hits, file_skips, file_failures = self.apply_file(
                rust_source_file=rs_path,
                exclude_list=exclude_list,
                ident_filter=ident_filter,
                update_rust=update_rust,
                keep_going=keep_going,
                failure_log_level=failure_log_level,
            )
            rewrites += file_rewrites
            cache_hits += file_cache_hits
            skips += file_skips
            failures += file_failures
        return rewrites, cache_hits, skips, failures

    def apply_file(
        self,
        rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
    ) -> tuple[int, int, int, int]:
        ident_regex = re.compile(ident_filter) if ident_filter else None
        rewrites = 0
        cache_hits = 0
        skips = 0
        failures = 0

        rust_definitions = get_rust_definitions(rust_source_file)
        c_definitions = get_c_definitions(rust_source_file)

        logging.info(f"Loaded {len(rust_definitions)} Rust definitions")
        logging.info(f"Loaded {len(c_definitions)} C definitions")

        for identifier, rust_definition in rust_definitions.items():
            if exclude_list.contains(path=rust_source_file, identifier=identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file}"
                    f"due to exclude file {exclude_list.src_path}"
                )
                skips += 1
                continue

            if ident_regex and not ident_regex.search(identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file}"
                    f"due to ident filter {ident_filter}"
                )
                skips += 1
                continue

            if identifier not in c_definitions:
                logging.warning(f"No corresponding C definition found for {identifier}")
                skips += 1
                continue

            c_definition = c_definitions[identifier]

            highlighted_c_definition = get_highlighted_c(c_definition)
            logging.debug(
                f"C function {identifier} definition:\n{highlighted_c_definition}\n"
            )

            try:
                outcome = self.apply_ident(
                    rust_source_file=rust_source_file,
                    rust_definition=rust_definition,
                    c_definition=c_definition,
                    identifier=identifier,
                    update_rust=update_rust,
                )
                if outcome is TransformOutcome.REWRITE:
                    rewrites += 1
                elif outcome is TransformOutcome.CACHE:
                    cache_hits += 1
                else:
                    skips += 1
            except TransformError as error:
                if not keep_going:
                    raise
                logging.log(
                    failure_log_level,
                    f"Transform failed for {identifier} in {rust_source_file}: {error}",
                )
                failures += 1

        return rewrites, cache_hits, skips, failures


# TODO: We probably want a an interface that generates validators specialized to
#       each individual prompt so maybe this should take in some transform-
#       specific parameters and return a callable that only takes the LLM
#       response as input.
# class AbstractValidator(ABC):
#     @abstractmethod
#     def validate_response(self, response: str) -> str:
#         pass
