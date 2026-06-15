import logging
import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Any, cast

from postprocess.definitions import (
    CDefinition,
    MergeRustError,
    get_c_definitions,
    get_rust_definitions,
    update_rust_definition,
)
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.utils import get_highlighted_c


class TransformError(Exception):
    """A transform failed to process a single definition."""


@dataclass(frozen=True)
class TransformCandidate:
    identifier: str
    messages: list[dict[str, Any]]
    response: str
    # Full replacement Rust definition for `identifier`, ready for merge_rust.
    definition: str
    cacheable: bool = True


class AbstractTransform(ABC):
    """
    Abstract base class for LLM-driven transforms of c2rust transpiler output.
    """

    max_merge_attempts = 3

    def __init__(self, system_instruction: str):
        self._system_instruction = system_instruction

    @property
    def system_instruction(self) -> str:
        return self._system_instruction

    def apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
        update_rust: bool = True,
    ) -> str | None:
        """
        Apply transform to one Rust definition and commit it through merge_rust.
        """
        previous_error: MergeRustError | None = None

        for attempt in range(self.max_merge_attempts):
            candidate = self.try_apply_ident(
                rust_source_file=rust_source_file,
                rust_definition=rust_definition,
                c_definition=c_definition,
                identifier=identifier,
                attempt=attempt,
                previous_error=previous_error,
            )
            if candidate is None:
                return None

            if candidate.definition == rust_definition:
                self.cache_candidate(candidate)
                logging.info(
                    f"{self.__class__.__name__}: "
                    f"No changes for function: {identifier}"
                )
                return None

            try:
                if update_rust:
                    update_rust_definition(
                        root_rust_source_file=rust_source_file,
                        identifier=candidate.identifier,
                        new_definition=candidate.definition,
                    )
            except MergeRustError as error:
                previous_error = error
                logging.warning(
                    f"merge_rust rejected transform candidate for {identifier} "
                    f"in {rust_source_file} on attempt "
                    f"{attempt + 1}/{self.max_merge_attempts}: {error}"
                )
                continue

            self.cache_candidate(candidate)

            logging.info(
                f"{self.__class__.__name__}: "
                f"Updated Rust fn {identifier}"
            )
            return candidate.definition

        raise TransformError(
            f"merge_rust failed after {self.max_merge_attempts} attempts "
            f"for {identifier}"
        ) from previous_error

    @abstractmethod
    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
        attempt: int = 0,
        previous_error: MergeRustError | None = None,
    ) -> TransformCandidate | None:
        """
        Return a candidate Rust replacement, or None to skip this identifier.
        """
        raise NotImplementedError

    def cache_candidate(self, candidate: TransformCandidate) -> None:
        if not candidate.cacheable:
            return

        transform = cast(Any, self)
        transform.cache.update(
            transform=self.__class__.__name__,
            identifier=candidate.identifier,
            model=transform.model.id,
            messages=candidate.messages,
            response=candidate.response,
        )

    @staticmethod
    def with_merge_retry_message(
        messages: list[dict[str, Any]], previous_error: MergeRustError | None
    ) -> list[dict[str, Any]]:
        if previous_error is None:
            return messages

        return [
            *messages,
            {
                "role": "user",
                "content": (
                    "The previous Rust function definition was rejected by "
                    f"merge_rust:\n{previous_error}\n\n"
                    "Return a syntactically valid full Rust function definition "
                    "only; say nothing else."
                ),
            },
        ]

    def apply_dir(
        self,
        root_rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
    ) -> int:
        """
        Run `self.apply_file` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.

        Returns the number of definitions that failed to transform.
        """
        failures = 0
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            failures += self.apply_file(
                rust_source_file=rs_path,
                exclude_list=exclude_list,
                ident_filter=ident_filter,
                update_rust=update_rust,
                keep_going=keep_going,
                failure_log_level=failure_log_level,
            )
        return failures

    def apply_file(
        self,
        rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
    ) -> int:
        ident_regex = re.compile(ident_filter) if ident_filter else None
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
                continue

            if ident_regex and not ident_regex.search(identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file}"
                    f"due to ident filter {ident_filter}"
                )
                continue

            if identifier not in c_definitions:
                logging.warning(f"No corresponding C definition found for {identifier}")
                continue

            c_definition = c_definitions[identifier]

            highlighted_c_definition = get_highlighted_c(c_definition.effective)
            logging.debug(
                f"C function {identifier} definition:\n{highlighted_c_definition}\n"
            )

            try:
                self.apply_ident(
                    rust_source_file=rust_source_file,
                    rust_definition=rust_definition,
                    c_definition=c_definition,
                    identifier=identifier,
                    update_rust=update_rust,
                )
            except TransformError as error:
                if not keep_going:
                    raise
                logging.log(
                    failure_log_level,
                    f"Transform failed for {identifier} in {rust_source_file}: {error}",
                )
                failures += 1

        return failures


# TODO: We probably want a an interface that generates validators specialized to
#       each individual prompt so maybe this should take in some transform-
#       specific parameters and return a callable that only takes the LLM
#       response as input.
# class AbstractValidator(ABC):
#     @abstractmethod
#     def validate_response(self, response: str) -> str:
#         pass
