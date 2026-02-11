import logging
import re
from collections.abc import Callable, Iterable
from dataclasses import dataclass, field
from functools import partial
from pathlib import Path
from typing import Any

from postprocess.cache import AbstractCache
from postprocess.definitions import (
    CDefinition,
    get_c_definitions,
    get_rust_definitions,
    update_rust_definition,
)
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.models import AbstractGenerativeModel, api_key_from_env
from postprocess.utils import get_highlighted_c
from postprocess.validate import BatchValidator, Candidate


class TransformError(Exception):
    """A transform failed to process a single definition."""


type TransformFailure = tuple[Path, str, str]


@dataclass
class TransformResult:
    """Summary of applying a transform to one or more Rust definitions."""

    failed: list[TransformFailure] = field(default_factory=list)

    @property
    def failures(self) -> int:
        return len(self.failed)

    def extend(self, other: "TransformResult") -> None:
        self.failed.extend(other.failed)


class AbstractTransform:
    """
    Abstract base class for LLM-driven transforms of c2rust transpiler output.
    """

    max_attempts = 3

    def __init__(
        self,
        system_instruction: str,
        cache: AbstractCache,
        model: AbstractGenerativeModel,
    ):
        self._system_instruction = system_instruction
        self.cache = cache
        self.model = model

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
        Apply the transform to one Rust definition and commit it through
        merge_rust.
        """
        new_definition = self.try_apply_ident(
            rust_source_file=rust_source_file,
            rust_definition=rust_definition,
            c_definition=c_definition,
            identifier=identifier,
        )
        if new_definition is None:
            return None

        if new_definition == rust_definition:
            logging.info(
                f"{self.__class__.__name__}: No changes for function: {identifier}"
            )
            return None

        if update_rust:
            update_rust_definition(
                root_rust_source_file=rust_source_file,
                identifier=identifier,
                new_definition=new_definition,
            )

        logging.info(f"{self.__class__.__name__}: Transformed Rust fn {identifier}")
        return new_definition

    def try_apply_ident(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: CDefinition,
        identifier: str,
    ) -> str | None:
        """
        Return a replacement Rust definition for `identifier`,
        or None to skip it.
        """
        raise NotImplementedError(
            f"{self.__class__.__name__} must implement try_apply_ident, "
            "or override apply_ident directly"
        )

    def generate(
        self,
        identifier: str,
        messages: list[dict[str, Any]],
        check: Callable[[str], str],
        tools: Iterable[Callable[..., Any]] = (),
    ) -> str | None:
        """
        Model call with caching, validation, and retries. `check` returns the
        validated result or raises TransformError to trigger regeneration.
        """
        transform = self.__class__.__name__
        error: TransformError | None = None

        response = self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=self.model.id,
            messages=messages,
        )
        if response is not None:
            try:
                return check(response)
            except TransformError as stale:
                error = stale
                logging.warning(
                    f"{transform}: cached response for {identifier} "
                    f"failed validation: {stale}"
                )

        if api_key_from_env(self.model.id) is None:
            if error is not None:
                # Can't regenerate the invalid cached response without a key.
                raise error
            logging.warning(
                f"Cache miss for {identifier}; skipping since no API key was set..."
            )
            return None

        for attempt in range(self.max_attempts):
            try:
                response = self.model.generate_with_tools(messages, tools=tools)
                if response is None:
                    raise TransformError(f"model returned no response for {identifier}")
                result = check(response)
            except TransformError as rejected:
                error = rejected
                logging.warning(
                    f"{transform}: response for {identifier} rejected on attempt "
                    f"{attempt + 1}/{self.max_attempts}: {rejected}"
                )
                continue

            self.cache.update(
                transform=transform,
                identifier=identifier,
                model=self.model.id,
                messages=messages,
                response=response,
            )
            return result

        raise TransformError(
            f"{transform} failed after {self.max_attempts} attempts "
            f"for {identifier}: {error}"
        ) from error

    def apply_dir(
        self,
        root_rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
        validator: BatchValidator | None = None,
    ) -> TransformResult:
        """
        Run `self.apply_file` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.

        Returns the failed and cargo-rejected definitions.
        """
        result = TransformResult()
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            result.extend(
                self.apply_file(
                    rust_source_file=rs_path,
                    exclude_list=exclude_list,
                    ident_filter=ident_filter,
                    update_rust=update_rust,
                    keep_going=keep_going,
                    failure_log_level=failure_log_level,
                    validator=validator,
                )
            )
        return result

    def apply_file(
        self,
        rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
        keep_going: bool = False,
        failure_log_level: int = logging.ERROR,
        validator: BatchValidator | None = None,
    ) -> TransformResult:
        ident_regex = re.compile(ident_filter) if ident_filter else None
        result = TransformResult()

        rust_definitions = get_rust_definitions(rust_source_file)
        c_definitions = get_c_definitions(rust_source_file)

        logging.info(f"Loaded {len(rust_definitions)} Rust definitions")
        logging.info(f"Loaded {len(c_definitions)} C definitions")

        # Collect candidates without touching the file; application and
        # validation happen per batch below.
        candidates: list[Candidate] = []
        for identifier, rust_definition in rust_definitions.items():
            if exclude_list.contains(path=rust_source_file, identifier=identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file} "
                    f"due to exclude file {exclude_list.src_path}"
                )
                continue

            if ident_regex and not ident_regex.search(identifier):
                logging.info(
                    f"Skipping Rust fn {identifier} in {rust_source_file} "
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
                new_definition = self.apply_ident(
                    rust_source_file=rust_source_file,
                    rust_definition=rust_definition,
                    c_definition=c_definition,
                    identifier=identifier,
                    update_rust=False,
                )
            except TransformError as error:
                if not keep_going:
                    raise
                logging.log(
                    failure_log_level,
                    f"Transform failed for {identifier} in {rust_source_file}: {error}",
                )
                result.failed.append(
                    (rust_source_file, identifier, "failed to transform")
                )
                continue

            if new_definition is None:
                continue

            candidates.append(
                Candidate(
                    identifier=identifier,
                    files=(rust_source_file,),
                    apply=partial(
                        update_rust_definition,
                        root_rust_source_file=rust_source_file,
                        identifier=identifier,
                        new_definition=new_definition,
                    ),
                    invalidate=partial(
                        self.cache.invalidate,
                        transform=self.__class__.__name__,
                        identifier=identifier,
                    ),
                )
            )

        if not update_rust or not candidates:
            return result

        if validator is None:
            for candidate in candidates:
                candidate.apply()
            return result

        logging.info(
            f"Validating {len(candidates)} rewrite(s) in {rust_source_file} "
            "with cargo check"
        )
        _, rejected = validator.validate(candidates)
        for candidate, error in rejected:
            candidate.invalidate()
            result.failed.append(
                (rust_source_file, candidate.identifier, "rejected by cargo check")
            )
            logging.log(
                failure_log_level,
                f"cargo check rejected rewrite of {candidate.identifier} "
                f"in {rust_source_file}:\n{error}",
            )
        if rejected and not keep_going:
            raise TransformError(
                f"cargo check rejected rewrite of {rejected[0][0].identifier} "
                f"in {rust_source_file}"
            )

        return result


# TODO: We probably want a an interface that generates validators specialized to
#       each individual prompt so maybe this should take in some transform-
#       specific parameters and return a callable that only takes the LLM
#       response as input.
# class AbstractValidator(ABC):
#     @abstractmethod
#     def validate_response(self, response: str) -> str:
#         pass
