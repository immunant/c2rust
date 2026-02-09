import logging
import re
from abc import ABC, abstractmethod
from pathlib import Path

from postprocess.definitions import (
    get_c_definitions,
    get_rust_definitions,
)
from postprocess.exclude_list import IdentifierExcludeList
from postprocess.utils import get_highlighted_c


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
    ) -> str | None:
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
    ):
        """
        Run `self.apply_file` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.
        """
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            self.apply_file(
                rust_source_file=rs_path,
                exclude_list=exclude_list,
                ident_filter=ident_filter,
                update_rust=update_rust,
            )

    def apply_file(
        self,
        rust_source_file: Path,
        exclude_list: IdentifierExcludeList,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ) -> None:
        ident_regex = re.compile(ident_filter) if ident_filter else None

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

            highlighted_c_definition = get_highlighted_c(c_definition)
            logging.debug(
                f"C function {identifier} definition:\n{highlighted_c_definition}\n"
            )

            self.apply_ident(
                rust_source_file=rust_source_file,
                rust_definition=rust_definition,
                c_definition=c_definition,
                identifier=identifier,
                update_rust=update_rust,
            )


# TODO: We probably want a an interface that generates validators specialized to
#       each individual prompt so maybe this should take in some transform-
#       specific parameters and return a callable that only takes the LLM
#       response as input.
# class AbstractValidator(ABC):
#     @abstractmethod
#     def validate_response(self, response: str) -> str:
#         pass
