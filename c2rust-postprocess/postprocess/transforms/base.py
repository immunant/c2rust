from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any


class AbstractTransform(ABC):
    """
    Abstract base class for LLM-driven transforms of c2rust transpiler output.
    """

    def __init__(self, system_instruction: str, **kwargs: Any):
        self._system_instruction = system_instruction

    @property
    def system_instruction(self) -> str:
        return self._system_instruction

    # TODO: update signature after factoring out common functionality such
    #       as matching up definitions, filtering out definitions, etc.
    @abstractmethod
    def apply(
        self,
        rust_source_file: Path,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ) -> None:
        pass

    def apply_dir(
        self,
        root_rust_source_file: Path,
        ident_filter: str | None = None,
        update_rust: bool = True,
    ):
        """
        Run `self.apply` on each `*.rs` in `dir`
        with a corresponding `*.c_decls.json`.
        """
        root_dir = root_rust_source_file.parent
        c_decls_json_suffix = ".c_decls.json"
        for c_decls_path in root_dir.glob(f"**/*{c_decls_json_suffix}"):
            rs_path = c_decls_path.with_name(
                c_decls_path.name.removesuffix(c_decls_json_suffix) + ".rs"
            )
            assert rs_path.exists()
            self.apply(
                rust_source_file=rs_path,
                ident_filter=ident_filter,
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
