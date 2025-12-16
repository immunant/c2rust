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
        self, root_rust_source_file: Path, ident_filter: str | None = None
    ) -> None:
        pass


# TODO: We probably want a an interface that generates validators specialized to
#       each individual prompt so maybe this should take in some transform-
#       specific parameters and return a callable that only takes the LLM
# .      response as input.
# class AbstractValidator(ABC):
#     @abstractmethod
#     def validate_response(self, response: str) -> str:
#         pass
