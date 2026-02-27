import logging
from pathlib import Path
from textwrap import dedent

from postprocess.cache import AbstractCache
from postprocess.definitions import get_c_comments
from postprocess.models import AbstractGenerativeModel
from postprocess.transforms.base import AbstractTransform
from postprocess.utils import remove_backticks

SYSTEM_INSTRUCTION = (
    "You are a helpful assistant that removes top-of-file prologues and"
    " other unnecessary content such as preprocessor definitions and"
    " comments that preceede and are unrelated to the definition of a C function."
)


class TrimTransform(AbstractTransform):
    def __init__(self, cache: AbstractCache, model: AbstractGenerativeModel):
        super().__init__(SYSTEM_INSTRUCTION)
        self.cache = cache
        self.model = model

    def apply_item(
        self,
        rust_source_file: Path,
        rust_definition: str,
        c_definition: str,
        identifier: str,
        update_rust: bool = True,
    ) -> str | None:
        c_comments = get_c_comments(c_definition)
        if not c_comments:
            logging.info(
                f"{self.__class__.__name__}: "
                f"Skipping C function without comments: {identifier}"
            )
            return

        prompt = """
        Remove any prologues, preprocessor definitions, and comments that are unrelated to the definition
        of the C function `{identifier}`. Respond with the trimmed C function definition; say nothing else.

        C function:
        ```c
        {c_definition}
        ```
        """  # noqa: E501
        prompt = dedent(
            prompt
        ).strip()  # note: dedent then format since the C definition isn't indented
        prompt = prompt.format(identifier=identifier, c_definition=c_definition)

        messages = [
            {"role": "user", "content": prompt},
        ]

        transform = self.__class__.__name__
        model = self.model.id
        if response := self.cache.lookup(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
        ):
            return response

        response = self.model.generate_with_tools(messages)

        if response is None:
            logging.error("Model returned no response")
            return response

        # TODO: validate that function definition is still present?

        response = remove_backticks(response)

        self.cache.update(
            transform=transform,
            identifier=identifier,
            model=model,
            messages=messages,
            response=response,
        )

        return response
