from enum import Enum
from typing import List


# TODO: Support for custom visibility paths, if needed
class RustVisibility(Enum):
    Private = ""
    Public = "pub "
    Crate = "pub(crate) "


class RustMod:
    def __init__(self, name: str, visibility: RustVisibility=None) -> None:
        self.name = name
        self.visibility = visibility or RustVisibility.Private

    def __str__(self) -> str:
        return f"{self.visibility.value}mod {self.name};\n"


class RustUse:
    def __init__(self, use: List[str], visibility: RustVisibility=None) -> str:
        self.use = "::".join(use)
        self.visibility = visibility or RustVisibility.Private

    def __str__(self) -> str:
        return f"{self.visibility.value}use {self.use};\n"


# TODO: Support params, lifetimes, generics, etc if needed
class RustFunction:
    def __init__(self, name: str, visibility: RustVisibility=None, body: List[str]=None) -> None:
        self.name = name
        self.visibility = visibility or RustVisibility.Private
        self.body = body or []

    def __str__(self) -> str:
        buffer = f"{self.visibility.value}fn {self.name}() {{\n"

        for line in self.body:
            buffer += "    " + line

        buffer += "}\n"

        return buffer

class RustFile:
    def __init__(self, features: List[str]=None, mods: List[RustMod]=None, uses: List[RustUse]=None,
                 functions: List[RustFunction]=None) -> None:
        self.features = features or []
        self.mods = mods or []
        self.uses = uses or []
        self.functions = functions or []

    def __str__(self) -> str:
        buffer = ""

        for feature in self.features:
            buffer += f"#![feature({feature})]\n"

        buffer += '\n'

        for mod in self.mods:
            buffer += str(mod)

        buffer += '\n'

        for use in self.uses:
            buffer += str(use)

        buffer += '\n'

        for function in self.functions:
            buffer += str(function)

        buffer += '\n'

        return buffer
