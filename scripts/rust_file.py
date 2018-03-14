import logging
import os

from enum import Enum
from common import get_cmd_or_die, NonZeroReturn
from plumbum.machines.local import LocalCommand
from typing import Iterable, List, Optional, Tuple

rustc = get_cmd_or_die("rustc")


# TODO: Support for custom visibility paths, if needed
class RustVisibility(Enum):
    Private = ""
    Public = "pub "
    Crate = "pub(crate) "


class CrateType(Enum):
    Binary = "bin"
    Library = "lib"


class RustFile:
    def __init__(self, path: str) -> None:
        self.path = path

    def compile(self, crate_type: CrateType, save_output: bool=False) -> Optional[LocalCommand]:
        current_dir, _ = os.path.split(self.path)
        extensionless_file, _ = os.path.splitext(self.path)

        # run rustc
        args = [
            "--crate-type={}".format(crate_type.value),
            "-L",
            current_dir,
        ]

        if save_output:
            args.append('-o')

            if crate_type == CrateType.Binary:
                args.append(extensionless_file)
            else:
                # REVIEW: Not sure if ext is correct
                args.append(extensionless_file + ".lib")

        args.append(self.path)

        # log the command in a format that's easy to re-run
        logging.debug("rustc compile command: %s", str(rustc[args]))

        retcode, stdout, stderr = rustc[args].run(retcode=None)

        logging.debug("stdout:\n%s", stdout)

        if retcode != 0:
            raise NonZeroReturn(stderr)

        if save_output:
            if crate_type == CrateType.Binary:
                return get_cmd_or_die(extensionless_file)
            # TODO: Support saving lib file

        return


class RustMod:
    def __init__(self, name: str, visibility: RustVisibility=None) -> None:
        self.name = name
        self.visibility = visibility or RustVisibility.Private

    def __str__(self) -> str:
        return "{}mod {};\n".format(self.visibility.value, self.name)

    def __hash__(self) -> int:
        return hash((self.visibility, self.name))

    def __eq__(self, other: "RustMod") -> bool:
        return self.name == other.name and self.visibility == other.visibility


class RustUse:
    def __init__(self, use: List[str], visibility: RustVisibility=None) -> str:
        self.use = "::".join(use)
        self.visibility = visibility or RustVisibility.Private

    def __str__(self) -> str:
        return "{}use {};\n".format(self.visibility.value, self.use)

    def __hash__(self) -> int:
        return hash((self.use, self.visibility))

    def __eq__(self, other: "RustUse") -> bool:
        return self.use == other.use and self.visibility == other.visibility


# TODO: Support params, lifetimes, generics, etc if needed
class RustFunction:
    def __init__(self, name: str, visibility: RustVisibility=None, body: List[str]=None) -> None:
        self.name = name
        self.visibility = visibility or RustVisibility.Private
        self.body = body or []

    def __str__(self) -> str:
        buffer = "{}fn {}() {{\n".format(self.visibility.value, self.name)

        for line in self.body:
            buffer += "    " + str(line)

        buffer += "}\n"

        return buffer


class RustMatch:
    def __init__(self, value: str, arms: List[Tuple[str, str]]) -> None:
        self.value = value
        self.arms = arms

    def __str__(self) -> str:
        buffer = "match {} {{\n".format(self.value)

        for left, right in self.arms:
            buffer += "        {} => {},\n".format(left, right)

        buffer += "    }\n"

        return buffer


class RustFileBuilder:
    def __init__(self) -> None:
        self.features = set()
        self.mods = set()
        self.uses = set()
        self.functions = []

    def __str__(self) -> str:
        buffer = ""

        for feature in self.features:
            buffer += "#![feature({})]\n".format(feature)

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

    def add_feature(self, feature: str) -> None:
        self.features.add(feature)

    def add_features(self, features: Iterable[str]) -> None:
        self.features.update(features)

    def add_mod(self, mod: RustMod) -> None:
        self.mods.add(mod)

    def add_mods(self, mods: Iterable[RustMod]) -> None:
        self.mods.update(mods)

    def add_use(self, use: RustUse) -> None:
        self.uses.add(use)

    def add_uses(self, uses: Iterable[RustUse]) -> None:
        self.uses.update(uses)

    def add_function(self, function: RustFunction) -> None:
        self.functions.append(function)

    def add_functions(self, functions: Iterable[RustFunction]) -> None:
        self.functions.extend(functions)

    def build(self, path: str) -> RustFile:
        with open(path, 'w') as fh:
            fh.write(str(self))

        return RustFile(path)
