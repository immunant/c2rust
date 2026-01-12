from pathlib import Path

import yaml

from postprocess.utils import check_isinstance


class IdentifierExcludeList:
    src_path: Path | None
    paths: dict[Path, set[str]]

    def __init__(self, src_path: Path | None) -> None:
        self.src_path = src_path
        self.paths = {}
        if src_path is None:
            return
        data = yaml.safe_load(src_path.read_text())
        data = check_isinstance(data, dict)
        for path, identifiers in data.items():
            path = check_isinstance(path, str)
            identifiers = check_isinstance(identifiers, list)
            identifiers = [check_isinstance(ident, str) for ident in identifiers]
            path = Path(path)
            existing_identifiers = self.paths.get(path)
            if existing_identifiers is None:
                self.paths[path] = set(identifiers)
            else:
                existing_identifiers.update(*identifiers)

    def contains(self, path: Path, identifier: str) -> bool:
        # No `src_path` means an empty exclude list.
        if self.src_path is None:
            return False
        # Consider paths relative to `src_path`, the location of the exclude file.
        rel_path: Path = path.relative_to(self.src_path.parent)
        identifiers = self.paths.get(rel_path, set())
        return identifier in identifiers
