#!/usr/bin/env -S uv run
# -*- coding: utf-8 -*-

import os.path
from typing import List

from common import (
    get_cmd_or_die,
)


EXTENSIONS = ['*.md', '*.png', '*.html', '*.css']


def list_files(untracked: bool = False) -> List[str]:
    git = get_cmd_or_die('git')
    args = ['ls-files', '--cached', '--exclude-standard']
    if untracked:
        args.append('--others')
    args.extend(EXTENSIONS)
    files = git(args).splitlines()
    return [f for f in files if not os.path.islink(f)]


def _main() -> None:
    candidates = list_files()
    for f in candidates:
        # skip files in manual/
        (head, tail) = os.path.split(f)
        while head != '':
            (head, tail) = os.path.split(head)
        if tail == 'manual':
            continue

        symlink_path = os.path.join('manual', f)
        symlink_target = os.path.relpath(f, os.path.dirname(symlink_path))
        if not os.path.islink(symlink_path):
            if os.path.exists(symlink_path):
                print('Warning: File already exists, not overwriting: ', symlink_path)
                continue

            print('Creating symlink: {} -> {}'.format(symlink_path, f))
            os.makedirs(os.path.dirname(symlink_path), exist_ok=True)
            os.symlink(symlink_target, symlink_path)
        

if __name__ == '__main__':
    _main()
