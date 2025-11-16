#!/usr/bin/env -S uv run

'''
Usage: `./rename_nll_facts.py src ref dest`

Renames atoms in `src/*.facts` to match the names used in `ref/*.facts`, then
writes the renamed facts to `dest/`.
'''

import ast
from collections import defaultdict
import os
import sys

src_dir, ref_dir, dest_dir = sys.argv[1:]

# Map `src` loan/origin/path names to `ref` loan/origin/path names.  We don't
# break this down by type because the names for each type don't collide anyway.
name_map = {}
# Set of `ref` names that appear as values in `name_map`.
ref_names_seen = set()

def match_name(src_name, ref_name):
    if src_name in name_map:
        old_ref_name = name_map[src_name]
        if ref_name != old_ref_name:
            print('error: %r matches both %r and %r' % (
                src_name, old_ref_name, ref_name))
            return
    else:
        if ref_name in ref_names_seen:
            print('error: %r matches %r, but %r is already used' % (
                src_name, ref_name, ref_name))
            return
        name_map[src_name] = ref_name
        ref_names_seen.add(ref_name)

def match_loan(src_name, ref_name):
    match_name(src_name, ref_name)

def match_origin(src_name, ref_name):
    match_name(src_name, ref_name)

def match_path(src_name, ref_name):
    match_name(src_name, ref_name)


def load(name):
    with open(os.path.join(src_dir, name + '.facts')) as f:
        src_rows = [[ast.literal_eval(s) for s in line.strip().split('\t')]
                for line in f]
    with open(os.path.join(ref_dir, name + '.facts')) as f:
        ref_rows = [[ast.literal_eval(s) for s in line.strip().split('\t')]
                for line in f]
    return src_rows, ref_rows


# Match up paths using `path_is_var` and `path_assigned_at_base`.

def match_path_is_var():
    src, ref = load('path_is_var')
    ref_dct = {var: path for path, var in ref}
    for path, var in src:
        if var not in ref_dct:
            continue
        match_path(path, ref_dct[var])

match_path_is_var()

def match_path_assigned_at_base():
    src, ref = load('path_assigned_at_base')
    ref_dct = {point: path for path, point in ref}
    for path, point in src:
        if point not in ref_dct:
            continue
        match_path(path, ref_dct[point])

match_path_assigned_at_base()

# Match up origins and loans using `loan_issued_at`

def match_loan_issued_at():
    src, ref = load('loan_issued_at')
    ref_dct = {point: (origin, loan) for origin, loan, point in ref}
    for origin, loan, point in src:
        if point not in ref_dct:
            continue
        match_origin(origin, ref_dct[point][0])
        match_origin(loan, ref_dct[point][1])

match_loan_issued_at()

# Match up origins using `use_of_var_derefs_origin`

def match_use_of_var_derefs_origin():
    src, ref = load('use_of_var_derefs_origin')
    src_dct = defaultdict(list)
    for var, origin in src:
        src_dct[var].append(origin)
    ref_dct = defaultdict(list)
    for var, origin in ref:
        ref_dct[var].append(origin)
    for var in set(src_dct.keys()) & set(ref_dct.keys()):
        src_origins = src_dct[var]
        ref_origins = ref_dct[var]
        if len(src_origins) != len(ref_origins):
            print('error: var %r has %d origins in src but %d in ref' % (
                var, len(src_origins), len(ref_origins)))
            continue
        for src_origin, ref_origin in zip(src_origins, ref_origins):
            match_origin(src_origin, ref_origin)

match_use_of_var_derefs_origin()


# Rewrite `src` using the collected name mappings.

os.makedirs(dest_dir, exist_ok=True)
for name in os.listdir(src_dir):
    if name.startswith('.') or not name.endswith('.facts'):
        continue

    with open(os.path.join(src_dir, name)) as src, \
            open(os.path.join(dest_dir, name), 'w') as dest:
        for line in src:
            src_parts = [ast.literal_eval(s) for s in line.strip().split('\t')]
            dest_parts = []
            for part in src_parts:
                if part.startswith('_') or part.startswith('Start') or part.startswith('Mid'):
                    dest_parts.append(part)
                    continue

                dest_part = name_map.get(part)
                if dest_part is None:
                    print('error: no mapping for %r (used in %s: %r)' % (
                        part, name, src_parts))
                    dest_part = 'OLD:' + part
                dest_parts.append(dest_part)

            dest.write('\t'.join('"%s"' % part for part in dest_parts) + '\n')

