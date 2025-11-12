#!/usr/bin/env -S uv run

import argparse
from collections import defaultdict, namedtuple
import json
import re
import sys

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='automatically apply rustc-suggested fixes for compile errors')
    parser.add_argument('-n', '--dry-run', action='store_true',
        help="print fixes that would be applied, but don't modify any files")
    parser.add_argument('path', metavar='ERRORS.JSON',
        help='output of `rustc --error-format json`')
    return parser.parse_args()

Fix = namedtuple('Fix', (
    # Path to the file to modify.
    'file_path',
    # Line number where the fix will be applied.  This is used for debug output
    # only.
    'line_number',
    # Start of the byte range to replace.
    'start_byte',
    # End (exclusive) of the byte range to replace.
    'end_byte',
    # Replacement text (as a `str`, not `bytes`).
    'new_text',
    # The original error message.  This is used for debug output only.
    'message',
))

LifetimeBound = namedtuple('LifetimeBound', (
    'file_path',
    'line_number',
    # Byte offset of the start of the lifetime parameter declaration.
    'start_byte',
    # Byte offset of the end of the lifetime parameter declaration.
    'end_byte',
    # The lifetime to use in the new bound.  If `'a: 'b` is the suggested
    # bound, then `start/end_byte` points to the declaration of `'a`, and
    # `bound_lifetime` is the string `"'b"`.
    'bound_lifetime',
))

RemoveDeriveCopy = namedtuple('RemoveDeriveCopy', (
    'file_path',
    'line_number',
    # Byte offset of the start of the token `Copy`.
    'start_byte',
    # Byte offset of the end of the token `Copy`.
    'end_byte',
))

MSG_LIFETIME_BOUND = 'lifetime may not live long enough'
MSG_DERIVE_COPY = 'the trait `Copy` may not be implemented for this type'

LIFETIME_DEFINED_RE = re.compile(r'^lifetime `([^`]*)` defined here$')
CONSIDER_ADDING_BOUND_RE = re.compile(r'^consider adding the following bound: `([^`:]*): ([^`]*)`$')
SPACE_COLON_RE = re.compile(rb'\s*:')
SPACE_COMMA_RE = re.compile(rb'\s*,')

def main():
    args = parse_args()

    fixes = []
    def gather_fixes(j, message):
        for span in j['spans']:
            if span.get('suggestion_applicability') != 'MachineApplicable':
                continue
            fix = Fix(
                file_path=span['file_name'],
                line_number=span['line_start'],
                start_byte=span['byte_start'],
                end_byte=span['byte_end'],
                new_text=span['suggested_replacement'],
                message=message,
            )
            fixes.append(fix)

        for child in j['children']:
            gather_fixes(child, message)

    lifetime_bounds = []
    def gather_lifetime_bounds(j):
        # We look for a particular pattern seen in lifetime errors.  First,
        # there should be a span with label "lifetime 'a defined here" pointing
        # at the declaration of `'a`.  Second, there should be a child of type
        # `help` with the text "consider adding the following bound: `'a: 'b`".

        decl_spans = {}
        for span in j['spans']:
            m = LIFETIME_DEFINED_RE.match(span['label'])
            if m is not None:
                lifetime = m.group(1)
                if lifetime in decl_spans:
                    # Duplicate declaration for this lifetime.  This shouldn't
                    # happen, but we can proceed as long as the lifetime isn't
                    # the target of the bound.  We mark the duplicate lifetime
                    # so it can't be used as the target.
                    decl_spans[lifetime] = None
                    continue
                decl_spans[lifetime] = span

        for child in j['children']:
            if child['level'] != 'help':
                continue
            m = CONSIDER_ADDING_BOUND_RE.match(child['message'])
            if m is None:
                continue
            lifetime_a = m.group(1)
            lifetime_b = m.group(2)
            span = decl_spans.get(lifetime_a)
            if span is None:
                # We don't have anywhere to insert the new bound.  This can
                # also happen if there were duplicate declaration spans for
                # this lifetime (we explicitly insert `None` into the map in
                # that case).
                continue
            lifetime_bounds.append(LifetimeBound(
                file_path=span['file_name'],
                line_number=span['line_start'],
                start_byte=span['byte_start'],
                end_byte=span['byte_end'],
                bound_lifetime=lifetime_b,
            ))

    remove_derive_copies = []
    def handle_derive_copy(j):
        # Find the "primary span", which covers the `Copy` token.
        primary_span = None
        for span in j['spans']:
            if span.get('is_primary'):
                primary_span = span
                break

        if primary_span is None:
            return

        remove_derive_copies.append(RemoveDeriveCopy(
            file_path=primary_span['file_name'],
            line_number=primary_span['line_start'],
            start_byte=primary_span['byte_start'],
            end_byte=primary_span['byte_end'],
        ))

    with open(args.path, 'r') as f:
        for line in f:
            j = json.loads(line)

            # Only process errors, not warnings.
            level = j['level']
            if level == 'error':
                pass
            elif level in ('warning', 'failure-note'):
                continue
            else:
                # `help`, `note`, etc should not appear at top level.
                assert False, 'unexpected `level` %r' % (level,)

            gather_fixes(j, j['message'])

            if j['message'] == MSG_LIFETIME_BOUND:
                gather_lifetime_bounds(j)
            elif j['message'] == MSG_DERIVE_COPY:
                handle_derive_copy(j)

    # Convert suggested lifetime bounds to fixes.  We have to group the bounds
    # first because there may be multiple suggested bounds for a single
    # declaration, in which case we want to generate a single `Fix` that adds
    # all of them at once.

    # Maps the `(file_path, line_number, start_byte, end_byte)` of the
    # declaration site to the set of new lifetimes to apply at that site.
    grouped_lifetime_bounds = {}
    for lb in lifetime_bounds:
        key = (lb.file_path, lb.line_number, lb.start_byte, lb.end_byte)
        if key not in grouped_lifetime_bounds:
            grouped_lifetime_bounds[key] = set()
        grouped_lifetime_bounds[key].add(lb.bound_lifetime)

    file_content = {}
    def read_file(file_path):
        if file_path not in file_content:
            file_content[file_path] = open(file_path, 'rb').read()
        return file_content[file_path]

    for key, bound_lifetimes in sorted(grouped_lifetime_bounds.items()):
        (file_path, line_number, start_byte, end_byte) = key
        content = read_file(file_path)
        decl_lifetime = content[start_byte : end_byte].decode('utf-8')
        bound_lifetimes = ' + '.join(bound_lifetimes)
        m = SPACE_COLON_RE.match(content, end_byte)
        if m is None:
            fix_end_byte = end_byte
            fix_new_text = '%s: %s' % (decl_lifetime, bound_lifetimes)
        else:
            space_colon = m.group().decode('utf-8')
            fix_end_byte = m.end()
            fix_new_text = '%s%s %s +' % (decl_lifetime, space_colon, bound_lifetimes)
        fixes.append(Fix(
            file_path=file_path,
            line_number=line_number,
            start_byte=start_byte,
            end_byte=fix_end_byte,
            new_text=fix_new_text,
            message=MSG_LIFETIME_BOUND,
        ))

    # Convert errors about `#[derive(Copy)]` to fixes.
    for rm in remove_derive_copies:
        # The error span points to the `Copy` token, but we actually want to
        # remove both `Copy` and the following comma, if one is present.
        #
        #       #[derive(Foo, Copy, Bar)]  ->  #[derive(Foo, Bar)]
        #       #[derive(Foo, Copy)]       ->  #[derive(Foo,)]
        #       #[derive(Copy, Bar)]       ->  #[derive(Bar)]
        #       #[derive(Copy)]            ->  #[derive()]
        #
        # Note that `#[derive()]` is legal, though ideally it should be removed
        # by a later cleanup pass.
        content = read_file(rm.file_path)
        m = SPACE_COMMA_RE.match(content, rm.end_byte)
        fix_end_byte = m.end() if m is not None else rm.end_byte
        fixes.append(Fix(
            file_path=rm.file_path,
            line_number=rm.line_number,
            start_byte=rm.start_byte,
            end_byte=fix_end_byte,
            new_text='',
            message=MSG_DERIVE_COPY,
        ))

    # Group fixes by file
    fixes_by_file = defaultdict(list)
    for fix in fixes:
        fixes_by_file[fix.file_path].append(fix)
    fixes_by_file = dict(fixes_by_file)
    for file_fixes in fixes_by_file.values():
        file_fixes.sort(key=lambda fix: fix.start_byte)

    # Apply fixes
    for file_path, file_fixes in sorted(fixes_by_file.items()):
        content = read_file(file_path)
        chunks = []
        pos = 0
        prev_fix = None
        for fix in file_fixes:
            old_text = content[fix.start_byte : fix.end_byte].decode('utf-8')
            desc = '%s:%d: %r -> %r (%s)' % (
                file_path, fix.line_number, old_text, fix.new_text, fix.message)
            if prev_fix is not None:
                if fix.start_byte < prev_fix.end_byte:
                    if fix.start_byte == prev_fix.start_byte \
                            and fix.end_byte == prev_fix.end_byte \
                            and fix.new_text == prev_fix.new_text:
                        # `fix` and `prev_fix` suggest the same change, so we
                        # don't need to apply `fix`.
                        continue
                    # We want to apply fix, but can't because it overlaps with
                    # `prev_fix`.
                    print('skipping due to overlap: %s' % desc)
                    continue

            prev_fix = fix

            print(desc)
            if fix.start_byte > pos:
                chunks.append(content[pos : fix.start_byte])
            chunks.append(fix.new_text.encode('utf-8'))
            pos = fix.end_byte

        if pos < len(content):
            chunks.append(content[pos:])

        new_content = b''.join(chunks)
        if not args.dry_run:
            open(file_path, 'wb').write(new_content)
            print('wrote to %r' % file_path)
        else:
            print('would write to %r' % file_path)

if __name__ == '__main__':
    main()
