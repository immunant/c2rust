import argparse
from dataclasses import dataclass
import json
import sys

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='automatically apply rustc-suggested fixes for compile errors')
    parser.add_argument('-n', '--dry-run', action='store_true',
        help="print fixes that would be applied, but don't modify any files")
    parser.add_argument('path', metavar='ERRORS.JSON',
        help='output of `rustc --error-format json`')
    return parser.parse_args()

@dataclass(frozen=True)
class Fix:
    file_path: str
    line_number: int
    start_byte: int
    end_byte: int
    new_text: str
    message: str

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

    fixes_by_file = {}
    for fix in fixes:
        file_fixes = fixes_by_file.get(fix.file_path)
        if file_fixes is None:
            fixes_by_file[fix.file_path] = [fix]
        else:
            file_fixes.append(fix)
    for file_fixes in fixes_by_file.values():
        file_fixes.sort(key=lambda fix: fix.start_byte)

    # Apply fixes
    for file_path, file_fixes in sorted(fixes_by_file.items()):
        content = open(file_path, 'rb').read()
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
