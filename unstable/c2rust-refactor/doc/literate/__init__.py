import argparse
import json
import os
import shlex
import shutil
import sys
from typing import List, Dict, Any

sys.path.append(os.path.join(os.path.dirname(__file__), '../../../scripts'))
from common import config

import literate.format
import literate.parse
import literate.refactor
import literate.render


def build_arg_parser() -> argparse.ArgumentParser:
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument('--project-dir', default='.',
            help='path to the project directory')
    config.add_args(common)

    ap = argparse.ArgumentParser(
            description='Process literate refactoring scripts.')

    subparsers = ap.add_subparsers(dest='cmd')

    sp = subparsers.add_parser('extract',
            help='extract refactoring script from a Markdown file and print it',
            parents=[common])
    sp.add_argument('input', metavar='INPUT.md')

    sp = subparsers.add_parser('exec',
            help='extract refactoring script and run it on the project directory',
            parents=[common])
    sp.add_argument('input', metavar='INPUT.md')
    sp.add_argument('--work-dir',
            help='copy the project into a work directory before refactoring'),
    sp.add_argument('-f', '--force', default=False, action='store_true',
            help='remove the work directory if it already exists'),

    sp = subparsers.add_parser('render',
            help='generate rendered Markdown, including a diff for every '
                'refactoring step',
            parents=[common])
    sp.add_argument('--playground-js',
            help='if set, the generated markdown will include this javascript '
            'URL and call `initRefactorPlaygroundButtons` to set up '
            'playground integration')
    sp.add_argument('input', metavar='INPUT.md')
    sp.add_argument('output', metavar='OUTPUT.md')

    sp = subparsers.add_parser('playground',
            help='run a refactoring script on some code, and render a diff',
            parents=[common])
    sp.add_argument('code', metavar='CODE.rs')
    sp.add_argument('script', metavar='SCRIPT.txt')
    sp.add_argument('output', metavar='OUTPUT.html')

    sp = subparsers.add_parser('playground-styles',
            help='print CSS styles for rendering playground diffs')

    return ap

def do_extract(args: argparse.Namespace):
    with open(args.input) as f:
        blocks = literate.parse.parse_blocks(f)

    for b in blocks:
        if not isinstance(b, literate.parse.Script):
            continue

        for l in b.lines:
            sys.stdout.write(l)
            if l.strip() == 'commit ;':
                sys.stdout.write('\n')

def do_exec(args: argparse.Namespace):
    with open(args.input) as f:
        blocks = literate.parse.parse_blocks(f)

    if args.work_dir is not None:
        if os.path.exists(args.work_dir):
            if args.force:
                print('removing old work dir `%s`' % args.work_dir)
                shutil.rmtree(args.work_dir)
            else:
                print('error: work directory `%s` already exists' % args.work_dir)
                sys.exit(1)
        print('copy project `%s` to work dir `%s`' % (args.project_dir, args.work_dir))
        shutil.copytree(args.project_dir, args.work_dir)
        work_dir = args.work_dir
    else:
        work_dir = args.project_dir

    literate.refactor.exec_refactor_scripts(args, blocks, work_dir)

def build_result_json(blocks: List[literate.refactor.Block]) -> Dict[str, Any]:
    code = []
    script = []
    results = []

    script_acc = []

    for b in blocks:
        if not isinstance(b, literate.refactor.RefactorCode):
            continue

        if b.parsed_old:
            if len(b.old) == 1:
                f = next(iter(b.old.values()))
                code.append(f.text)
            else:
                code.append(None)

            script_acc = []

        if len(script_acc) > 0:
            # If there are previous commands in this block, make sure they end with
            # a semicolon
            words = shlex.split('\n'.join(script_acc))
            if len(words) > 0 and words[-1] != ';':
                for i in reversed(range(len(script_acc))):
                    if script_acc[i].strip() != '':
                        script_acc[i] = script_acc[i].rstrip('\n') + ' ;\n'
                        break

            # Add a blank line between blocks
            script_acc.append('\n')

        script_acc.extend(b.lines)
        script.append(''.join(script_acc))

        results.append({
            'code_idx': len(code) - 1,
            'script_idx': len(script) - 1,
        })

    return {
            'code': code,
            'script': script,
            'results': results,
            }

def do_render(args: argparse.Namespace):
    with open(args.input) as f:
        blocks = literate.parse.parse_blocks(f)
    blocks, all_files = literate.refactor.run_refactor_scripts(args, blocks)
    literate.format.format_files(all_files)

    literate.render.prepare_files(all_files)

    with open(args.output, 'w') as f:
        f.write('<style>\n')
        f.write(literate.render.get_styles())
        f.write('</style>\n\n')

        diff_idx = 0
        for b in blocks:
            if isinstance(b, literate.refactor.Text):
                for line in b.lines:
                    f.write(line)
            elif isinstance(b, literate.refactor.Code):
                f.write('```%s\n' % ' '.join(b.attrs))
                for line in b.lines:
                    f.write(line)
                f.write('```\n')
            elif isinstance(b, literate.refactor.RefactorCode):
                if not b.opts['hide-code']:
                    f.write('```sh %s\n' % ' '.join(b.attrs[1:]))
                    for line in b.lines:
                        f.write(line)
                    f.write('```\n\n')
                    # Unfortunately the `pulldown-cmark` package used by `mdbook`
                    # provides no way to set the `id` of a code block.  Instead we
                    # use this hack: we place an empty, invisible <a> tag with an
                    # `id` just after the block, and in the Javascript code we use
                    # `document.getElementById(...).previousElementSibling` to get
                    # the actual code block.
                    f.write('<a id="refactor-anchor-%d" style="display: none"></a>\n' % diff_idx)

                f.write('\n\n')

                print('rendering diff #%d' % (diff_idx + 1))
                print('  diff options: %s' % (b.opts,))
                diff_text = literate.render.render_diff(b.old, b.new, b.opts)
                if diff_text is not None:
                    collapse = b.opts['collapse-diff']
                    if collapse:
                        f.write('<details><summary>Diff #%d</summary>\n' % (diff_idx + 1))
                    f.write(diff_text)
                    if collapse:
                        f.write('\n<hr></details>\n\n')
                diff_idx += 1
            else:
                raise TypeError('expected Text or ScriptDiff, got %s' % (type(b),))

        if args.playground_js is not None:
            j = build_result_json(blocks)
            f.write('<script>var C2RUST_REFACTOR_JSON = %s;</script>\n' %
                    json.dumps(j, indent=2))
            f.write('<script src="%s"></script>' % args.playground_js)
            f.write('<script>initRefactorPlaygroundButtons();</script>')

def do_playground(args: argparse.Namespace):
    # Stupid hack here, because Rust `Process` doesn't support merging stdout
    # and stderr.
    sys.stderr = None
    os.close(2)
    os.dup2(1, 2)
    sys.stderr = sys.stdout

    with open(args.script) as f:
        script = f.read()

    result, all_files = literate.refactor.run_refactor_for_playground(
            args, script)
    old = result.old
    new = result.new

    literate.format.format_files(all_files)
    literate.render.prepare_files(all_files)

    opts = literate.refactor.OPT_DEFAULTS.copy()
    opts['show-filename'] = False
    opts['highlight-mode'] = 'ace'

    diff_text = literate.render.render_diff(old, new, opts)
    with open(args.output, 'w') as f:
        f.write(diff_text)

def do_playground_styles(args: argparse.Namespace):
    print(literate.render.get_styles())
    print(literate.render.get_pygments_styles())

def main(argv: List[str]):
    ap = build_arg_parser()
    args = ap.parse_args(argv)
    config.update_args(args)

    if args.cmd == 'extract':
        do_extract(args)
    elif args.cmd == 'exec':
        do_exec(args)
    elif args.cmd == 'render':
        do_render(args)
    elif args.cmd == 'playground':
        do_playground(args)
    elif args.cmd == 'playground-styles':
        do_playground_styles(args)
    else:
        if args.cmd is not None:
            print('unknown subcommand `%s`' % args.cmd)
        ap.print_usage()
        sys.exit(1)
