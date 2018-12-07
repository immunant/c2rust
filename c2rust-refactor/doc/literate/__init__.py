import argparse
import os
import shutil
import sys
from typing import List

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
            description='Process literate refactoring scripts.',
            parents=[common])

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
    sp.add_argument('-r', '--rewrite-mode', default='inplace',
            help='rewrite mode for the refactoring process'),

    sp = subparsers.add_parser('render',
            help='generate rendered Markdown, including a diff for every '
                'refactoring step',
            parents=[common])
    sp.add_argument('input', metavar='INPUT.md')
    sp.add_argument('output', metavar='OUTPUT.md')

    sp = subparsers.add_parser('playground',
            help='run a refactoring script on some code, and render a diff')
    sp.add_argument('code', metavar='CODE.rs')
    sp.add_argument('script', metavar='SCRIPT.txt')
    sp.add_argument('output', metavar='OUTPUT.html')

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

    cmds, _ = literate.refactor.combine_script_blocks(blocks)

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

    literate.refactor.run_refactor(work_dir, cmds, mode=args.rewrite_mode)

def do_render(args: argparse.Namespace):
    with open(args.input) as f:
        blocks = literate.parse.parse_blocks(f)
    blocks, all_files = literate.refactor.run_refactor_scripts(args, blocks)
    literate.format.format_files(all_files)

    literate.render.prepare_files(all_files)

    with open(args.output, 'w') as f:
        f.write('<style>')
        f.write(literate.render.get_styles())
        f.write('</style>')

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
                f.write('```sh %s\n' % ' '.join(b.attrs[1:]))
                for line in b.lines:
                    f.write(line)
                f.write('```\n\n')

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

def do_playground(args: argparse.Namespace):
    # Stupid hack here, because Rust `Process` doesn't support merging stdout
    # and stderr.
    sys.stderr = None
    os.close(2)
    os.dup2(1, 2)
    sys.stderr = sys.stdout

    with open(args.script) as f:
        script = f.read()

    (old, new), all_files = literate.refactor.run_refactor_for_playground(
            args, script)

    literate.format.format_files(all_files)
    literate.render.prepare_files(all_files)

    opts = literate.refactor.OPT_DEFAULTS.copy()
    opts['show-filename'] = False

    diff_text = literate.render.render_diff(old, new, opts)
    with open(args.output, 'w') as f:
        f.write(diff_text)


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
    else:
        if args.cmd is not None:
            print('unknown subcommand `%s`' % args.cmd)
        ap.print_usage()
        sys.exit(1)
