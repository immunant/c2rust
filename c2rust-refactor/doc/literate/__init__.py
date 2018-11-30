import argparse
import os
import shutil
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), '../../../scripts'))
from common import config

import literate.format
import literate.parse
import literate.refactor
import literate.render


def build_arg_parser():
    ap = argparse.ArgumentParser(
            description='Process literate refactoring scripts.')

    ap.add_argument('--project-dir', default='.',
            help='path to the project directory')

    subparsers = ap.add_subparsers(dest='cmd')

    sp = subparsers.add_parser('extract',
            help='extract refactoring script from a Markdown file and print it')
    sp.add_argument('input', metavar='INPUT.md')

    sp = subparsers.add_parser('exec',
            help='extract refactoring script and run it on the project directory')
    sp.add_argument('input', metavar='INPUT.md')
    sp.add_argument('--work-dir',
            help='copy the project into a work directory before refactoring'),
    sp.add_argument('-f', '--force', default=False, action='store_true',
            help='remove the work directory if it already exists'),
    sp.add_argument('-r', '--rewrite-mode', default='inplace',
            help='rewrite mode for the refactoring process'),

    sp = subparsers.add_parser('render',
            help='generate rendered Markdown, including a diff for every '
                'refactoring step')
    sp.add_argument('input', metavar='INPUT.md')
    sp.add_argument('output', metavar='OUTPUT.md')

    return ap

def do_extract(args):
    with open(args.input) as f:
        blocks = literate.parse.parse_blocks(f)

    for b in blocks:
        if not isinstance(b, literate.parse.Script):
            continue

        for l in b.lines:
            sys.stdout.write(l)
            if l.strip() == 'commit ;':
                sys.stdout.write('\n')

def do_exec(args):
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

def do_render(args):
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
                if 'hidden' not in b.attrs:
                    remove_attrs = {'refactor-target', 'refactor-config'}
                    filtered_attrs = [a for a in b.attrs if a not in remove_attrs]

                    f.write('```%s\n' % ' '.join(filtered_attrs))
                    for line in b.lines:
                        f.write(line)
                    f.write('```\n')
            elif isinstance(b, literate.refactor.RefactorCode):
                if 'hidden' in b.attrs:
                    continue

                f.write('```sh\n')
                for line in b.lines:
                    f.write(line)
                f.write('```\n\n')

                print('rendering diff #%d' % (diff_idx + 1))
                print('  diff options: %s' % (b.opts,))
                diff_text = literate.render.render_diff(b.old, b.new, b.opts)
                if diff_text is not None:
                    f.write('<details><summary>Diff #%d</summary>\n' % (diff_idx + 1))
                    f.write(diff_text)
                    f.write('\n<hr></details>\n\n')
                diff_idx += 1
            else:
                raise TypeError('expected Text or ScriptDiff, got %s' % (type(b),))

def main(argv):
    ap = build_arg_parser()
    config.add_args(ap)
    args = ap.parse_args(argv)
    config.update_args(args)

    if args.cmd == 'extract':
        do_extract(args)
    elif args.cmd == 'exec':
        do_exec(args)
    elif args.cmd == 'render':
        do_render(args)
    else:
        if args.cmd is not None:
            print('unknown subcommand `%s`' % args.cmd)
        ap.print_usage()
        sys.exit(1)
