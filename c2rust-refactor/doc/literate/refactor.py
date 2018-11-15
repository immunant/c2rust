'''Refactor command invocations.  Turns `Script` blocks from the input into
`ScriptDiff` blocks that include before-and-after file contents.'''
from collections import namedtuple
import os
import sys

from plumbum import local, FG
from plumbum.cmd import cargo
from common import *

from literate import parse


def combine_script_blocks(blocks):
    '''Combine all refactoring commands into a single list, inserting `write`
    commands to record intermediate states.  Returns the combined command list,
    along with a dict mapping each script block's index in the input to the
    index where its output will appear (for use in finding
    "rewrite.N.json").'''
    script_output = {}
    last_output = -1
    all_cmds = []

    def emit(cmd):
        nonlocal last_output
        if cmd[0] in ('write', 'commit'):
            last_output += 1
        all_cmds.append(cmd)

    for i, b in enumerate(blocks):
        if isinstance(b, parse.Text):
            pass
        elif isinstance(b, parse.Script):
            for c in b.commands:
                emit(c)

            if len(b.commands) == 0 or b.commands[-1][0] not in ('write', 'commit'):
                emit(['write'])
            assert last_output != -1
            script_output[i] = last_output

        else:
            raise TypeError('expected Text or Script, got %s' % (type(b),))

    return all_cmds, script_output

def run_refactor(work_dir, cmds, mode='json,alongside'):
    refactor = get_cmd_or_die(config.RREF_BIN)

    args = ['-r', mode, '--cargo']
    for cmd in cmds:
        args.extend(cmd)
        args.append(';')

    ld_lib_path = get_rust_toolchain_libpath()

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in local.env:
        ld_lib_path += ':' + local.env['LD_LIBRARY_PATH']

    with local.env(RUST_BACKTRACE='1',
                   LD_LIBRARY_PATH=ld_lib_path):
        with local.cwd(work_dir):
            print('running %s in %s with %d cmds...' % (refactor, work_dir, len(cmds)))
            refactor[args]()
            print('  refactoring done')


def subspan_src(span, lo, hi):
    assert span['lo'] <= lo
    assert hi <= span['hi']
    start = span['lo']
    return span['src'][lo - start : hi - start]

def apply_rewrites(span, rws):
    '''Given a span and a forest of rewrites to apply to that span, return the
    rewritten text for that span.'''
    rws.sort(key=lambda rw: rw['old_span']['lo'])
    parts = []
    pos = span['lo']
    for rw in rws:
        if pos < rw['old_span']['lo']:
            parts.append(subspan_src(span, pos, rw['old_span']['lo']))

        if rw['adjust'] == 'parenthesize':
            parts.append('(')

        parts.append(apply_rewrites(rw['new_span'], rw['rewrites']))

        if rw['adjust'] == 'parenthesize':
            parts.append(')')

        pos = rw['old_span']['hi']
    if pos < span['hi']:
        parts.append(subspan_src(span, pos, span['hi']))

    return ''.join(parts)


Text = parse.Text
ScriptDiff = namedtuple('ScriptDiff', ('commands', 'raw', 'files'))

def run_refactor_scripts(args, blocks):
    '''Run all refactoring commands in `blocks`, returning a new list of blocks
    where each `Script` is replaced by a `ScriptDiff`, which includes the old
    and new text of each file modified by the commands.'''
    all_cmds, script_output = combine_script_blocks(blocks)
    run_refactor(args.project_dir, all_cmds)

    result = []
    last_text = {}
    for i, b in enumerate(blocks):
        rw_idx = script_output.get(i)
        if rw_idx is None:
            assert isinstance(b, parse.Text)
            result.append(b)
            continue

        assert isinstance(b, parse.Script)
        with open(os.path.join(args.project_dir, 'rewrites.%d.json' % rw_idx)) as f:
            rws = json.load(f)

        files = {}
        for rw in rws:
            path = rw['new_span']['file']
            if path not in last_text:
                old_text = rw['new_span']['src']
            else:
                old_text = last_text[path]
            new_text = apply_rewrites(rw['new_span'], rw['rewrites'])
            files[path] = (old_text, new_text)
            last_text[path] = new_text
        result.append(ScriptDiff(b.commands, b.raw, files))

    return result
