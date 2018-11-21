'''Refactor command invocations.  Turns `Script` blocks from the input into
`ScriptDiff` blocks that include before-and-after file contents.'''
from collections import namedtuple
import os
import sys

from plumbum import local, FG
from plumbum.cmd import cargo
from common import *

from literate import parse
#import literate.marks


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

def run_refactor(work_dir, cmds, mode='json,marks,alongside'):
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
            #refactor[args]()
            print('  refactoring done')


def subspan_src(span, lo, hi):
    assert span['lo'] <= lo
    assert hi <= span['hi']
    start = span['lo']
    return span['src'][lo - start : hi - start]

def apply_rewrites(span, rws, nodes):
    '''Given a span and a forest of rewrites to apply to that span, return the
    rewritten text for that span, along with a list of node spans in the output
    text.'''

    # Design:
    #
    # `nodes` maps NodeIds to certain subspans of `span`.  We want to instead
    # map NodeIds to subspans of the output text.  Some of the output text is
    # copied verbatim from `span`, while other pieces are rewritten due to
    # `rws`.  We perform this transformation according to these rules:
    #
    #  1. If both endpoints of a node span fall within (or on the boundary of)
    #     a verbatim portion of `span`, then the transformed node span covers
    #     the region between the two corresponding points in the output text.
    #     This applies even if some rewrites occur in between those two
    #     endpoints.
    #  2. If either endpoint falls strictly within rewritten text, then the
    #     node span is dropped and a warning is printed.

    # Implementation:
    #
    # We keep track of the current input position (within `span`), the current
    # output position (within `parts`), and a translated copy of `nodes` (with
    # start/end positions relative to the output rather than the input).  Each
    # time we record a new output part, we also look at the corresponding
    # input positions, and use that to translate the start/end positions of
    # entries in the translated copy of `nodes`.

    rws.sort(key=lambda rw: rw['old_span']['lo'])
    # Position in the old text (`span`)
    old_pos = span['lo']

    parts = []
    # Position in the new text (`span` plus rewrites, as stored in `parts`)
    new_pos = 0

    # Translated endpoints of nodes in `nodes`
    node_ends = [[None, None] for _ in nodes]
    # All translated nodes.  We fill this in with the results of recursive
    # calls, then add everything from `nodes`/`node_ends` when we're done.
    new_nodes = []
    # Index nodes by lo and hi position for fast lookups.
    nodes_by_lo = sorted(range(len(nodes)), key=lambda i: nodes[i]['span']['lo'])
    nodes_by_hi = sorted(range(len(nodes)), key=lambda i: nodes[i]['span']['hi'])
    # Wrap in a list so we can pass it by reference
    lo_idx = [0]
    hi_idx = [0]

    def indexed_nodes(end, nodes_by_x, x_idx, side, inclusive):
        '''Yield `i, nodes[i]` for every node whose `side` endpoint is between
        `old_pos` and `end`.'''
        while x_idx[0] < len(nodes_by_x):
            i = nodes_by_x[x_idx[0]]
            n = nodes[i]
            pos = n['span'][side]
            if pos < end or (inclusive and pos == end):
                yield i, n
                x_idx[0] += 1
            else:
                break

    def emit(next_old_pos, text):
        nonlocal old_pos, new_pos

        if text is None:
            # Reusing existing text.  Update `node_ends` with translated
            # endpoint positions.
            text = subspan_src(span, old_pos, next_old_pos)
            offset = new_pos - old_pos
            for i, n in indexed_nodes(next_old_pos, nodes_by_lo, lo_idx, 'lo', True):
                node_ends[i][0] = n['span']['lo'] + offset
            for i, n in indexed_nodes(next_old_pos, nodes_by_hi, hi_idx, 'hi', True):
                node_ends[i][1] = n['span']['hi'] + offset

        parts.append(text)
        old_pos = next_old_pos
        new_pos += len(text)

    for rw in rws:
        if old_pos < rw['old_span']['lo']:
            emit(rw['old_span']['lo'], None)

        if rw['adjust'] == 'parenthesize':
            emit(old_pos, '(')

        new_text, rw_new_nodes = apply_rewrites(rw['new_span'], rw['rewrites'], rw['nodes'])
        new_nodes.extend((lo + new_pos, hi + new_pos, n) for (lo, hi, n) in rw_new_nodes)
        emit(rw['old_span']['hi'], new_text)

        if rw['adjust'] == 'parenthesize':
            emit(old_pos, ')')

    if old_pos < span['hi']:
        emit(span['hi'], None)


    new_text = ''.join(parts)

    for i, n in enumerate(nodes):
        new_lo, new_hi = node_ends[i]
        if new_lo is None or new_hi is None:
            print('warning: bad mapped range %s, %s for %s' % (new_lo, new_hi, n))
        new_nodes.append((new_lo, new_hi, n['id']))

    return ''.join(parts), new_nodes


Text = parse.Text
ScriptDiff = namedtuple('ScriptDiff', ('commands', 'raw', 'text', 'nodes', 'marks'))

def run_refactor_scripts(args, blocks):
    '''Run all refactoring commands in `blocks`, returning a new list of blocks
    where each `Script` is replaced by a `ScriptDiff`, which includes the old
    and new text of each file modified by the commands.'''
    all_cmds, script_output = combine_script_blocks(blocks)
    run_refactor(args.project_dir, all_cmds)

    result = []
    prev_text = {}
    # The "old" node map for the first file is always empty, but that's okay
    # because we only use it to look up marks, and the mark list is also always
    # empty.
    prev_nodes = {}
    prev_marks = []
    for i, b in enumerate(blocks):
        rw_idx = script_output.get(i)
        if rw_idx is None:
            assert isinstance(b, parse.Text)
            result.append(b)
            continue

        assert isinstance(b, parse.Script)
        with open(os.path.join(args.project_dir, 'rewrites.%d.json' % rw_idx)) as f:
            rws = json.load(f)

        text = {}
        nodes = {}
        for rw in rws:
            path = rw['new_span']['file']
            if path not in prev_text:
                old_text = rw['new_span']['src']
            else:
                old_text = prev_text[path]
            if path not in prev_nodes:
                old_nodes = []
            else:
                old_nodes = prev_nodes[path]
            new_text, new_nodes = apply_rewrites(rw['new_span'], rw['rewrites'], rw['nodes'])
            for lo, hi, n in new_nodes:
                print(' ** new node: %d @ %s .. %s = %r' % (
                    n, lo, hi, new_text[lo:hi]))
            text[path] = (old_text, new_text)
            nodes[path] = (old_nodes, new_nodes)
            prev_text[path] = new_text
            prev_nodes[path] = new_nodes

        if len(b.commands) > 0 and b.commands[-1] == ['commit']:
            # `commit` saves the previous marks before clearing, but we
            # actually want to pretend that the marks were cleared first, so
            # that the next block doesn't get a bunch of random removed marks
            # included in its diff.
            cur_marks = []
        else:
            with open(os.path.join(args.project_dir, 'marks.%d.json' % rw_idx)) as f:
                j = json.load(f)
            #cur_marks = literate.marks.parse_marks(j)
            cur_marks = None #TODO

        result.append(ScriptDiff(b.commands, b.raw,
            text, nodes, (prev_marks, cur_marks)))
        prev_marks = cur_marks

    return result
