'''`c2rust-refactor` command invocation and output parsing.'''
import bisect
from collections import namedtuple
import os
import sys

from plumbum import local, FG
from plumbum.cmd import cargo
from common import *

from literate.file import File
from literate import parse


def combine_script_blocks(blocks):
    '''Given a list of `Text` and `Script` blocks from the `parse` module,
    combine all refactoring commands into a single list, inserting `write`
    commands to record intermediate states.  Returns the combined command list,
    along with a dict mapping each script block's index in the input to the
    index where its output will appear (for use in finding
    "rewrite.N.json").'''
    script_output = {}
    # Index (`N`) of the most recent `rewrite.N.json`.
    last_output = -1
    all_cmds = []

    def emit(cmd: [str]):
        nonlocal last_output
        if cmd[0] in ('write', 'commit'):
            # This handles the `write` commands we inject in the loop below, as
            # well as any `write` or `commit` commands in the original script.
            last_output += 1
        all_cmds.append(cmd)

    for i, b in enumerate(blocks):
        if isinstance(b, parse.Text):
            pass
        elif isinstance(b, parse.Script):
            for c in b.commands:
                emit(c)

            # We need a snapshot of the state as of the end of the block, so if
            # it doesn't end with `write` or `commit`, we inject an additional
            # `write` ourselves.
            if len(b.commands) == 0 or b.commands[-1][0] not in ('write', 'commit'):
                emit(['write'])
            assert last_output != -1
            script_output[i] = last_output

        else:
            raise TypeError('expected Text or Script, got %s' % (type(b),))

    return all_cmds, script_output

def run_refactor(work_dir, cmds: [[str]], mode='json,marks,alongside'):
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


class BisectRange:
    '''A sequence of items that can be queried to find every item `x` where
    `start < f(x) < end`.'''
    def __init__(self, xs, f):
        keyed_vals = sorted(((x, f(x)) for x in xs), key=lambda x_fx: x_fx[1])
        self.xs = [x for x, fx in keyed_vals]
        self.fxs = [fx for x, fx in keyed_vals]

    def iter_range(self, start, end, include_start=False, include_end=False):
        if start is None:
            i0 = 0
        elif include_start:
            i0 = bisect.bisect_left(self.fxs, start)
        else:
            i0 = bisect.bisect_right(self.fxs, start)

        if end is None:
            i1 = len(self.xs)
        elif include_end:
            i1 = bisect.bisect_right(self.fxs, end)
        else:
            i1 = bisect.bisect_left(self.fxs, end)

        for i in range(i0, i1):
            yield self.xs[i]

def subspan_src(span, lo, hi):
    '''Get the source text of the subspan of JSON `span` that ranges from `lo
    .. hi`.'''
    assert span['lo'] <= lo
    assert hi <= span['hi']
    start = span['lo']
    return span['src'][lo - start : hi - start]

def apply_rewrites(span, rws, nodes):
    '''Given a "new" span and its corresponding rewrites and nodes, return the
    rewritten text for that span along with a list of all node spans in the
    output (including those from recursive rewrites).'''

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
    nodes_by_lo = BisectRange(range(len(nodes)), f=lambda i: nodes[i]['span']['lo'])
    nodes_by_hi = BisectRange(range(len(nodes)), f=lambda i: nodes[i]['span']['hi'])

    def emit(next_old_pos, text):
        nonlocal old_pos, new_pos

        if text is None:
            # Reusing existing text.  Translate node endpoind positions
            # everywhere in the reused text.
            text = subspan_src(span, old_pos, next_old_pos)

            offset = new_pos - old_pos
            for i in nodes_by_lo.iter_range(old_pos, next_old_pos,
                    include_start=True, include_end=True):
                node_ends[i][0] = nodes[i]['span']['lo'] + offset
            for i in nodes_by_hi.iter_range(old_pos, next_old_pos,
                    include_start=True, include_end=True):
                node_ends[i][1] = nodes[i]['span']['hi'] + offset

        if len(text) > 0:
            parts.append(text)
        old_pos = next_old_pos
        new_pos += len(text)

    for rw in rws:
        # Note we "emit" the region from `old_pos` to `old_span.lo` even when
        # it's empty.  This doesn't add any text, but it does update any node
        # endpoints that fall exactly on `old_pos`/`old_span.lo`.
        emit(rw['old_span']['lo'], None)

        if rw['adjust'] == 'parenthesize':
            emit(old_pos, '(')

        new_text, rw_new_nodes = apply_rewrites(rw['new_span'], rw['rewrites'], rw['nodes'])
        new_nodes.extend((lo + new_pos, hi + new_pos, n) for (lo, hi, n) in rw_new_nodes)
        emit(rw['old_span']['hi'], new_text)

        if rw['adjust'] == 'parenthesize':
            emit(old_pos, ')')

    emit(span['hi'], None)

    new_text = ''.join(parts)

    for i, n in enumerate(nodes):
        new_lo, new_hi = node_ends[i]
        if new_lo is None or new_hi is None:
            print('warning: bad mapped range %s, %s for %s' % (new_lo, new_hi, n))
        new_nodes.append((new_lo, new_hi, n['id']))

    return ''.join(parts), new_nodes


Text = parse.Text
ScriptDiff = namedtuple('ScriptDiff', ('commands', 'raw', 'old', 'new'))

def run_refactor_scripts(args, blocks):
    '''Run all refactoring commands in `blocks`, returning a new list of blocks
    where each `Script` is replaced by a `ScriptDiff`, which includes the old
    and new text of each file modified by the commands.'''
    all_cmds, script_output = combine_script_blocks(blocks)
    run_refactor(args.project_dir, all_cmds)

    result = []
    all_files = []
    prev_files = {}
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


        # Handle marks first, so `cur_marks` is available for `File`
        # construction.

        if len(b.commands) > 0 and b.commands[-1] == ['commit']:
            # `commit` saves the previous marks before clearing, but we
            # actually want to pretend that the marks were cleared first, so
            # that the next block doesn't get a bunch of random removed marks
            # included in its diff.
            cur_marks = []
        else:
            with open(os.path.join(args.project_dir, 'marks.%d.json' % rw_idx)) as f:
                cur_marks = json.load(f)

        prev_marks = cur_marks


        # Look at the rewrites and use them to add `File`s to `old` and `new`.
        old = {}
        new = {}

        for rw in rws:
            path = rw['new_span']['file']

            if path not in prev_files:
                text = rw['new_span']['src']
                nodes = []
                old[path] = File(path, text, nodes, [])
                all_files.append(old[path])
            else:
                old[path] = prev_files[path]

            text, nodes = apply_rewrites(rw['new_span'], rw['rewrites'], rw['nodes'])

            new[path] = File(path, text, nodes, cur_marks)
            all_files.append(new[path])
            prev_files[path] = new[path]

        result.append(ScriptDiff(b.commands, b.raw, old, new))
        prev_marks = cur_marks

    return result, all_files
