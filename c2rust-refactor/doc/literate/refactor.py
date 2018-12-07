'''`c2rust-refactor` command invocation and output parsing.'''
import argparse
import bisect
import os
import shlex
import sys
import tempfile
from typing import List, Tuple, Dict, Optional, Any, Union, Iterable, \
        Callable, NamedTuple, Generic, TypeVar

from plumbum import local, FG
from plumbum.cmd import cargo
from common import *

from literate.file import File
from literate import parse


class CargoCrate(NamedTuple):
    '''A real crate, identified by its Cargo project directory.'''
    dir: str

class FileCrate(NamedTuple):
    '''A real crate consisting only of the indicated file.'''
    path: str

class TempCrate(NamedTuple):
    '''A temporary crate, built from some source text.  We handle these by
    writing the text to a temp file and passing explicit `rustc` arguments.'''
    text: str

AnyCrate = Union[CargoCrate, FileCrate, TempCrate]


class PermanentDirectory(NamedTuple):
    '''A simple TemporaryDirectory replacement, with just a `name` field.'''
    name: str

AnyDirectory = Union[PermanentDirectory, tempfile.TemporaryDirectory]


ResultKey = int

class ResultInfo(NamedTuple):
    dests: List[ResultKey]
    '''Keys in `RefactorState.results` where the (old, new) result pair should
    be stored.'''

    is_commit: bool
    '''`True` if this result is produced by a `commit` command, `False`
    otherwise (for `write`).'''


FLAG_OPTS = {
        'revert',
        'hidden',
        'refactor-target',
        'show-filename',
        'collapse-diff',
        'hide-diff',
        'highlight-mode',
        }

STR_OPTS = {
        'diff-style',
        'highlight-mode',
        }

OPT_DEFAULTS = {
        'revert': False,
        'hidden': False,
        'refactor-target': False,
        'show-filename': True,
        'collapse-diff': True,
        'hide-diff': False,
        'diff-style': 'context',
        'highlight-mode': 'hljs',
        }

FLAG_TRUTHY = { '1', 'true', 'y', 'yes', 'on' }
FLAG_FALSY = { '0', 'false', 'n', 'no', 'off' }

RefactorCommand = List[str]

class RefactorState:
    args: argparse.Namespace

    cur_crate: Optional[AnyCrate]

    '''Accumulator of refactoring commands to run.  We try to run all commands
    in one go for efficiency, and to avoid the need to keep track of rewritten
    files.'''
    pending_cmds: List[RefactorCommand]

    pending_results: List[ResultInfo]
    '''A `ResultInfo` for each refactoring result that will be produced by
    running `pending_cmds`.  That is, there is an entry in `pending_results`
    for each `commit` or `write` in `pending_cmds`.'''

    results: Dict[ResultKey, Tuple[Dict[str, File], Dict[str, File]]]
    '''A dict of refactoring results (pairs of old and new files), keyed on the
    `key` argument passed to `force_write`.'''

    all_files: List[File]
    '''A list of all `File` objects that appear somewhere in `results`.  Note
    that many `File`s appear twice in `results`, once as an old file and once
    as a new one.'''

    global_opts: Dict[str, Any]
    '''The current global refactoring options.'''

    def __init__(self):
        self.cur_crate = None

        self.pending_cmds = []
        self.pending_results = []

        self.results = {}
        self.all_files = []

        self.global_opts = OPT_DEFAULTS.copy()

    def flush(self):
        '''Process all pending commands, and clear the `pending_cmds`
        buffer.'''
        if len(self.pending_cmds) == 0:
            assert len(self.pending_results) == 0
            return

        work_dir = refactor_crate(self.cur_crate, self.pending_cmds)

        rp = ResultProcessor(self.all_files, work_dir.name)
        for i, info in enumerate(self.pending_results):
            # `commit` saves the previous marks before clearing, but we
            # actually want to pretend that the marks were cleared first, so
            # that the next block doesn't get a bunch of random removed marks
            # included in its diff.
            clear_marks = info.is_commit

            result = rp.next_result(clear_marks)
            for k in info.dests:
                self.results[k] = result

        self.pending_cmds = []
        self.pending_results = []

    def add_command(self, cmd: RefactorCommand):
        '''Add a single refactoring command to run.'''
        assert len(cmd) > 0
        self.pending_cmds.append(cmd)
        if cmd[0] == 'commit':
            self.pending_results.append(ResultInfo([], True))
        elif cmd[0] == 'write':
            self.pending_results.append(ResultInfo([], False))

    def add_commands(self, key: ResultKey, cmds: Iterable[RefactorCommand]):
        '''Add a block of refactoring commands to run.  Once the commands are
        actually run, the results will be stored under `key` in
        `self.results`.'''

        # Did `cmds` end with a command that writes out refactoring results?
        last_wrote = False
        for cmd in cmds:
            self.add_command(cmd)
            last_wrote = cmd[0] in ('commit', 'write')

        if not last_wrote:
            self.add_command(['write'])

        self.pending_results[-1].dests.append(key)

    def set_crate(self, crate: AnyCrate):
        self.flush()
        self.cur_crate = crate

    def reset(self):
        self.flush()

    def finish(self) -> Dict[ResultKey, Tuple[Dict[str, File], Dict[str, File]]]:
        self.flush()
        # Cause an error on further `add_commands`
        self.pending_cmds = None
        return self.results

    def parse_block_options(self, attrs: List[str]) -> Dict[str, Any]:
        '''Parse the attributes on a block to find any `literate`-specific
        options, merge those with the current global options, and return a dict
        of all options that apply to the block.

        The result dict contains entries for every option in `FLAG_OPTS` and
        `STR_OPTS`, plus special entries `_lang` (containing the initial
        language attribute) and `_attrs` (containing any leftover unrecognized
        attributes).'''
        opts = self.global_opts.copy()
        opts['_lang'] = None

        remaining_attrs = []

        for i, attr in enumerate(attrs):
            key, _, value = attr.partition('=')
            key, value = key.strip(), value.strip()

            if key.startswith('no-'):
                assert key[3:] in FLAG_OPTS, \
                        '`no-` prefix is only supported on flag options (option: %r)' % \
                        (key,)
                assert value == '', \
                        'cannot mix value with `no-` prefix (option: %r, value: %r)' % \
                        (key, value)
                key = key[3:]
                value = False

            if key in FLAG_OPTS:
                if isinstance(value, bool):
                    # It was set above by `no-` handling.
                    pass
                elif value == '':
                    value = True
                elif value.lower() in FLAG_TRUTHY:
                    value = True
                elif value.lower() in FLAG_FALSY:
                    value = False
                else:
                    raise ValueError('unknown value %r for flag option %r' %
                            (value, key))

            elif key in STR_OPTS:
                # No conversion necessary
                pass

            elif i == 0 and value == '':
                # The first option is normally expected to be a language name.
                opts['_lang'] = key
                remaining_attrs.append(attr)
                continue

            else:
                print('warning: unknown option %r (value: %r)' % (key, value))
                remaining_attrs.append(attr)
                continue

            # If we got here, `key` is in `FLAG_OPTS` or `STR_OPTS`, which
            # means it should at least have a default value in `opts` already.
            assert key in opts
            opts[key] = value

        opts['_attrs'] = remaining_attrs

        return opts

    def set_global_options(self, lines: List[str]):
        '''Update the current global options by parsing the lines of a
        `refactor-options` block.'''
        attrs = ['refactor-options']
        for l in lines:
            l = l.strip()
            if l == '' or l.startswith('#'):
                continue
            attrs.append(l)
        new_opts = self.parse_block_options(attrs)
        print('parsed attrs %s as %s' % (attrs, new_opts))
        del new_opts['_lang']
        del new_opts['_attrs']
        self.global_opts = new_opts



class ResultProcessor:
    def __init__(self, all_files: List[File], dir_path: str):
        self.all_files = all_files
        self.dir_path = dir_path

        self.rw_index = 0
        self.prev_files = {}
        self.prev_marks = []

    def next_result(self, clear_marks: bool=False) -> \
            Tuple[Dict[str, File], Dict[str, File]]:
        '''Load and process the next refactoring result.  If `clear_marks` is
        set, the content of the `marks.json` file is ignored, as if the
        refactoring process cleared all marks at the end.'''

        with open(os.path.join(self.dir_path, 'rewrites.%d.json' % self.rw_index)) as f:
            rws = json.load(f)

        if clear_marks:
            marks = []
        else:
            with open(os.path.join(self.dir_path, 'marks.%d.json' % self.rw_index)) as f:
                marks = json.load(f)

        old = {}
        new = {}

        for rw in rws:
            path = rw['new_span']['file']

            if path not in self.prev_files:
                text = rw['new_span']['src']
                nodes = []
                old[path] = File(path, text, nodes, [])
                self.all_files.append(old[path])
            else:
                old[path] = self.prev_files[path]

            text, nodes = apply_rewrites(rw['new_span'], rw['rewrites'], rw['nodes'])

            new[path] = File(path, text, nodes, marks)
            self.all_files.append(new[path])
            self.prev_files[path] = new[path]

        self.prev_marks = marks
        self.rw_index += 1

        return (old, new)


def refactor_crate(crate: AnyCrate, cmds: List[RefactorCommand]):
    '''Run refactoring commands `cmds` on `crate`.  If `crate` is a
    `TempCrate`, return the `TemporaryDirectory` where the refactoring was
    done.  Otherwise, return `None`.'''
    if isinstance(crate, CargoCrate):
        work_dir = PermanentDirectory(crate.dir)
        pre_args, post_args = ['--cargo'], []
    elif isinstance(crate, FileCrate):
        work_dir = PermanentDirectory(os.getcwd())
        pre_args, post_args = [], ['--', crate.path, '--crate-type', 'rlib']
    elif isinstance(crate, TempCrate):
        work_dir = tempfile.TemporaryDirectory()
        with open(os.path.join(work_dir.name, 'tmp.rs'), 'w') as f:
            f.write(crate.text)
        pre_args, post_args = [], ['--', os.path.join(work_dir.name, 'tmp.rs'),
                '--crate-type', 'rlib']
    else:
        raise TypeError('bad crate type %s' % type(crate))

    all_args = ['-r', 'json,marks']
    all_args.extend(pre_args)
    for cmd in cmds:
        all_args.extend(cmd)
        all_args.append(';')
    all_args.extend(post_args)


    refactor = get_cmd_or_die(config.C2RUST_BIN)['refactor']

    ld_lib_path = get_rust_toolchain_libpath()
    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in local.env:
        ld_lib_path += ':' + local.env['LD_LIBRARY_PATH']

    with local.env(RUST_BACKTRACE='1',
                   LD_LIBRARY_PATH=ld_lib_path):
        with local.cwd(work_dir.name):
            print('running %s in %s with %d cmds...' %
                    (refactor, work_dir.name, len(cmds)))
            refactor[all_args] & FG
            print('  refactoring done')


    return work_dir


T = TypeVar('T')

class BisectRange(Generic[T]):
    '''A sequence of items that can be queried to find every item `x` where
    `start < f(x) < end`.'''
    def __init__(self, xs: Iterable[T], f: Callable[[T], int]):
        keyed_vals = sorted(((x, f(x)) for x in xs), key=lambda x_fx: x_fx[1])
        self.xs = [x for x, fx in keyed_vals]
        self.fxs = [fx for x, fx in keyed_vals]

    def iter_range(self, start: int, end: int,
            include_start: bool=False, include_end: bool=False):
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

JsonDict = Dict[str, Any]

def subspan_src(span: JsonDict, lo: int, hi: int):
    '''Get the source text of the subspan of JSON `span` that ranges from `lo
    .. hi`.'''
    assert span['lo'] <= lo
    assert hi <= span['hi']
    start = span['lo']
    return span['src'][lo - start : hi - start]

def apply_rewrites(span: JsonDict, rws: List[JsonDict], nodes: List[JsonDict]):
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

    def emit(next_old_pos: int, text: Optional[str]):
        nonlocal old_pos, new_pos

        if text is None:
            # Reusing existing text.  Translate node endpoind positions
            # everywhere in the reused text.
            text = subspan_src(span, old_pos, next_old_pos)

            offset = new_pos - old_pos
            # These checks are carefully designed to give good results around
            # insertions (where the replaced text has zero width).
            #
            # Suppose we have two spans, A from 0 .. 10 and B from 10 .. 20,
            # and our only rewrites are two insertions at position 10, each
            # inserting 5 characters.  The "correct" output is for A to cover
            # the range 0 .. 10 and B to cover 20 .. 30 in the output text,
            # with 10 .. 20 (the inserted text) being unannotated.
            #
            # We reach this case of `emit` three times: once for the reused
            # text 0 .. 10 -> 0 .. 10 (old -> new indices), once for the empty
            # string 10 .. 10 -> 15 .. 15 between the two rewrites, and once
            # for 10 .. 20 -> 20 .. 30.
            #
            # Note we can't simply ignore the middle case by filtering out
            # zero-width reuses, because there could be a zero-width reuse in
            # between two non-zero-(old-)width rewrites.  And also note that we
            # can't simply tweak `include_start` and `include_end` to get the
            # result we want, because `A.hi` and `B.lo` both coincide with both
            # the start and end positions of the middle case.
            #
            # Our solution is to include both ends in `iter_range`, but
            # postprocess (the additional `if` inside the second loop) so that
            # if the same old position is encountered multiple times, any `hi`
            # endpoints at that old position stay at the new position
            # corresponding to the first encounter, while any `lo` endpoints
            # stay at the last encounter.  So in our example, `A.hi` is 10 from
            # the first of the three cases, while `B.lo` is 20 from the last of
            # them.
            #
            # For cases that aren't near zero-(old-)width insertions, this is
            # all irrelevant: if the same old position is encountered multiple
            # times, it must correspond to the same new position at each
            # encounter, unless an insertion happened in between.

            for i in nodes_by_lo.iter_range(old_pos, next_old_pos,
                    include_start=True, include_end=True):
                # Keep only the last result (always overwrite)
                node_ends[i][0] = nodes[i]['span']['lo'] + offset
            for i in nodes_by_hi.iter_range(old_pos, next_old_pos,
                    include_start=True, include_end=True):
                # Keep only the first result
                if node_ends[i][1] is None:
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
            # Don't warn about nodes with dummy spans not getting updated
            # endpoints.  Those nodes don't actually appear in the source code.
            if n['span']['file'] != '<<dummy>>':
                print('warning: bad mapped range %s, %s for %s' % (new_lo, new_hi, n))
            continue
        new_nodes.append((new_lo, new_hi, n['id']))

    return ''.join(parts), new_nodes


class RefactorCode(NamedTuple):
    '''A Markdown code block (like `literate.parse.Code`), augmented with the
    state of the crate before and after running the contained refactoring
    scripts.'''
    attrs: List[str]
    lines: List[str]

    opts: Dict[str, Any]
    '''Rendering options that apply to this block.'''

    old: Dict[str, File]
    '''The files of the project prior to this refactoring step.'''
    new: Dict[str, File]
    '''The files of the project following this refactoring step.'''

# Reexport for convenience
Text = parse.Text
Code = parse.Code

Block = Union[Text, Code, RefactorCode]

def split_commands(code: str) -> List[RefactorCommand]:
    '''Parse a string as a sequence of shell words, then split those words into
    refactoring commands on `';'` separators.'''
    words = shlex.split(code)
    acc = []
    cmds = []

    for word in words:
        if word == ';':
            if len(acc) > 0:
                cmds.append(acc)
            acc = []
        else:
            acc.append(word)

    if len(acc) > 0:
        cmds.append(acc)

    return cmds

def run_refactor_scripts(args: argparse.Namespace,
        blocks: List[parse.Block]) -> Tuple[List[Block], List[File]]:
    # Run all refactoring commands, and get the refactoring results.
    rs = RefactorState()

    if args.project_dir is not None:
        rs.set_crate(CargoCrate(args.project_dir))

    block_opts = {}
    for i, b in enumerate(blocks):
        if not isinstance(b, parse.Code):
            continue

        opts = rs.parse_block_options(b.attrs)

        if opts['_lang'] == 'refactor':
            cmds = split_commands(''.join(b.lines))
            rs.add_commands(i, cmds)

            if opts['revert']:
                rs.reset()

        elif opts['_lang'] == 'refactor-options':
            rs.set_global_options(b.lines)

        if opts['refactor-target']:
            rs.set_crate(TempCrate(''.join(b.lines)))

        block_opts[i] = opts


    results = rs.finish()
    all_files = rs.all_files

    new_blocks = []
    for i, b in enumerate(blocks):
        if i in block_opts:
            opts = block_opts[i]
            print('opts for block %d: %s' % (i, block_opts[i]))
            print('  %r' % ''.join(b.lines))
            if opts['hidden']:
                continue

            if i in results:
                old, new = results[i]
                new_blocks.append(RefactorCode(opts['_attrs'], b.lines,
                    opts, old, new))
            else:
                new_blocks.append(b)
        else:
            new_blocks.append(b)

    return new_blocks, all_files

def run_refactor_for_playground(args: argparse.Namespace,
        script: str) -> Tuple[File, File]:
    rs = RefactorState()
    rs.set_crate(FileCrate(args.code))

    cmds = split_commands(script)
    rs.add_commands(0, cmds)

    results = rs.finish()
    return results[0], rs.all_files


