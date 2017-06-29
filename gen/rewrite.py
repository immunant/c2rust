'''This module generates `Rewrite` impls for each AST node type.

- Rewrite struct vaules by rewriting their corresponding fields.  If any field
  fails to rewrite, the whole struct fails to rewrite.
- Rewrite two enum values of the same variant as with structs.  For two enum
  values of different variants, rewriting fails.
- Rewriting of flag values succeeds if the values are equal and fails if they
  are not.  It otherwise has no effect.

Note that no actual rewriting happens in the default implementations above.
Rewriting is handled by "recovery" actions: if a node would fail to rewrite
(due a failure among its children, for example), instead the recovery action
runs, and it succeeds.  The recovery action will typically record a rewrite
with the `RewriteCtxt`.

Attributes:

- `#[rewrite_splice]`: This node type is a "splice point": if needed, the
  rewriting process can cut out the node's source text and replace it with
  other text.  In recycled mode, if the node or its children require rewriting,
  then `rewrite_recycled` will call `splice_recycled_[node]` and return `true`
  instead of `false`.  In fresh mode, `rewrite_fresh` will check this node's
  span and call `splice_fresh_[node]` if it has old source text available.

- `#[rewrite=ignore]`: Ignore nodes of this type.  Perform no side effects and
  always return success, in both `rewrite_recycled` and `rewrite_fresh`.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


@linewise
def do_recycled_match(se, target1, target2):
    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            yield '    Rewrite::rewrite_recycled(%s1, %s2, rcx.borrow()) ||' % (f.name, f.name)
        yield '    false'
        yield '  }'
    yield '  (_, _) => true,'
    yield '}'

@linewise
def do_recycled_body(d):
    mode = d.attrs.get('rewrite')
    if mode is None:
        if isinstance(d, (Struct, Enum)):
            mode = 'compare'
        else:
            mode = 'eq'

    splice = 'rewrite_splice' in d.attrs

    if splice:
        # We only need the mark if we might recover.
        yield 'let mark = rcx.mark();'

    # Make recursive calls
    yield 'let need_rewrite ='
    if mode == 'ignore':
        yield '  false'
    elif mode == 'eq':
        yield '  self != old'
    elif mode == 'compare':
        yield indent(do_recycled_match(d, 'self', 'old'), '  ')
    yield ';'

    # Maybe recover, then return the final result
    yield 'if need_rewrite {'
    if splice:
        # Drop any rewrites pushed before the failure, then recover.
        yield '  rcx.rewind(mark);'
        yield '  splice_recycled_%s(self, old, rcx);' % snake(d.name)
        yield '  false'
    else:
        yield '  true'
    yield '} else {'
    yield '  false'
    yield '}'

@linewise
def do_fresh_match(se, target1, target2):
    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            yield '    Rewrite::rewrite_fresh(%s1, %s2, rcx.borrow());' % (f.name, f.name)
        yield '  }'
    yield '  (_, _) => panic!("new and reparsed AST differ"),'
    yield '}'

@linewise
def do_fresh_body(d):
    splice = 'rewrite_splice' in d.attrs

    if splice:
        # We only need the mark if we might recover.
        yield 'if splice_fresh_%s(self, reparsed, rcx.borrow()) {' % snake(d.name)
        yield '  return;'
        yield '}'

    if isinstance(d, (Struct, Enum)):
        yield do_fresh_match(d, 'self', 'reparsed')

@linewise
def do_impl(d):
    yield "impl Rewrite for %s {" % d.name
    yield "  fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {"
    yield indent(do_recycled_body(d), '    ')
    yield '  }'
    yield "  fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {"
    yield indent(do_fresh_body(d), '    ')
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        yield do_impl(d)
