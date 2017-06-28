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

- `#[rewrite_recover=func]`: If this node would fail to rewrite, instead invoke
  `func(old, new, rcx)` and succeed.

- `#[rewrite_context]`: Push a context entry onto the `rcx` stack upon entering
  this node, and pop it upon leaving.

- `#[rewrite=ignore]`: Ignore nodes of this type.  Perform no side effects and
  always return success.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


@linewise
def do_match(se, target1, target2):
    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            yield '    rcx.rewrite(%s1, %s2) &&' % (f.name, f.name)
        yield '    true'
        yield '  }'
    yield '  (_, _) => false,'
    yield '}'

@linewise
def do_fn_body(d):
    mode = d.attrs.get('rewrite')
    if mode is None:
        if isinstance(d, (Struct, Enum)):
            mode = 'compare'
        else:
            mode = 'eq'

    recover = d.attrs.get('rewrite_recover')
    context = 'rewrite_context' in d.attrs

    if recover:
        # We only need the mark if we might recover.
        yield 'let mark = rcx.mark();'

    # Make recursive calls
    yield 'let result ='
    if context:
        yield '  rcx.in_%s(self, new, |rcx| {' % snake(d.name)
    if mode == 'ignore':
        yield '  true'
    elif mode == 'eq':
        yield '  self == new'
    elif mode == 'compare':
        yield indent(do_match(d, 'self', 'new'), '  ')
    if context:
        yield '  })'
    yield ';'

    # Maybe recover, then return the final result
    yield 'if !result {'
    if recover:
        # Drop any rewrites pushed before the failure, then recover.
        yield '  rcx.rewind(mark);'
        yield '  %s(self, new, rcx);' % recover
        yield '  true'
    else:
        yield '  false'
    yield '} else {'
    yield '  true'
    yield '}'

@linewise
def do_impl(d):
    yield "impl<'ast> Rewrite<'ast> for %s {" % d.name
    yield "  fn rewrite(&'ast self, new: &'ast Self, rcx: &mut RewriteCtxt<'ast>) -> bool {"
    yield indent(do_fn_body(d), '    ')
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        yield do_impl(d)
