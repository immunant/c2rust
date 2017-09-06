'''This module generates `Rewrite` impls for each AST node type.

By default, the generated impls simply walk recursively over their two input
ASTs, checking for differences (in the `rewrite_recycled` case) but otherwise
having no side effects.  Rewriting is accomplished by marking some AST node
types as "splice points" with the `#[rewrite_splice]`, and then implementing
the `Splice` trait for those types.  The generated impls will call `Splice`
methods to perform the actual rewriting when needed (for `rewrite_recycled`,
when a difference is detected between ASTs; for `rewrite_fresh`, when a node is
found to originate in the old code).

Attributes:

- `#[rewrite_splice]`: This node type is a "splice point": if needed, the
  rewriting process can cut out the node's source text and replace it with
  other text.  In recycled mode, if the node or its children require rewriting,
  then `rewrite_recycled` will call `Splice::splice_recycled` and return `true`
  instead of `false`.  In fresh mode, `rewrite_fresh` will check this node's
  span and call `Splice::splice_fresh` if it has old source text available.

- `#[rewrite_seq_item]`: Give the node type a complete (non-stub) `SeqItem`
  impl.  This allows the use of smarter rewriting (based on edit distance) for
  sequences of this node type.  This only works for types supporting
  `GetNodeId` and `GetSpan`.

- `#[rewrite=ignore]`: On a type, ignore nodes of this type; on a field, ignore
  the contents of this field.  Perform no side effects and always return
  success, in both `rewrite_recycled` and `rewrite_fresh`.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


@linewise
def do_record_step_kind(se, v, f):
    kind = f.attrs.get('rewrite_step_kind') or \
            v.attrs.get('rewrite_default_step_kind') or \
            se.attrs.get('rewrite_default_step_kind') or \
            'Other'

    if kind in ('Other', 'StmtExpr'):
        yield 'rcx.push_step(VisitStep::%s);' % kind
    else:
        yield 'rcx.push_step(VisitStep::%s(P(self.clone())));' % kind

@linewise
def do_recycled_match(se, target1, target2):
    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            if f.attrs.get('rewrite') == 'ignore':
                continue
            yield '    ({'
            yield indent(do_record_step_kind(se, v, f), '      ')
            yield '      let ok = Rewrite::rewrite_recycled(%s1, %s2, rcx.borrow());' % \
                    (f.name, f.name)
            yield '      rcx.pop_step();'
            yield '      ok'
            yield '    }) ||'
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
        yield '  <%s as Splice>::splice_recycled(self, old, rcx);' % d.name
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
            if f.attrs.get('rewrite') == 'ignore':
                continue
            yield '    {'
            yield indent(do_record_step_kind(se, v, f), '      ')
            yield '      Rewrite::rewrite_fresh(%s1, %s2, rcx.borrow());' % (f.name, f.name)
            yield '      rcx.pop_step();'
            yield '    }'
        yield '  }'
    yield '  (_, _) => panic!("new and reparsed AST differ ({:?}  !=  {:?})",'
    if 'no_debug' not in se.attrs:
        yield '                   %s, %s),' % (target1, target2)
    else:
        yield '                   "<%s>", "<%s>"),' % (se.name, se.name)
    yield '}'

@linewise
def do_fresh_body(d):
    splice = 'rewrite_splice' in d.attrs

    if splice:
        # We only need the mark if we might recover.
        yield 'if <%s as Splice>::splice_fresh(self, reparsed, rcx.borrow()) {' % d.name
        yield '  return;'
        yield '}'

    if isinstance(d, (Struct, Enum)):
        yield do_fresh_match(d, 'self', 'reparsed')

@linewise
def do_impl(d):
    yield '#[allow(unused)]'
    yield 'impl Rewrite for %s {' % d.name
    yield '  fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {'
    yield indent(do_recycled_body(d), '    ')
    yield '  }'
    yield '  fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {'
    yield indent(do_fresh_body(d), '    ')
    yield '  }'
    yield '}'

@linewise
def do_full_seq_item_impl(d):
    yield '#[allow(unused)]'
    yield 'impl SeqItem for %s {' % d.name
    yield '  #[inline]'
    yield '  fn supported() -> bool { true }'
    yield ''
    yield '  fn get_span(&self) -> Span {'
    yield '    <Self as get_span::GetSpan>::get_span(self)'
    yield '  }'
    yield ''
    yield '  fn get_id(&self) -> NodeId {'
    yield '    <Self as get_node_id::GetNodeId>::get_node_id(self)'
    yield '  }'
    yield ''
    yield '  fn splice_recycled_span(new: &Self, old_span: Span, rcx: RewriteCtxtRef) {'
    yield '    <Self as Splice>::splice_recycled_span(new, old_span, rcx);'
    yield '  }'
    yield '}'

@linewise
def do_stub_seq_item_impl(d):
    yield 'impl SeqItem for %s {}' % d.name

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        yield do_impl(d)
        if 'rewrite_seq_item' in d.attrs:
            yield do_full_seq_item_impl(d)
        else:
            yield do_stub_seq_item_impl(d)
