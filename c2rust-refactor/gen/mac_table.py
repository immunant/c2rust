'''This module generates the `CollectMacros` impls for building the macro
collapser's `MacTable`.  This is a basic recursive traversal on two ASTs
(unexpanded and expanded), with a few special behaviors:

 * When the unexpanded AST is `MacCall`, we call `record_one_macro` with the
   unexpanded `MacCall` node and the corresponding expanded node's `NodeId`.  This
   check happens at nodes marked `#[mac_table_record]`, which should be the
   node with the ID (`Expr`, not `ExprKind`) and should implement `MaybeInvoc`.

 * On fields with the `#[mac_table_seq]` attribute, we call the helper function
   `collect_macros_seq` instead of the normal `collect_macros`, which handles
   macros possibly expanding into multiple sequential nodes.

Note that `#[mac_table_record]` should only be applied on nodes where a macro
expands to exactly one node.  Macros that can expand to multiple nodes need to
be handled by `#[mac_table_seq]` instead.  Leaving off `#[mac_table_record]`
for these nodes means we get a visible error if we forget the sequence
handling.


Attributes:

- `#[mac_table_record]`: Check for and record macros at each node of this type.

- `#[mac_table_seq]`: On a field, use `collect_macros_seq` to handle macros
  within the unexpanded sequence that produce multiple items in the expanded
  sequence.

- `#[mac_table_custom]`: Don't generate an impl for this type - a custom one
  will be provided elsewhere.
'''

from datetime import datetime
from textwrap import indent

from ast import *
from util import *

@linewise
def do_collect_macros_body(se, target1, target2):
    if not isinstance(se, (Struct, Enum)):
        return

    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')

        for f in v.fields:
            if f.attrs.get('match') == 'ignore':
                continue
            if 'mac_table_seq' in f.attrs:
                yield '    collect_macros_seq(%s1, %s2, cx);' % (f.name, f.name)
            else:
                yield '    CollectMacros::collect_macros(%s1, %s2, cx);' % (f.name, f.name)

        yield '  },'

    yield '  (_, _) => {},'
    yield '}'


    if 'mac_table_record' in se.attrs:
        yield 'if let Some(invoc) = MaybeInvoc::as_invoc(%s) {' % target1
        yield '  match MaybeInvoc::as_invoc(%s) {' % target2
        yield '    Some(InvocKind::Attrs(..)) => {}'
        yield '    Some(_) => panic!("impossible: found macro invocation in expanded AST"),'
        yield '    None => cx.record_one_macro(%s.id, invoc, MacNodeRef::%s(new)),' % \
                (target1, se.name)
        yield '  }'
        yield '}'

@linewise
def do_collect_macros_impl(d):
    yield '#[allow(unused)]'
    yield 'impl CollectMacros for %s {' % d.name
    yield "  fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {"
    yield indent(do_collect_macros_body(d, 'old', 'new'), '    ')
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        if 'mac_table_custom' not in d.attrs:
            yield do_collect_macros_impl(d)
