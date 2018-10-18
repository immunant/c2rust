'''This module generates `NtMatch` impls, which look for macro-substituted
nonterminals in an old AST, and try to match them to nodes in a new AST.
Specifically, this is a recursive traversal of both ASTs that, when
encountering an old AST node with an empty-SyntaxContext Span, records the span
along with the new AST node.

Attributes:

- `#[nonterminal]`: Indicates a node type that can be used as a macro
  nonterminal.  When `nt_match` enters such a node, it will check the old
  node's SyntaxContext, and may record the old span and new node into the
  `Ctxt`.  The type must implement `GetSpan` and `AsNonterminal`.
'''

from datetime import datetime
from textwrap import indent

from ast import *
from util import *

@linewise
def do_nt_match_body(se, target1, target2):
    if 'nonterminal' in se.attrs:
        yield 'if check_nonterminal(%s, %s, cx) {' % (target1, target2)
        yield '  return;'
        yield '}'

    if not isinstance(se, (Struct, Enum)):
        return

    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')

        for f in v.fields:
            yield '    NtMatch::nt_match(%s1, %s2, cx);' % (f.name, f.name)

        yield '  },'

    yield '  (_, _) => {},'
    yield '}'

@linewise
def do_nt_match_impl(d):
    yield '#[allow(unused)]'
    yield 'impl NtMatch for %s {' % d.name
    yield "  fn nt_match(old: &Self, new: &Self, cx: &mut Ctxt) {"
    yield indent(do_nt_match_body(d, 'old', 'new'), '    ')
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        yield do_nt_match_impl(d)
