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


- `#[prec_contains_expr]`: When entering a child of this node type, by default
  set expr precedence to `RESET` (don't parenthesize).  The expr precedence can
  be overridden using other `prec` attributes on specific fields.

- `#[prec=name]`: When entering this child node, set expr precedence to
  `PREC_[name]` (if `name` is all-caps) or to the precedence of
  `AssocOp::[name]` (otherwise).

- `#[prec_inc=name]`: Like `#[prec=name]`, but add 1 to the precedence value
  so that the subexpr will be parenthesized if it has the same type as the
  current expr.

- `#[prec_first=name]`: On a field containing a sequence, use a different
  precedence value for the first element of the sequence.  `name` is parsed the
  same as in `#[prec=name]`.

- `#[prec_left_of_binop=op]`, `#[prec_right_of_binop=op]`: Use the appropriate
  precedence value for the left/right operand of a binary operator.  The
  argument `op` should be the name of the field containing the binop.

- `#[prec_special=kind]`: Apply special parenthesization rules, in addition to
  normal precedence.  `kind` should be the name of a `rewrite::ExprPrec`
  variant: `Cond` for exprs in conditional-like positions, or `Callee` for
  exprs in function-call callee positions.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


def prec_name_to_expr(name, inc):
    inc_str = '' if not inc else ' + 1'
    if name.isupper():
        # If all letters are uppercase, it's a precedence constant from
        # syntax::util::parser
        return 'parser::PREC_%s%s' % (name, inc_str)
    else:
        # If some letters are lowercase, it's an AssocOp variant name.
        return 'parser::AssocOp::%s.precedence() as i8%s' % (name, inc_str)

def field_prec_expr(f, first):
    # First, figure out the "normal" precedence expression.
    prec_val = 'parser::PREC_RESET'

    prec = f.attrs.get('prec')
    if prec:
        prec_val = prec_name_to_expr(prec, False)

    prec_inc = f.attrs.get('prec_inc')
    if prec_inc:
        prec_val = prec_name_to_expr(prec_inc, True)

    left_of = f.attrs.get('prec_left_of_binop')
    if left_of:
        # Refer to `op1` instead of `op`, to get the binop as it appear in the
        # new AST.
        prec_val = 'binop_left_prec(%s1)' % left_of

    right_of = f.attrs.get('prec_right_of_binop')
    if right_of:
        prec_val = 'binop_right_prec(%s1)' % right_of

    prec_first = f.attrs.get('prec_first')
    if first and prec_first:
        prec_val = prec_name_to_expr(prec_first, False)

    # Now apply `prec_special`, if present
    ctor = f.attrs.get('prec_special', 'Normal')
    return 'ExprPrec::%s(%s)' % (ctor, prec_val)

@linewise
def do_recycled_match(se, target1, target2):
    contains_expr = 'prec_contains_expr' in se.attrs

    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            if f.attrs.get('rewrite') == 'ignore':
                continue
            yield '    ({'

            if 'prec_first' in f.attrs:
                yield '      let old = rcx.replace_expr_prec(%s);' % \
                        field_prec_expr(f, True)
                yield '      let fail = Rewrite::rewrite_recycled(&%s1[0], &%s2[0], ' \
                        'rcx.borrow());' % (f.name, f.name)
                yield '      rcx.replace_expr_prec(%s);' % field_prec_expr(f, False)
                yield '      let fail = fail || Rewrite::rewrite_recycled(&%s1[1..], &%s2[1..], ' \
                        'rcx.borrow());' % (f.name, f.name)
                yield '      rcx.replace_expr_prec(old);'
                yield '      fail'
            else:
                if contains_expr:
                    yield '      let old = rcx.replace_expr_prec(%s);' % \
                            field_prec_expr(f, False)
                yield '      let fail = Rewrite::rewrite_recycled(%s1, %s2, rcx.borrow());' % \
                        (f.name, f.name)
                if contains_expr:
                    yield '      rcx.replace_expr_prec(old);'
                yield '      fail'

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
    contains_expr = 'prec_contains_expr' in se.attrs

    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            if f.attrs.get('rewrite') == 'ignore':
                continue
            yield '    {'

            if 'prec_first' in f.attrs:
                yield '      let old = rcx.replace_expr_prec(%s);' % \
                        field_prec_expr(f, True)
                yield '      Rewrite::rewrite_fresh(&%s1[0], &%s2[0], rcx.borrow());' % \
                        (f.name, f.name)
                yield '      rcx.replace_expr_prec(%s);' % field_prec_expr(f, False)
                yield '      Rewrite::rewrite_fresh(&%s1[1..], &%s2[1..], rcx.borrow());' % \
                        (f.name, f.name)
                yield '      rcx.replace_expr_prec(old);'
            else:
                if contains_expr:
                    yield '      let old = rcx.replace_expr_prec(%s);' % \
                            field_prec_expr(f, False)
                yield '      Rewrite::rewrite_fresh(%s1, %s2, rcx.borrow());' % (f.name, f.name)
                if contains_expr:
                    yield '      rcx.replace_expr_prec(old);'

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
    yield '    <Self as GetSpan>::get_span(self)'
    yield '  }'
    yield ''
    yield '  fn get_id(&self) -> NodeId {'
    yield '    <Self as GetNodeId>::get_node_id(self)'
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
