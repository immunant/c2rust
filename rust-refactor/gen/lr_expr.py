'''This module generates `LRExpr` implementations for distinguishing l-value
and r-value contexts in expressions.

Attributes:

- `#[lvalue_imm]`, `#[lvalue_mut]`: This subexpression is always an lvalue,
  with the indicated mutability.

- `#[lvalue_kind=e]`: This subexpression is always an lvalue, whose mutability
  is given by the expression `e`.

- `#[lr_propagate]`: This subexpression is an lvalue if the parent expression
  is an lvalue, with the same mutability.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *

@linewise
def expr_kind_match(d, mode):
    yield 'match self {'
    for v, path in variants_paths(d):
        yield '  %s => %s(' % (struct_pattern(v, path, bind_mode=''), path)
        for f in v.fields:
            if 'lvalue_mut' in f.attrs:
                yield '    %s.fold_lvalue_mut(lr),' % f.name
            elif 'lvalue_imm' in f.attrs:
                yield '    %s.fold_lvalue(lr),' % f.name
            elif 'lr_propagate' in f.attrs:
                yield '    %s.fold_%s(lr),' % (f.name, mode)
            elif 'lvalue_kind' in f.attrs:
                yield '    match %s {' % f.attrs['lvalue_kind']
                yield '      Mutability::Mutable =>'
                yield '        %s.fold_lvalue_mut(lr),' % f.name
                yield '      Mutability::Immutable =>'
                yield '        %s.fold_lvalue(lr),' % f.name
                yield '    },'
            else:
                yield '    %s.fold_rvalue(lr),' % f.name
        yield '  ),'
    yield '}'

@linewise
def expr_kind_impl(d):
    yield 'impl LRExpr for %s {' % d.name
    yield '  fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield indent(expr_kind_match(d, 'rvalue'), '    ')
    yield '  }'
    yield '  fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield indent(expr_kind_match(d, 'lvalue'), '    ')
    yield '  }'
    yield '  fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield indent(expr_kind_match(d, 'lvalue_mut'), '    ')
    yield '  }'
    yield '}'

@linewise
def expr_impl(d):
    yield 'impl LRExpr for %s {' % d.name
    yield '  fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    let e = Expr { node: self.node.fold_rvalue(lr), ..self };'
    yield '    lr.fold_rvalue(e)'
    yield '  }'
    yield '  fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    let e = Expr { node: self.node.fold_lvalue(lr), ..self };'
    yield '    lr.fold_lvalue(e)'
    yield '  }'
    yield '  fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    let e = Expr { node: self.node.fold_lvalue_mut(lr), ..self };'
    yield '    lr.fold_lvalue_mut(e)'
    yield '  }'
    yield '}'

@linewise
def null_impl(d):
    yield 'impl LRExpr for %s {' % d.name
    yield '  fn fold_rvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    self'
    yield '  }'
    yield '  fn fold_lvalue<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    self'
    yield '  }'
    yield '  fn fold_lvalue_mut<LR: LRRewrites>(self, lr: &mut LR) -> Self {'
    yield '    self'
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        if d.name == 'Expr':
            yield expr_impl(d)
        elif d.name == 'ExprKind':
            yield expr_kind_impl(d)
        else:
            yield null_impl(d)
