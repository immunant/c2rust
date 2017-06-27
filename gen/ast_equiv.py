from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


@comma_sep
def struct_fields(fields, suffix):
    for f in fields:
        yield '%s: ref %s%s' % (f.name, f.name, suffix)

@comma_sep
def tuple_fields(fields, suffix):
    for f in fields:
        yield 'ref %s%s' % (f.name, suffix)

def struct_pattern(s, path, suffix=''):
    if not s.is_tuple:
        return '%s { %s }' % (path, struct_fields(s.fields, suffix))
    else:
        if len(s.fields) == 0:
            return path
        else:
            return '%s(%s)' % (path, tuple_fields(s.fields, suffix))

@linewise
def exhaustiveness_check(se, target):
    yield 'match %s {' % target
    for v, path in variants_paths(se):
        yield '  &%s => {},' % struct_pattern(v, path)
    yield '}'

@linewise
def comparison(se, target1, target2):
    yield 'match (%s, %s) {' % (target1, target2)
    for v, path in variants_paths(se):
        yield '  (&%s,' % struct_pattern(v, path, '1')
        yield '   &%s) => {' % struct_pattern(v, path, '2')
        for f in v.fields:
            yield '    AstEquiv::ast_equiv(%s1, %s2) &&' % \
                    (f.name, f.name)
        yield '    true'
        yield '  }'
    yield '  (_, _) => false,'
    yield '}'


@linewise
def compare_impl(se):
    yield 'impl AstEquiv for %s {' % se.name
    yield '  fn ast_equiv(&self, other: &Self) -> bool {'
    yield '    // Exhaustiveness check'
    yield indent(exhaustiveness_check(se, 'self'), '    ')
    yield ''
    yield '    // Comparison'
    yield indent(comparison(se, 'self', 'other'), '    ')
    yield '  }'
    yield '}'

@linewise
def eq_impl(d):
    yield 'impl AstEquiv for %s {' % d.name
    yield '  fn ast_equiv(&self, other: &Self) -> bool {'
    yield '    self == other'
    yield '  }'
    yield '}'

@linewise
def ignore_impl(d):
    yield 'impl AstEquiv for %s {' % d.name
    yield '  fn ast_equiv(&self, other: &Self) -> bool {'
    yield '    true'
    yield '  }'
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        mode = d.attrs.get('equiv_mode')
        if mode is None:
            if isinstance(d, (Struct, Enum)):
                mode = 'compare'
            else:
                mode = 'eq'

        if mode == 'compare':
            yield compare_impl(d)
        elif mode == 'eq':
            yield eq_impl(d)
        elif mode == 'ignore':
            yield ignore_impl(d)
