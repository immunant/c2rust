'''This module generates `TryMatch` impls for each AST node type.

- Match two struct values by matching their corresponding fields.
- Match two enum values built from the same variant by matching their corresponding fields.  Enum
  values built from different variants fail to match.
- Ignore flag values - they always match.

Attributes:

- `#[match=custom]`: On a top-level declaration, skip generating an `impl` for this type, because
  one will be provided manually.  This is used for `Expr`, for example, since an `Expr` could in
  fact be a capturing pattern.

- `#[match=eq]`: On a top-level declaration, generate an `impl` that dispatches to `==`.
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
            yield '    mcx.try_match(%s1, %s2)?;' % \
                    (f.name, f.name)
        yield '    Ok(())'
        yield '  }'
    yield '  (_, _) => Err(matcher::Error::VariantMismatch),'
    yield '}'

@linewise
def compare_impl(se):
    yield 'impl TryMatch for %s {' % se.name
    yield '  fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {'
    yield indent(do_match(se, 'self', 'target'), '    ')
    yield '  }'
    yield '}'

@linewise
def eq_impl(d):
    yield 'impl TryMatch for %s {' % d.name
    yield '  fn try_match(&self, target: &Self, _mcx: &mut MatchCtxt) -> matcher::Result {'
    yield '    if self == target {'
    yield '      Ok(())'
    yield '    } else {'
    yield '      Err(matcher::Error::VariantMismatch)'
    yield '    }'
    yield '  }'
    yield '}'

@linewise
def ignore_impl(d):
    yield 'impl TryMatch for %s {' % d.name
    yield '  fn try_match(&self, target: &Self, _mcx: &mut MatchCtxt) -> matcher::Result {'
    yield '    Ok(())'
    yield '  }'
    yield '}'

@linewise
def custom_impl(se):
    yield 'fn default_try_match_%s(this: &%s, target: &%s, mcx: &mut MatchCtxt) -> matcher::Result {' % \
            (camel(se.name), se.name, se.name)
    yield indent(do_match(se, 'this', 'target'), '  ')
    yield '}'

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        mode = d.attrs.get('match')
        if mode is None:
            if isinstance(d, (Struct, Enum)):
                mode = 'compare'
            else:
                mode = 'ignore'

        if mode == 'compare':
            yield compare_impl(d)
        elif mode == 'eq':
            yield eq_impl(d)
        elif mode == 'ignore':
            yield ignore_impl(d)
        elif mode == 'custom':
            if isinstance(d, (Struct, Enum)):
                yield custom_impl(d)
            # Otherwise, we don't need to provide a default fn
