'''This module generates `GetSpan` impls for each AST node type that has a span.

- Structs with a field named `span` or a field with the `#[span]` attribute get
  a `GetSpan` impl that returns that span.
- Enums and flags do not have spans and thus do not get impls.

Attributes:

- `#[span]`: On a struct field, causes the field to be treated as the struct's
  span, even if its name is not `span`.  Only one field in a given struct can
  have this attribute.

- `#[no_span]`: On a struct, causes no impl to be generated, even if the struct
  has a field named `span`.

- `#[extend_span=field_name]`: Indicates that the struct's span should be
  extended to cover its attributes, which are stored in the indicated field.
  The field name can be omitted, in which case it defaults to `attrs`.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *


@linewise
def do_impl(s, field_name):
    yield 'impl GetSpan for %s {' % s.name
    yield '  fn get_span(&self) -> Span {'
    if 'extend_span' not in s.attrs:
        yield '    self.%s' % field_name
    else:
        attr_field = s.attrs['extend_span'] or 'attrs'
        yield '    extend_span_attrs(self.%s, &self.%s)' % (field_name, attr_field)
    yield '  }'
    yield '}'

def find_span_field(s):
    if 'no_span' in s.attrs:
        return None

    marked_fields = []
    for f in s.fields:
        if 'span' in f.attrs:
            marked_fields.append(f.dot_name)
    if len(marked_fields) == 1:
        return marked_fields[0]
    elif len(marked_fields) > 1:
        raise ValueError('struct %s has %d fields marked #[span] (expected 0 or 1)' %
                (s.name, len(marked_fields)))

    for f in s.fields:
        if f.name == 'span':
            return f.dot_name

    return None

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        if not isinstance(d, Struct):
            continue
        field_name = find_span_field(d)
        if field_name is not None:
            yield do_impl(d, field_name)

def has_get_span_impl(d):
    '''Returns `true` if type `d` implements `GetSpan`; `False` otherwise.'''
    return isinstance(d, Struct) and find_span_field(d) is not None
