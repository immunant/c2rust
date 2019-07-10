from datetime import datetime

from ast import *
from util import *

@linewise
def do_ast_names_impl(d):
    if not isinstance(d, (Struct, Enum)):
        return
    yield '#[allow(unused, non_shorthand_field_patterns)]'
    yield 'impl AstName for %s {' % d.name
    yield '  fn ast_name(&self) -> String {'

    yield '    match self {'
    for v, path in variants_paths(d):
        yield '      &%s => {' % struct_pattern(v, path)
        yield '        "%s".to_string()' % v.name
        if isinstance(d, (Struct)):
            kind_field = find_kind_field(d)
            if kind_field:
                yield '        + ":" + &self.%s.ast_name()' % kind_field
        yield '      }'
    yield '    }'
    yield '  }'
    yield '}'

def find_kind_field(s):
    if 'no_kind' in s.attrs:
        return None

    marked_fields = []
    for f in s.fields:
        if 'kind' in f.attrs:
            marked_fields.append(f.name)
    if len(marked_fields) == 1:
        return marked_fields[0]
    elif len(marked_fields) > 1:
        raise ValueError('struct %s has %d fields marked #[kind] (expected 0 or 1)' %
                (s.name, len(marked_fields)))

    for f in s.fields:
        if f.name == 'node':
            return f.name

    return None

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''

    for d in decls:
        yield do_ast_names_impl(d)

