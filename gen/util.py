import functools
import re


def linewise(f):
    @functools.wraps(f)
    def g(*args, **kwargs):
        return '\n'.join(f(*args, **kwargs))
    return g

def comma_sep(f):
    @functools.wraps(f)
    def g(*args, **kwargs):
        return ', '.join(f(*args, **kwargs))
    return g

def wordwise(f):
    @functools.wraps(f)
    def g(*args, **kwargs):
        return ' '.join(f(*args, **kwargs))
    return g


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


CAPS_RE = re.compile(r'[A-Z]')

def camel(s):
    s = CAPS_RE.sub(lambda m: '_' + m.group().lower(), s)
    if s.startswith('_'):
        s = s[1:]
    return s

