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
def struct_fields(fields, suffix, bind_mode):
    for f in fields:
        yield '%s: %s%s%s' % (f.name, bind_mode, f.name, suffix)

@comma_sep
def tuple_fields(fields, suffix, bind_mode):
    for f in fields:
        yield '%s%s%s' % (bind_mode, f.name, suffix)

def struct_pattern(s, path, suffix='', bind_mode='ref '):
    if not s.is_tuple:
        return '%s { %s }' % (path, struct_fields(s.fields, suffix, bind_mode))
    else:
        if len(s.fields) == 0:
            return path
        else:
            return '%s(%s)' % (path, tuple_fields(s.fields, suffix, bind_mode))


CAPS_RE = re.compile(r'[A-Z]')

def snake(s):
    s = CAPS_RE.sub(lambda m: '_' + m.group().lower(), s)
    if s.startswith('_'):
        s = s[1:]
    return s

