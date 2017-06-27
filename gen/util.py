import functools

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

RUST_KEYWORDS = (
        'const',
        'else',
        'mod',
        'mut',
        'self',
        'type',
        'unsafe',
        )


