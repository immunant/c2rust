from typing import Dict, Optional, Any

import pygments.lexers
import pygments.token
from pygments.token import *

from literate.annot import Span, cut_annot
from literate.file import File


def highlight_file(f: File):
    '''Run syntax highlighting on `f`, setting the `highlight` annotation for
    each of its lines.'''
    # Annotate the entire file
    lexer = pygments.lexers.get_lexer_by_name('rust')
    raw_annot = []
    for start, token, token_text in lexer.get_tokens_unprocessed(f.text):
        if token == pygments.token.Whitespace:
            continue
        raw_annot.append(Span(start, start + len(token_text), token))

    # Cut annotations into pieces, one per line.
    for line_span, line_annot in cut_annot(raw_annot, f.line_annot):
        f.lines[line_span.label].set_highlight(line_annot)

    assert all(l.highlight is not None for l in f.lines)


# This is a very rough mapping from pygments token types to HighlightJS CSS
# classes
HLJS_CLASSES = {
    Token:                         None,

    Text:                          None,
    Whitespace:                    None,
    Other:                         None,

    Keyword:                       'keyword',
    Keyword.Constant:              'literal',
    Keyword.Type:                  'built_in',  # Built-in types

    Name.Builtin:                  'built_in',
    Name.Builtin.Pseudo:           'built_in',
    Name.Class:                    'title',
    Name.Constant:                 'title',
    Name.Function:                 'title',
    Name.Namespace:                None,

    Literal:                       'literal',

    String:                        'string',
    String.Doc:                    'comment',   # Doc comments

    Number:                        'number',

    Comment:                       'comment',
}

ACE_CLASSES = {
    Token:                         None,

    Text:                          None,
    Whitespace:                    None,
    Other:                         None,

    Keyword:                       ('keyword',),
    Keyword.Constant:              ('constant', 'language'),    # `true` etc
    Keyword.Pseudo:                ('keyword', 'operator'),     # `&` (type)
    Keyword.Type:                  ('storage', 'type'),     # built-in types

    Name:                          None,
    Name.Attribute:                ('variable', 'other'),   # lifetimes
    Name.Function:                 ('entity', 'name', 'function'),

    Literal:                       None,

    String:                        ('string', 'quoted', 'double'),
    String.Char:                   ('string', 'quoted', 'single'),
    String.Doc:                    ('comment',),
    String.Double:                 ('string', 'quoted', 'double'),
    String.Escape:                 ('constant', 'character', 'escape'),
    String.Single:                 ('string', 'quoted', 'single'),

    Number:                        ('constant', 'numeric'),

    Operator:                      ('keyword', 'operator'),

    Punctuation:                   ('punctuation', 'operator'),

    Comment:                       ('comment'),
    Comment.Multiline:             ('comment', 'block'),
    Comment.Preproc:               None,    # attributes
    Comment.Single:                ('comment', 'line'),

    Generic:                       None,
}

def token_css_class(tok: type(pygments.token.Token),
        mode: str='hljs') -> Optional[str]:
    '''Get the CSS class for a Pygments token type.'''
    # If the token is A.B.C, we first look for A.B.C, then A.B, then A.
    # Everything's a subtype of Token, so eventually we'll get a match.
    if mode == 'hljs':
        classes = HLJS_CLASSES
    elif mode == 'pygments':
        classes = pygments.token.STANDARD_TYPES
    elif mode == 'ace':
        classes = ACE_CLASSES
    else:
        raise ValueError('unknown highlighting mode %r' % mode)

    while tok is not Token:
        if tok in classes:
            if classes[tok] is None:
                return None
            if mode == 'hljs':
                return 'hljs-' + classes[tok]
            elif mode == 'ace':
                cs = classes[tok]
                return ' '.join('ace_%s' % c for c in cs)
            else:
                return classes[tok]
        else:
            tok = tok.parent
    return None


def get_highlight_class(opts: Dict[str, Any]) -> str:
    mode = opts['highlight-mode']
    if mode == 'hljs':
        return ''
    elif mode == 'pygments':
        return 'highlight'
    elif mode == 'ace':
        return 'ace-tm'
