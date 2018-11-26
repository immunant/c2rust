import pygments.formatters
import pygments.lexers
import pygments.token


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
