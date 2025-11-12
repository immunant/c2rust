#!/usr/bin/env -S uv run

from collections import namedtuple
import re
import sys

from ast import *


TOKEN_RE = re.compile(r'''
        (?P<keyword>
            struct |
            enum |
            flag
        ) |
        (?P<symbol> [{}()\[\],;#=] ) |
        (?P<ident> [a-zA-Z_0-9]+ ) |
        (?P<litstr> '[^']*' )
        ''', re.VERBOSE)

SPACE_RE = re.compile(r'(\s|//[^\n]*)*')

Keyword = namedtuple('Keyword', ('text'))
Symbol = namedtuple('Symbol', ('text'))
Ident = namedtuple('Ident', ('text'))
LitStr = namedtuple('LitStr', ('text'))
EOF = namedtuple('EOF', ())

def tokenize(s):
    tokens = []
    i = 0
    while i < len(s):
        m = SPACE_RE.match(s, i)
        i = m.end()

        if i >= len(s):
            break

        m = TOKEN_RE.match(s, i)
        if m is None or not m.group():
            raise ValueError('bad input at offset %d: %r' % (i, s[i:i + 10] + '...'))
        elif m.group('keyword'):
            tokens.append(Keyword(m.group('keyword')))
        elif m.group('symbol'):
            tokens.append(Symbol(m.group('symbol')))
        elif m.group('ident'):
            tokens.append(Ident(m.group('ident')))
        elif m.group('litstr'):
            tokens.append(LitStr(m.group('litstr').strip("'")))
        else:
            assert False
        i = m.end()

    return tokens


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def eof(self):
        return self.pos >= len(self.tokens)

    def peek(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        else:
            return EOF()

    def take(self):
        t = self.peek()
        self.pos += 1
        return t


    def take_type(self, ty):
        t = self.take()
        if not isinstance(t, ty):
            raise ValueError('expected %s, but got %s' % (ty.__name__, t,))
        return t.text

    def take_keyword(self):
        return self.take_type(Keyword)

    def take_symbol(self):
        return self.take_type(Symbol)

    def take_ident(self):
        return self.take_type(Ident)

    def take_symbol_from(self, ss):
        s = self.take_type(Symbol)
        if s not in ss:
            raise ValueError('expected one of %s, but got %s' % (list(ss), s))
        return s


    def peek_type(self, ty):
        t = self.peek()
        if not isinstance(t, ty):
            return None
        return t.text

    def peek_symbol(self):
        return self.peek_type(Symbol)

    def peek_ident(self):
        return self.peek_type(Ident)


    def parse_decl(self):
        attrs = self.parse_attrs()
        kw = self.take_keyword()
        if kw == 'struct':
            return self.parse_struct(attrs, top_level=True)
        elif kw == 'enum':
            return self.parse_enum(attrs)
        elif kw == 'flag':
            return self.parse_flag(attrs)

    def parse_decls(self):
        decls = []
        while not self.eof():
            decls.append(self.parse_decl())
        return decls

    def parse_attrs(self):
        attrs = {}
        while self.peek_symbol() == '#':
            self.take()
            self.take_symbol_from('[')
            key = self.take_ident()
            if self.peek_symbol() == '=':
                self.take()
                value = self.take_type((Ident, LitStr))
            else:
                value = ''
            self.take_symbol_from(']')
            attrs[key] = value
        return attrs

    def parse_struct(self, attrs, top_level=False):
        name = self.take_ident()

        start_delim = self.peek_symbol()
        if start_delim == '(':
            self.take()
            end_delim = ')'
            is_tuple = True
        elif start_delim == '{':
            self.take()
            end_delim = '}'
            is_tuple = False
        else:
            # Nullary tuple struct, with no parens
            return Struct(name, [], True, attrs)

        fields = self.parse_fields(end_delim, is_tuple)
        self.take_symbol_from(end_delim)
        if top_level and is_tuple:
            self.take_symbol_from(';')

        return Struct(name, fields, is_tuple, attrs)

    def parse_enum(self, enum_attrs):
        name = self.take_ident()
        self.take_symbol_from('{')

        variants = []
        while self.peek_symbol() != '}':
            attrs = self.parse_attrs()
            variants.append(self.parse_struct(attrs))
            if self.peek_symbol() == ',':
                self.take()
            else:
                break

        self.take_symbol_from('}')

        return Enum(name, variants, enum_attrs)

    def parse_flag(self, attrs):
        name = self.take_ident()
        self.take_symbol_from(';')
        return Flag(name, attrs)

    def parse_fields(self, end_delim, is_tuple):
        fields = []
        while self.peek_symbol() != end_delim:
            attrs = self.parse_attrs()
            name = self.take_ident()
            dot_name = str(len(fields)) if is_tuple else name
            fields.append(Field(name, dot_name, attrs))
            if self.peek_symbol() == ',':
                self.take()
            else:
                break
        return fields

def parse(s):
    t = tokenize(s)
    p = Parser(t)
    return p.parse_decls()


if __name__ == '__main__':
    decls = parse(open('gen/ast.txt').read())

    mode, out_file = sys.argv[1:]

    if mode == 'ast_deref':
        import ast_deref
        text = ast_deref.generate(decls)
    elif mode == 'ast_equiv':
        import ast_equiv
        text = ast_equiv.generate(decls)
    elif mode == 'matcher':
        import matcher
        text = matcher.generate(decls)
    elif mode == 'get_span':
        import get_span
        text = get_span.generate(decls)
    elif mode == 'get_node_id':
        import get_node_id
        text = get_node_id.generate(decls)
    elif mode == 'lr_expr':
        import lr_expr
        text = lr_expr.generate(decls)
    elif mode == 'list_node_ids':
        import list_node_ids
        text = list_node_ids.generate(decls)
    elif mode == 'rewrite_rewrite':
        import rewrite
        text = rewrite.generate_rewrite_impls(decls)
    elif mode == 'rewrite_recursive':
        import rewrite
        text = rewrite.generate_recursive_impls(decls)
    elif mode == 'rewrite_recover_children':
        import rewrite
        text = rewrite.generate_recover_children_impls(decls)
    elif mode == 'rewrite_seq_item':
        import rewrite
        text = rewrite.generate_seq_item_impls(decls)
    elif mode == 'rewrite_maybe_rewrite_seq':
        import rewrite
        text = rewrite.generate_maybe_rewrite_seq_impls(decls)
    elif mode == 'mac_table':
        import mac_table
        text = mac_table.generate(decls)
    elif mode == 'nt_match':
        import nt_match
        text = nt_match.generate(decls)
    elif mode == 'ast_names':
        import ast_names
        text = ast_names.generate(decls)
    else:
        raise ValueError('unknown mode: %r' % mode)

    with open(out_file, 'w') as f:
        f.write(text)
        f.write('\n')
