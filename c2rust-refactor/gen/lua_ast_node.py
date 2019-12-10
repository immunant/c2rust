'''This module generates `LuaAstNode` impls for each AST node.

Attributes:

- `#[fold_kind=FooKind]`: Folds the kind enum with the given name
  into the current structure.

- `#[boxed]`: marks nodes that are wrapper in `P<T>` smart pointers,
  e.g., `P<Expr>` or `P<Ty>`.

- `#[boxed=both]`: marks nodes that are used both boxed and unboxed,
  e.g., `UseTree`.

- `#[to_lua_custom]`: implements `ToLuaExt` and `UserData` separately.
'''

from datetime import datetime
from textwrap import indent, dedent

from ast import *
from util import *

@linewise
def do_child_method(s, match_pat, method_name, args, default_value, bind_mode, out_fn):
    arg_names = ', '.join(name for name, ty in args)
    arg_types = ', '.join(ty for name, ty in args)
    yield '    methods.add_method("%s", |lua_ctx, this, (idx, %s): (Value, %s)| {' % (method_name, arg_names, arg_types)
    yield '      match idx {'
    # Emit integer indices first (but only if there are any tuple variants)
    if any(v.is_tuple and len(v.fields) > 0 for v in s.variants):
        yield '        Value::Integer(idx) => match (%s, idx) {' % match_pat
        for v in s.variants:
            if not v.is_tuple:
                continue
            for idx, f in enumerate(v.fields):
                fpat = struct_pattern(v, '%s::%s' % (s.name, v.name), bind_mode=bind_mode)
                yield '           (%s, %d) => %s' % (fpat, (idx + 1), out_fn(f))
        yield '           _ => %s' % default_value
        yield '        }'

    # Emit string indices (for non-tuple variants)
    if any(not v.is_tuple and len(v.fields) > 0 for v in s.variants):
        yield '        Value::String(idx) => match (%s, idx.to_str()?) {' % match_pat
        for v in s.variants:
            if v.is_tuple:
                continue
            for f in v.fields:
                fpat = struct_pattern(v, '%s::%s' % (s.name, v.name), bind_mode=bind_mode)
                yield '          (%s, "%s") => %s' % (fpat, f.name, out_fn(f))
        yield '          _ => %s' % default_value
        yield '        }'

    # Otherwise, just return the default value
    yield '        _ => %s' % default_value
    yield '      }'
    yield '    });'


@linewise
def do_enum_variants(s, match_pats, emit_ldoc):
    # Emit `children`
    if emit_ldoc:
        yield '    /// Return the children of the current node'
        yield '    // @function children'
        yield '    // @treturn Vec<LuaAstNode> array containing the children'
    yield '    methods.add_method("children", |lua_ctx, this, ()| {'
    yield '      let table = lua_ctx.create_table()?;'
    yield '      match %s {' % match_pats[0]
    for v in s.variants:
        fpat = struct_pattern(v, '%s::%s' % (s.name, v.name))
        yield '        %s => {' % fpat
        if v.is_tuple:
            for i, f in enumerate(v.fields):
                yield '          table.set(%d, %s.clone().to_lua_ext(lua_ctx)?)?;' % (i + 1, f.name)
        else:
            for f in v.fields:
                yield '          table.set("%s", %s.clone().to_lua_ext(lua_ctx)?)?;' % (f.name, f.name)
        yield '        }'
    yield '      };'
    yield '      Ok(Value::Table(table))'
    yield '    });'

    # Emit `child`
    if emit_ldoc:
        yield '    /// Return a copy of the given child'
        yield '    // @function child'
        yield '    // @param idx the index of the child. Can be an integer or a string'
        yield '    // @treturn LuaAstNode the child'
    yield do_child_method(s, match_pats[0], 'child',
        [], 'Ok(Value::Nil)', 'ref ',
        lambda field: '%s.clone().to_lua_ext(lua_ctx),' % field.name)

    # Emit `replace_child`
    if emit_ldoc:
        yield '    /// Replace a child of the current node with a new value'
        yield '    // @function replace_child'
        yield '    // @param idx the index of the child. Can be an integer or a string'
        yield '    // @param value the replacement value. Can be a LuaAstNode or a direct Lua representation'
    yield do_child_method(s, match_pats[1], 'replace_child',
        [('value', 'Value')], 'Ok(())', 'ref mut ',
        lambda field: '{ *%s = FromLuaExt::from_lua_ext(value, lua_ctx)?; Ok(()) }' % field.name)

@linewise
def do_one_impl(s, kind_map, boxed, emit_ldoc):
    type_name = 'P<%s>' % s.name if boxed else s.name
    yield 'unsafe impl Send for LuaAstNode<%s> {}' % type_name
    yield 'impl LuaAstNodeSafe for LuaAstNode<%s> {}' % type_name
    yield 'impl UserData for LuaAstNode<%s> {' % type_name
    yield '  #[allow(unused, non_shorthand_field_patterns)]'
    yield '  fn add_methods<\'lua, M: UserDataMethods<\'lua, Self>>(methods: &mut M) {'
    if isinstance(s, Struct):
        # FIXME: handle tuple struct
        kind_field = find_kind_field(s)
        for f in s.fields:
            if emit_ldoc:
                yield '    /// Return the "%s" field of the current node' % f.name
                yield '    // @function get_%s' % f.name
                yield '    // @treturn LuaAstNode the field'
            yield '    methods.add_method("get_%s", |lua_ctx, this, ()| {' % f.name
            yield '      this.borrow().%s.clone().to_lua_ext(lua_ctx)' % f.name
            yield '    });'
            if emit_ldoc:
                yield '    /// Set the "%s" field of the current node to a new value' % f.name
                yield '    // @function set_%s' % f.name
                yield '    // @param value the replacement value. Can be a LuaAstNode or a direct Lua representation'
            yield '    methods.add_method("set_%s", |lua_ctx, this, (value,)| {' % f.name
            yield '      this.borrow_mut().%s = FromLuaExt::from_lua_ext(value, lua_ctx)?;' % f.name
            yield '      Ok(())'
            yield '    });'

        if 'fold_kind' in s.attrs:
            assert kind_field is not None
            # Emit a getter for the folded kind's name
            if emit_ldoc:
                yield '    /// Return the kind of the current node as a string'
                yield '    // @function %s_name' % kind_field
                yield '    // @treturn string string representation of the kind'
            yield '    methods.add_method("%s_name", |_lua_ctx, this, ()| {' % kind_field
            yield '      Ok(this.borrow().%s.ast_name())' % kind_field
            yield '    });'

            kind_name = s.attrs['fold_kind']
            kind_decl = kind_map[kind_name]
            match_pats = ('&this.borrow().%s' % kind_field,
                          '&mut this.borrow_mut().%s' % kind_field)
            yield do_enum_variants(kind_decl, match_pats, emit_ldoc)

    elif isinstance(s, Enum):
        if emit_ldoc:
            yield '    /// Return the kind of the current node as a string'
            yield '    // @function kind_name'
            yield '    // @treturn string string representation of the kind'
        yield '    methods.add_method("kind_name", |_lua_ctx, this, ()| {'
        yield '      Ok(this.borrow().ast_name())'
        yield '    });'
        imm_box_prefix = '&**' if boxed else '&*'
        mut_box_prefix = '&mut **' if boxed else '&mut *'
        match_pats = (imm_box_prefix + 'this.borrow()',
                      mut_box_prefix + 'this.borrow_mut()')
        yield do_enum_variants(s, match_pats, emit_ldoc)

    if 'no_debug' not in s.attrs:
        yield '    methods.add_meta_method('
        yield '      MetaMethod::ToString,'
        yield '      |_lua_ctx, this, ()| Ok(format!("{:?}", this.borrow())),'
        yield '    );'

    yield '    <Self as AddMoreMethods>::add_more_methods(methods);'
    yield '  }'
    yield '}'

    yield 'impl FromLuaTable for %s {' % type_name
    yield '  fn from_lua_table<\'lua>(_table: LuaTable<\'lua>, _lua_ctx: Context<\'lua>) -> Result<Self> {'
    yield '    let _kind = _table.get::<_, LuaString>(1)?;'
    yield '    let _kind = _kind.to_str()?;'
    if isinstance(s, Struct):
        yield '    if _kind == "%s" {' % s.name
        yield '      Ok(%s%s {' % ('P(' if boxed else '', s.name)
        for f in s.fields:
            yield '        %s: from_lua_prepend_field("%s", FromLuaExt::from_lua_ext(_table.get::<_, Value>("%s")?, _lua_ctx))?,' % (f.name, f.name, f.name)
            # TODO: kind folding???

        yield '      }%s)' % (')' if boxed else '')
        yield '    } else {'
        yield '      from_lua_kind_error("%s", _kind)' % s.name
        yield '    }'

    elif isinstance(s, Enum):
        yield '    match _kind {'
        for v in s.variants:
            delim_open, delim_close = ('', '') if len(v.fields) == 0 else (
                ('(', ')') if v.is_tuple else ('{', '}'))

            box_open, box_close = ('P(', ')') if boxed else ('', '')
            yield '      "%s" => Ok(%s%s::%s%s' % (v.name, box_open, s.name, v.name, delim_open)
            if v.is_tuple:
                for i, f in enumerate(v.fields):
                    yield '        FromLuaExt::from_lua_ext(_table.get::<_, Value>(%d)?, _lua_ctx)?,' % (i + 2)
            else:
                for f in v.fields:
                    yield '        %s: FromLuaExt::from_lua_ext(_table.get::<_, Value>("%s")?, _lua_ctx)?,' % (f.name, f.name)
            yield '      %s%s),' % (delim_close, box_close)

        yield '      _ => from_lua_kind_error("%s", _kind)' % s.name
        yield '    }'

    else:
        # TODO: add a message
        yield '    Err(Error::FromLuaConversionError { from: "Table", to: "%s", message: None })' % s.name

    yield '  }'
    yield '}'

    # FromLuaString implementation for enums
    if isinstance(s, Enum):
        yield 'impl TryFromString for %s {' % type_name
        yield '  fn try_from_string<\'lua>(_str: &str) -> Option<Self> {'
        yield '    match _str {'
        for v in s.variants:
            if len(v.fields) == 0:
                fpat = struct_pattern(v, '%s::%s' % (s.name, v.name))
                if boxed:
                    fpat = 'P(%s)' % fpat
                yield '      "%s" => Some(%s),' % (v.name, fpat)
        yield '      _ => None'
        yield '    }'
        yield '  }'
        yield '}'


@linewise
def do_impl(s, kind_map):
    yield '/// %s AST node handle' % s.name
    yield '// This object is NOT thread-safe. Do not use an object of this class from a'
    yield '// thread that did not acquire it.'
    yield '// @type %s' % s.name
    if 'boxed' in s.attrs:
        yield do_one_impl(s, kind_map, True, True)
    if 'boxed' not in s.attrs or s.attrs['boxed'] == 'both':
        # Don't emit the ldoc tags a second time if we already did it above
        emit_ldoc = s.attrs.get('boxed') != 'both'
        yield do_one_impl(s, kind_map, False, emit_ldoc)

@linewise
def generate(decls):
    yield '// AUTOMATICALLY GENERATED - DO NOT EDIT'
    yield '// Produced %s by process_ast.py' % (datetime.now(),)
    yield ''
    yield '/// Refactoring module'
    yield '// @module Refactor'
    yield ''

    kind_map = {}
    for d in decls:
        if 'fold_kind' in d.attrs:
            kind_name = d.attrs['fold_kind']
            kind_map[kind_name] = None

    for d in decls:
        if d.name in kind_map:
            kind_map[d.name] = d

    for d in decls:
        if 'to_lua_custom' in d.attrs:
            continue

        yield do_impl(d, kind_map)

