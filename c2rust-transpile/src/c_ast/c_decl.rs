use crate::c_ast::c_expr::{CExprId, ConstIntExpr};
use crate::c_ast::c_stmt::CStmtId;
use crate::c_ast::c_type::{CFuncTypeId, CQualTypeId, CTypeKind};
use crate::c_ast::{Attribute, Located, SrcLoc, TypedAstContext};
use indexmap::{IndexMap, IndexSet};
use std::fmt::Debug;
use std::ops::Index;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CDeclId(pub u64);

// These are references into particular variants of AST nodes
pub type CFieldId = CDeclId; // Records always contain 'DeclKind::Field's
pub type CParamId = CDeclId; // Parameters always contain 'DeclKind::Variable's
pub type CRecordId = CDeclId; // Record types need to point to 'DeclKind::Record'
pub type CTypedefId = CDeclId; // Typedef types need to point to 'DeclKind::Typedef'
pub type CEnumId = CDeclId; // Enum types need to point to 'DeclKind::Enum'
pub type CEnumConstantId = CDeclId; // Enum's need to point to child 'DeclKind::EnumConstant's

#[derive(Debug, Clone)]
pub enum CDeclKind {
    // http://clang.llvm.org/doxygen/classclang_1_1FunctionDecl.html
    Function {
        is_global: bool,
        is_inline: bool,
        is_implicit: bool,
        is_extern: bool,
        is_inline_externally_visible: bool,
        typ: CFuncTypeId,
        name: String,
        parameters: Vec<CParamId>,
        body: Option<CStmtId>,
        attrs: IndexSet<Attribute>,
    },

    // http://clang.llvm.org/doxygen/classclang_1_1VarDecl.html
    Variable {
        has_static_duration: bool,
        has_thread_duration: bool,
        is_externally_visible: bool,
        is_defn: bool,
        ident: String,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
        attrs: IndexSet<Attribute>,
    },

    // Enum (http://clang.llvm.org/doxygen/classclang_1_1EnumDecl.html)
    Enum {
        name: Option<String>,
        variants: Vec<CEnumConstantId>,
        integral_type: Option<CQualTypeId>,
    },

    EnumConstant {
        name: String,
        value: ConstIntExpr,
    },

    // Typedef
    Typedef {
        name: String,
        typ: CQualTypeId,
        is_implicit: bool,
        target_dependent_macro: Option<String>,
    },

    // Struct
    Struct {
        name: Option<String>,
        fields: Option<Vec<CFieldId>>,
        is_packed: bool,
        manual_alignment: Option<u64>,
        max_field_alignment: Option<u64>,
        platform_byte_size: u64,
        platform_alignment: u64,
    },

    // Union
    Union {
        name: Option<String>,
        fields: Option<Vec<CFieldId>>,
        is_packed: bool,
    },

    // Field
    Field {
        name: String,
        typ: CQualTypeId,
        bitfield_width: Option<u64>,
        platform_bit_offset: u64,
        platform_type_bitwidth: u64,
    },

    MacroObject {
        name: String,
        // replacements: Vec<CExprId>,
    },

    MacroFunction {
        name: String,
        // replacements: Vec<CExprId>,
    },

    NonCanonicalDecl {
        canonical_decl: CDeclId,
    },

    StaticAssert {
        assert_expr: CExprId,
        message: Option<String>,
    },
}

pub type CDecl = Located<CDeclKind>;

impl CDeclKind {
    pub fn get_name(&self) -> Option<&String> {
        use CDeclKind::*;
        Some(match self {
            Function { name: i, .. } => i,
            Variable { ident: i, .. } => i,
            Typedef { name: i, .. } => i,
            EnumConstant { name: i, .. } => i,
            Enum { name: Some(i), .. } => i,
            Struct { name: Some(i), .. } => i,
            Union { name: Some(i), .. } => i,
            Field { name: i, .. } => i,
            MacroObject { name, .. } => name,
            _ => return None,
        })
    }
}

/// The range of source code locations for a C declaration.
#[derive(Copy, Clone)]
pub struct CDeclSrcRange {
    /// The earliest position where this declaration or its documentation might start.
    pub earliest_begin: SrcLoc,
    /// A position by which this declaration itself is known to have begun.
    /// Attributes or return type may possibly precede this position.
    pub strict_begin: SrcLoc,
    /// The end of the declaration, except for possible trailing semicolon.
    pub end: SrcLoc,
}

impl TypedAstContext {
    /// Construct a map from top-level decls in the main file to their source ranges.
    pub fn top_decl_locs(&self) -> IndexMap<CDeclId, CDeclSrcRange> {
        let mut name_loc_map = IndexMap::new();
        let mut prev_end_loc = SrcLoc {
            fileid: 0,
            line: 0,
            column: 0,
        };
        // Sort decls by source location so we can reason about the possibly comment-containing gaps
        // between them.
        let mut decls_sorted = self.c_decls_top.clone();
        // Break ties in `begin_loc` (e.g. from `int a, b;`) using `end_loc`.
        decls_sorted
            .sort_by_key(|decl| (self.c_decls[decl].begin_loc(), self.c_decls[decl].end_loc()));
        for decl_id in &decls_sorted {
            let decl = &self.c_decls[decl_id];
            let begin_loc: SrcLoc = decl.begin_loc().expect("no begin loc for top-level decl");
            let end_loc: SrcLoc = decl.end_loc().expect("no end loc for top-level decl");

            // Skip fileid 0; this is not a real file, so these source locations aren't important.
            if begin_loc.fileid == 0 {
                continue;
            }
            if begin_loc == end_loc {
                log::warn!(
                    "zero-length source range for top-level decl; skipping. source ranges for \
                    top-level decls may be incorrect.\ndecl: {decl:?}"
                );
                continue;
            }

            // If encountering a new file, reset end of last top-level decl.
            if prev_end_loc.fileid != begin_loc.fileid {
                prev_end_loc = SrcLoc {
                    fileid: begin_loc.fileid,
                    line: 1,
                    column: 1,
                }
            }

            // This definition ends before the previous one does, i.e. it is nested.
            // This does not generally occur for regular definitions, e.g. variables within
            // functions, because the variables will not be top-level decls. But it can occur
            // for macros defined inside functions, since all macros are top-level decls!
            let is_nested = end_loc < prev_end_loc;

            // Clang emits declarations of builtins (and opaque types such as when encountering
            // `struct undeclared *`) at their first usage site. This means that they are usually
            // nested within another function, and (at least with how the C AST is currently
            // exported) they have an end location with line and column zero. Fix this up before
            // continuing to maintain the invariant that begin is not ordered after end.
            if is_nested
                && end_loc.line == 0
                && end_loc.column == 0
                && !(begin_loc.line == 0 && begin_loc.column == 0)
            {
                log::debug!("skipping nested decl with zero end line/col: {decl:?}");
                continue;
            }

            // If the beginning is not ordered after the end, skip this decl and warn.
            if begin_loc > end_loc {
                log::warn!(
                    "backward source range for top-level decl; skipping. source ranges for \
                    top-level decls may be incorrect.\ndecl: {decl:?}"
                );
                continue;
            }

            // End of the previous decl is the start of comments pertaining to the current one.
            let earliest_begin_loc = if is_nested { begin_loc } else { prev_end_loc };

            // Include only decls from the main file.
            if self.c_decls_top.contains(decl_id)
                && self.get_source_path(decl) == Some(&self.main_file)
            {
                // For multiple decls, e.g. `int a, b;`, `begin_loc` is shared, in which case it is
                // earlier than `earliest_begin_loc` for decls after the first; to maintain their
                // relative order we must either move `earliest_begin_loc` earlier or move
                // `begin_loc` later.
                // For now, we move `begin_loc` later, so that the range used by each variable from
                // a multiple decl does not overlap the others. If other tooling would benefit more
                // from maximal but overlapping ranges, we could go the other way.
                let begin_loc = begin_loc.max(earliest_begin_loc);
                let entry = CDeclSrcRange {
                    earliest_begin: earliest_begin_loc,
                    strict_begin: begin_loc,
                    end: end_loc,
                };
                name_loc_map.insert(*decl_id, entry);
            }
            if !is_nested {
                prev_end_loc = end_loc;
            }
        }
        name_loc_map
    }

    pub fn iter_decls(&self) -> indexmap::map::Iter<'_, CDeclId, CDecl> {
        self.c_decls.iter()
    }

    pub fn iter_mut_decls(&mut self) -> indexmap::map::IterMut<'_, CDeclId, CDecl> {
        self.c_decls.iter_mut()
    }

    pub fn get_decl(&self, key: &CDeclId) -> Option<&CDecl> {
        self.c_decls.get(key)
    }

    /// Return the list of types for a list of declared function parameters.
    ///
    /// Returns `None` if one of the parameters is not a `CDeclKind::Variable`, e.g. if it was not a
    /// function parameter but actually some other kind of declaration.
    pub fn tys_of_params(&self, parameters: &[CDeclId]) -> Option<Vec<CQualTypeId>> {
        parameters
            .iter()
            .map(|p| match self.index(*p).kind {
                CDeclKind::Variable { typ, .. } => Some(CQualTypeId::new(typ.ctype)),
                _ => None,
            })
            .collect()
    }

    /// Return the most precise possible CTypeKind for the given function declaration.
    /// Specifically, ensures that arguments' types are not resolved to underlying types if they were
    /// declared as typedefs, but returned as those typedefs.
    ///
    /// The passed CDeclId must refer to a function declaration.
    pub fn fn_decl_ty_with_declared_args(&self, func_decl: &CDeclKind) -> CTypeKind {
        if let CDeclKind::Function {
            typ, parameters, ..
        } = func_decl
        {
            let typ = self.resolve_type_id(*typ);
            let decl_arg_tys = self.tys_of_params(parameters).unwrap();
            let typ_kind = &self[typ].kind;
            if let &CTypeKind::Function(ret, ref _arg_tys, a, b, c) = typ_kind {
                return CTypeKind::Function(ret, decl_arg_tys, a, b, c);
            }
            panic!("expected {typ:?} to be CTypeKind::Function, but it was {typ_kind:?}")
        }
        panic!("expected a CDeclKind::Function, but passed {func_decl:?}")
    }

    pub fn has_inner_struct_decl(&self, decl_id: CDeclId) -> bool {
        matches!(
            self.index(decl_id).kind,
            CDeclKind::Struct {
                manual_alignment: Some(_),
                ..
            }
        )
    }

    pub fn is_packed_struct_decl(&self, decl_id: CDeclId) -> bool {
        use CDeclKind::*;
        matches!(
            self.index(decl_id).kind,
            Struct {
                is_packed: true,
                ..
            } | Struct {
                max_field_alignment: Some(_),
                ..
            }
        )
    }
}

impl Index<CDeclId> for TypedAstContext {
    type Output = CDecl;

    fn index(&self, index: CDeclId) -> &CDecl {
        match self.c_decls.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
        }
    }
}
