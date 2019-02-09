//! The `Bindings` type, for mapping names to AST fragments.
use std::collections::hash_map::{HashMap, Entry};
use syntax::ast::{Ident, Path, Expr, Pat, Ty, Stmt, Item};
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::{Cursor, Delimited, TokenTree, TokenStream, TokenStreamBuilder};

use crate::ast_manip::AstEquiv;
use c2rust_ast_builder::IntoSymbol;

/// A set of binding types, mapping names to binding types.
#[derive(Clone, Debug)]
pub struct BindingTypes {
    types: HashMap<Symbol, Type>,
}

impl BindingTypes {
    pub fn new() -> BindingTypes {
        BindingTypes {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Symbol) -> Option<&Type> {
        self.types.get(name)
    }

    /// Set the type for a name, so that the name matches (and captures) only nodes of the
    /// appropriate typ.
    pub fn set_type<S: IntoSymbol>(&mut self, name: S, ty: Type) {
        let name = name.into_symbol();
        match self.types.entry(name.into_symbol()) {
            Entry::Vacant(e) => {
                e.insert(ty);
            },
            Entry::Occupied(mut e) => {
                let old_ty = *e.get();
                match (old_ty, ty) {
                    (xty, yty) if xty == yty => {}
                    (_, Type::Unknown) => {}
                    (Type::Unknown, yty) => {
                        e.insert(yty);
                    }
                    _ => {
                        assert!(false, "tried to set type of {:?} to {:?}, but its type is already set to {:?}",
                                name, ty, old_ty);
                    }
                }
            }
        }
    }

    pub fn merge(&mut self, other: Self) {
        for (name, ty) in other.types.into_iter() {
            self.set_type(name, ty);
        }
    }
}

/// A set of bindings, mapping names to AST fragments.
#[derive(Clone, Debug)]
pub struct Bindings {
    map: HashMap<Symbol, Value>,
}

impl Bindings {
    pub fn new() -> Bindings {
        Bindings {
            map: HashMap::new(),
        }
    }

    /// Try to add a binding, mapping `sym` to `val`.  Fails if `sym` is already bound to a value
    /// and that value is not equal to `val`.
    fn try_add<S: IntoSymbol>(&mut self, sym: S, val: Value) -> bool {
        match self.map.entry(sym.into_symbol()) {
            Entry::Vacant(e) => {
                e.insert(val);
                true
            },
            Entry::Occupied(e) => {
                val.ast_equiv(e.get())
            },
        }
    }

    pub fn add_opt_none<S: IntoSymbol>(&mut self, name: S) {
        let name = name.into_symbol();
        let ok = self.try_add_opt_none(name);
        assert!(ok, "cannot alter existing binding {:?}", name);
    }

    pub fn try_add_opt_none<S: IntoSymbol>(&mut self, name: S) -> bool {
        self.try_add(name, Value::Optional(None))
    }
}

macro_rules! define_binding_functions {
    ($Thing:ident($Repr:ty):add_thing=[$add_thing:ident, $try_add_thing:ident]) => {
        pub fn $add_thing<S: IntoSymbol>(&mut self, name: S, val: $Repr) {
            let name = name.into_symbol();
            let ok = self.$try_add_thing(name, val);
            assert!(ok, "cannot alter existing binding {:?}", name);
        }

        pub fn $try_add_thing<S: IntoSymbol>(&mut self, name: S, val: $Repr) -> bool {
            self.try_add(name, Value::$Thing(val))
        }
    };
    ($Thing:ident($Repr:ty):add_opt_thing=[$add_opt_thing:ident, $try_add_opt_thing:ident]) => {
        pub fn $add_opt_thing<S: IntoSymbol>(&mut self, name: S, val: Option<$Repr>) {
            let name = name.into_symbol();
            let ok = self.$try_add_opt_thing(name, val);
            assert!(ok, "cannot alter existing binding {:?}", name);
        }

        pub fn $try_add_opt_thing<S: IntoSymbol>(&mut self, name: S, val: Option<$Repr>) -> bool {
            if let Some(val) = val {
                self.try_add(name, Value::Optional(Some(Box::new(Value::$Thing(val)))))
            } else {
                self.try_add_opt_none(name)
            }
        }
    };
    ($Thing:ident($Repr:ty):thing=[$thing:ident]) => {
        pub fn $thing<S: IntoSymbol>(&self, name: S) -> &$Repr {
            let name = name.into_symbol();
            match self.map.get(&name) {
                Some(&Value::$Thing(ref val)) => val,
                Some(_) => panic!(
                    concat!("found binding for {}, but its type is not ",
                            stringify!($Thing)),
                    name),
                None => panic!("found no binding for {}", name),
            }
        }
    };
    ($Thing:ident($Repr:ty):opt_thing=[$opt_thing:ident]) => {
        pub fn $opt_thing<S: IntoSymbol>(&self, name: S) -> Option<&$Repr> {
            let name = name.into_symbol();
            match self.map.get(&name) {
                Some(&Value::Optional(Some(box Value::$Thing(ref val)))) => Some(val),
                Some(&Value::Optional(None)) => None,
                Some(_) => panic!(
                    concat!("found binding for {}, but its type is not ",
                            stringify!($Thing)),
                    name),
                None => panic!("found no binding for {}", name),
            }
        }
    };
    ($Thing:ident($Repr:ty):get_thing=[$get_thing:ident]) => {
        pub fn $get_thing<S: IntoSymbol>(&self, name: S) -> Option<&$Repr> {
            let name = name.into_symbol();
            match self.map.get(&name) {
                Some(&Value::$Thing(ref val)) => Some(val),
                Some(_) => None,
                None => None,
            }
        }
    };
    ($Thing:ident($Repr:ty):get_opt_thing=[$get_opt_thing:ident]) => {
        pub fn $get_opt_thing<S: IntoSymbol>(&self, name: S) -> Option<Option<&$Repr>> {
            let name = name.into_symbol();
            match self.map.get(&name) {
                Some(&Value::Optional(Some(box Value::$Thing(ref val)))) => Some(Some(val)),
                Some(&Value::Optional(None)) => Some(None),
                Some(_) => None,
                None => None,
            }
        }
    };
}

macro_rules! define_binding_values {
    ($( $Thing:ident($Repr:ty) { $($Fn:tt=[$($Fnames:ident),+]);* } )*) => {
        /// An AST fragment, of any of the supported node types.
        #[derive(Clone, Debug)]
        enum Value {
            Optional(Option<Box<Value>>),
            $( $Thing($Repr), )*
        }

        /// The types of AST fragments that can be used in `Bindings`.
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub enum Type {
            Unknown,
            Optional(&'static Type),
            $( $Thing, )*
        }

        static STATIC_TYPES: &[Type] = &[
            Type::Unknown,
            $( Type::$Thing, )*
        ];

        impl Type {
            fn from_ast_ident(ty_ident: Ident) -> Option<Type> {
                match &*ty_ident.as_str() {
                    $(stringify!($Thing) => Some(Type::$Thing),)*
                    _ => None
                }
            }
        }

        impl Bindings {
            $( $( define_binding_functions!($Thing($Repr):$Fn=[$($Fnames),+]); )* )*

            /// Get the type of fragment associated with `name`, if any.
            pub fn get_type<S: IntoSymbol>(&self, name: S) -> Option<Type> {
                self.map.get(&name.into_symbol()).map(|v| {
                    match v {
                        $( &Value::Optional(Some(box Value::$Thing(_))) =>
                           Type::Optional(Type::$Thing.as_static_ref()), )*
                        $( &Value::$Thing(_) => Type::$Thing, )*
                        &Value::Optional(None) => Type::Unknown,
                        &Value::Optional(Some(box Value::Optional(_))) => {
                            panic!("nested Optional values")
                        }
                    }
                })
            }
        }

        impl AstEquiv for Value {
            fn ast_equiv(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (&Value::$Thing(ref x1),
                         &Value::$Thing(ref x2)) => {
                            x1.ast_equiv(x2)
                        },
                        (&Value::$Thing(ref x1),
                         &Value::Optional(Some(box Value::$Thing(ref x2)))) => {
                            x1.ast_equiv(x2)
                        },
                        (&Value::Optional(Some(box Value::$Thing(ref x1))),
                         &Value::$Thing(ref x2)) => {
                            x1.ast_equiv(x2)
                        },
                        (&Value::Optional(Some(box Value::$Thing(ref x1))),
                         &Value::Optional(Some(box Value::$Thing(ref x2)))) => {
                            x1.ast_equiv(x2)
                        },
                    )*
                    (&Value::Optional(None), &Value::Optional(None)) => true,
                    (_, _) => false,
                }
            }
        }
    };
}

// To allow bindings to contain more types of AST nodes, add more lines to this macro.
define_binding_values! {
    Ident(Ident) {
        add_thing=[add_ident, try_add_ident];
        add_opt_thing=[add_opt_ident, try_add_opt_ident];
        thing=[ident];
        opt_thing=[opt_ident];
        get_thing=[get_ident];
        get_opt_thing=[get_opt_ident]
    }
    Path(Path) {
        add_thing=[add_path, try_add_path];
        thing=[path];
        get_thing=[get_path]
    }
    Expr(P<Expr>) {
        add_thing=[add_expr, try_add_expr];
        thing=[expr];
        get_thing=[get_expr]
    }
    Pat(P<Pat>) {
        add_thing=[add_pat, try_add_pat];
        thing=[pat];
        get_thing=[get_pat]
    }
    Ty(P<Ty>) {
        add_thing=[add_ty, try_add_ty];
        thing=[ty];
        get_thing=[get_ty]
    }
    Stmt(Stmt) {
        add_thing=[add_stmt, try_add_stmt];
        thing=[stmt];
        get_thing=[get_stmt]
    }
    MultiStmt(Vec<Stmt>) {
        add_thing=[add_multi_stmt, try_add_multi_stmt];
        thing=[multi_stmt];
        get_thing=[get_multi_stmt]
    }
    Item(P<Item>) {
        add_thing=[add_item, try_add_item];
        thing=[item];
        get_thing=[get_item]
    }
}

impl Type {
    fn as_static_ref(self) -> &'static Self {
        for ty in STATIC_TYPES {
            if *ty == self {
                return ty;
            }
        }
        panic!("as_static_ref() for invalid type: {:?}", self);
    }
}

fn maybe_get_type(c: &mut Cursor) -> Type {
    let mut c_idx = 0;
    if let Some(TokenTree::Token(_, Token::Colon)) = c.look_ahead(c_idx) {
        c_idx += 1;
        let is_optional = match c.look_ahead(c_idx) {
            Some(TokenTree::Token(_, Token::Question)) => {
                c_idx += 1;
                true
            }
            _ => false
        };
        match c.look_ahead(c_idx) {
            Some(TokenTree::Token(_, Token::Ident(ty_ident, _))) => {
                if let Some(ty) = Type::from_ast_ident(ty_ident) {
                    c.nth(c_idx);
                    if is_optional {
                        return Type::Optional(ty.as_static_ref());
                    } else {
                        return ty;
                    }
                }
            }
            _ => {}
        }
    }
    Type::Unknown
}

/// Rewrite tokens like `$foo:ty` into `$foo` and extract the types
fn rewrite_token_stream(ts: TokenStream, bt: &mut BindingTypes) -> TokenStream {
    let mut tsb = TokenStreamBuilder::new();
    let mut c = ts.into_trees();
    while let Some(tt) = c.next() {
        let new_tt = match tt {
            TokenTree::Token(sp, Token::Dollar) => match c.look_ahead(0) {
                Some(TokenTree::Token(sp, Token::Ident(ident, is_raw))) => {
                    c.next();
                    let dollar_sym = Symbol::intern(&format!("${}", ident));
                    let ident_ty = maybe_get_type(&mut c);
                    bt.set_type(dollar_sym, ident_ty);
                    TokenTree::Token(sp, Token::Ident(Ident::new(dollar_sym, ident.span), is_raw))
                }

                Some(TokenTree::Token(sp, Token::Lifetime(ident))) => {
                    c.next();
                    let ident_str = &*ident.as_str();
                    let (prefix, label) = ident_str.split_at(1);
                    assert!(prefix == "'", "Lifetime identifier does not start with ': {}", ident);
                    let dollar_sym = Symbol::intern(&format!("'${}", label));
                    let label_ty = maybe_get_type(&mut c);
                    bt.set_type(dollar_sym, label_ty);
                    TokenTree::Token(sp, Token::Lifetime(Ident::new(dollar_sym, ident.span)))
                }

                _ => TokenTree::Token(sp, Token::Dollar)
            },

            TokenTree::Delimited(sp, del) => {
                let Delimited { delim, tts } = del;
                let dts = rewrite_token_stream(tts.into(), bt);
                let del = Delimited { delim, tts: dts.into() };
                TokenTree::Delimited(sp, del)
            }

            tt @ _ => tt
        };
        tsb.push(new_tt);
    }
    tsb.build()
}

pub fn parse_bindings(ts: TokenStream) -> (TokenStream, BindingTypes) {
    let mut bt = BindingTypes::new();
    let new_ts = rewrite_token_stream(ts, &mut bt);
    (new_ts, bt)
}
