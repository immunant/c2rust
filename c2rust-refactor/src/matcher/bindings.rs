//! The `Bindings` type, for mapping names to AST fragments.
use std::collections::hash_map::{Entry, HashMap};
use std::convert::{TryFrom, TryInto};

use derive_more::{From, TryInto};
use syntax::ast::{Expr, Ident, Item, Pat, Path, Stmt, Ty};
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::{Cursor, TokenStream, TokenStreamBuilder, TokenTree};

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
            }
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

    /// Try to add a binding, mapping `name` to `val`. Returns false if `name`
    /// is already bound to a value and that value is not equal to `val`.
    pub fn try_add<S, T>(&mut self, name: S, val: T) -> bool
    where
        S: IntoSymbol,
        T: Into<Value>,
    {
        let name = name.into_symbol();
        let val = val.into();
        match self.map.entry(name) {
            Entry::Vacant(e) => {
                e.insert(val);
                true
            }
            Entry::Occupied(e) => val.ast_equiv(e.get()),
        }
    }

    /// Unconditionally add a binding. Panics if the `name` is already bound to
    /// a value and that value is not equal to `val`.
    pub fn add<S, T>(&mut self, name: S, val: T)
    where
        S: IntoSymbol,
        T: Into<Value>,
    {
        let name = name.into_symbol();
        let ok = self.try_add(name, val);
        assert!(ok, "cannot alter existing binding {:?}", name);
    }

    pub fn try_add_none<S>(&mut self, name: S) -> bool
    where
        S: IntoSymbol,
    {
        self.try_add(name.into_symbol(), Value::Optional(None))
    }

    pub fn add_none<S>(&mut self, name: S)
    where
        S: IntoSymbol,
    {
        self.add(name, Value::Optional(None));
    }

    pub fn get<'a, S, T>(&'a self, name: S) -> Option<&'a T>
    where
        S: IntoSymbol,
        &'a T: TryFrom<&'a Value>,
    {
        let name = name.into_symbol();
        self.map.get(&name).and_then(|v| v.try_into().ok())
    }

    pub fn get_opt<'a, S, T>(&'a self, name: S) -> Option<Option<&'a T>>
    where
        S: IntoSymbol,
        &'a T: TryFrom<&'a Value>,
    {
        let name = name.into_symbol();
        self.map.get(&name).and_then(|v| match v {
            Value::Optional(Some(box val)) => Some(val.try_into().ok()),
            Value::Optional(None) => Some(None),
            _ => None,
        })
    }
}

impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(val: Option<T>) -> Value {
        match val {
            Some(v) => Value::Optional(Some(Box::new(v.into()))),
            None => Value::Optional(None),
        }
    }
}

macro_rules! define_binding_values {
    ($( $Thing:ident($Repr:ty) ),*) => {
        /// An AST fragment, of any of the supported node types.
        #[derive(Clone, Debug, From, TryInto)]
        pub enum Value {
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

        static INTERNED_TYPES: &[Type] = &[
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
            /// Get the type of fragment associated with `name`, if any.
            pub fn get_type<S: IntoSymbol>(&self, name: S) -> Option<Type> {
                self.map.get(&name.into_symbol()).map(|v| {
                    match v {
                        $( &Value::Optional(Some(box Value::$Thing(_))) =>
                           Type::Optional(Type::$Thing.interned()), )*
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

        $(
            impl<'a> TryFrom<&'a Value> for &'a $Repr {
                type Error = String;
                fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
                    match value {
                        Value::$Thing(ref x) => Ok(x),
                        _ => Err(format!("Only &Value::{} can be converted to &{}", stringify!($Thing), stringify!($Repr))),
                    }
                }
            }
        )*
    };
}

// To allow bindings to contain more types of AST nodes, add more lines to this macro.
define_binding_values! {
    Ident(Ident),
    Path(Path),
    Expr(P<Expr>),
    Pat(P<Pat>),
    Ty(P<Ty>),
    Stmt(Stmt),
    MultiStmt(Vec<Stmt>),
    Item(P<Item>)
}

impl Type {
    fn interned(self) -> &'static Self {
        for ty in INTERNED_TYPES {
            if *ty == self {
                return ty;
            }
        }
        panic!("Type::interned() for invalid type: {:?}", self);
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
            _ => false,
        };
        match c.look_ahead(c_idx) {
            Some(TokenTree::Token(_, Token::Ident(ty_ident, _))) => {
                if let Some(ty) = Type::from_ast_ident(ty_ident) {
                    c.nth(c_idx);
                    if is_optional {
                        return Type::Optional(ty.interned());
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
                    assert!(
                        prefix == "'",
                        "Lifetime identifier does not start with ': {}",
                        ident
                    );
                    let dollar_sym = Symbol::intern(&format!("'${}", label));
                    let label_ty = maybe_get_type(&mut c);
                    bt.set_type(dollar_sym, label_ty);
                    TokenTree::Token(sp, Token::Lifetime(Ident::new(dollar_sym, ident.span)))
                }

                _ => TokenTree::Token(sp, Token::Dollar),
            },

            TokenTree::Delimited(sp, delim, tts) => {
                let dts = rewrite_token_stream(tts.into(), bt);
                TokenTree::Delimited(sp, delim, dts.into())
            }

            tt @ _ => tt,
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
