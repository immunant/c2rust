//! The `Bindings` type, for mapping names to AST fragments.
use std::collections::hash_map::{HashMap, Entry};
use syntax::ast::{Ident, Path, Expr, Pat, Ty, Stmt, Item};
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::{Delimited, TokenTree, TokenStream, TokenStreamBuilder};

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
}

macro_rules! define_binding_values {
    ($( $Thing:ident($Repr:ty),
            $add_thing:ident, $try_add_thing:ident,
            $thing:ident, $get_thing:ident; )*) => {
        /// An AST fragment, of any of the supported node types.
        #[derive(Clone, Debug)]
        enum Value {
            $( $Thing($Repr), )*
        }

        /// The types of AST fragments that can be used in `Bindings`.
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub enum Type {
            Unknown,
            $( $Thing, )*
        }

        impl Type {
            fn from_ast_ident(ty_ident: Ident) -> Type {
                match &*ty_ident.as_str() {
                    $(stringify!($thing) => Type::$Thing,)*
                    ty @ _ => panic!("unknown binding type: {}", ty)
                }
            }
        }

        impl Bindings {
            $(
                pub fn $add_thing<S: IntoSymbol>(&mut self, name: S, val: $Repr) {
                    let name = name.into_symbol();
                    let ok = self.$try_add_thing(name, val);
                    assert!(ok, "cannot alter existing binding {:?}", name);
                }

                pub fn $try_add_thing<S: IntoSymbol>(&mut self, name: S, val: $Repr) -> bool {
                    self.try_add(name, Value::$Thing(val))
                }

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

                pub fn $get_thing<S: IntoSymbol>(&self, name: S) -> Option<&$Repr> {
                    let name = name.into_symbol();
                    match self.map.get(&name) {
                        Some(&Value::$Thing(ref val)) => Some(val),
                        Some(_) => None,
                        None => None,
                    }
                }
            )*

            /// Get the type of fragment associated with `name`, if any.
            pub fn get_type<S: IntoSymbol>(&self, name: S) -> Option<Type> {
                self.map.get(&name.into_symbol()).map(|v| {
                    match v {
                        $( &Value::$Thing(_) => Type::$Thing, )*
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
                    )*
                    (_, _) => false,
                }
            }
        }
    };
}

// To allow bindings to contain more types of AST nodes, add more lines to this macro.
define_binding_values! {
    Ident(Ident), add_ident, try_add_ident, ident, get_ident;
    Path(Path), add_path, try_add_path, path, get_path;
    Expr(P<Expr>), add_expr, try_add_expr, expr, get_expr;
    Pat(P<Pat>), add_pat, try_add_pat, pat, get_pat;
    Ty(P<Ty>), add_ty, try_add_ty, ty, get_ty;
    Stmt(Stmt), add_stmt, try_add_stmt, stmt, get_stmt;
    MultiStmt(Vec<Stmt>), add_multi_stmt, try_add_multi_stmt, multi_stmt, get_multi_stmt;
    Item(P<Item>), add_item, try_add_item, item, get_item;
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
                    let ident_ty = if let Some(TokenTree::Token(_, Token::Colon)) = c.look_ahead(0) {
                        c.next();
                        match c.next() {
                            Some(TokenTree::Token(_, Token::Ident(ty_ident, _))) =>
                                Type::from_ast_ident(ty_ident),
                            tt @ _ => panic!("expected identifier, got {:?}", tt)
                        }
                    } else {
                        Type::Unknown
                    };
                    bt.set_type(dollar_sym, ident_ty);
                    TokenTree::Token(sp, Token::Ident(Ident::new(dollar_sym, ident.span), is_raw))
                }

                Some(TokenTree::Token(sp, Token::Lifetime(ident))) => {
                    c.next();
                    let ident_str = &*ident.as_str();
                    let (prefix, label) = ident_str.split_at(1);
                    assert!(prefix == "'", "Lifetime identifier does not start with ': {}", ident);
                    let dollar_sym = Symbol::intern(&format!("'${}", label));
                    bt.set_type(dollar_sym, Type::Ident);
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
