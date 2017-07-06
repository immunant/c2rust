use syntax::ast::*;
use syntax::codemap::{DUMMY_SP, Spanned};
use syntax::ptr::P;
use bindings::IntoSymbol;


pub trait Make<T> {
    fn make(self, mk: &Builder) -> T;
}

impl<T> Make<T> for T {
    fn make(self, _mk: &Builder) -> T {
        self
    }
}

impl<'a, T: Clone> Make<T> for &'a T {
    fn make(self, _mk: &Builder) -> T {
        self.clone()
    }
}


impl<S: IntoSymbol> Make<Ident> for S {
    fn make(self, _mk: &Builder) -> Ident {
        Ident::with_empty_ctxt(self.into_symbol())
    }
}


impl<'a> Make<Visibility> for &'a str {
    fn make(self, _mk: &Builder) -> Visibility {
        match self {
            "pub" => Visibility::Public,
            "priv" | "" | "inherit" => Visibility::Inherited,
            _ => panic!("unrecognized string for Visibility: {:?}", self),
        }
    }
}


impl<I: Make<Ident>> Make<PathSegment> for I {
    fn make(self, mk: &Builder) -> PathSegment {
        PathSegment {
            identifier: self.make(mk),
            span: DUMMY_SP,
            parameters: None,
        }
    }
}


impl<S: Make<PathSegment>> Make<Path> for Vec<S> {
    fn make(self, mk: &Builder) -> Path {
        Path {
            span: DUMMY_SP,
            segments: self.into_iter().map(|s| s.make(mk)).collect(),
        }
    }
}


pub struct Builder {
    vis: Visibility,
    mutbl: Mutability,
    generics: Generics,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            vis: Visibility::Inherited,
            mutbl: Mutability::Immutable,
            generics: Generics::default(),
        }
    }


    pub fn vis<V: Make<Visibility>>(self, vis: V) -> Self {
        let vis = vis.make(&self);
        Builder {
            vis: vis,
            .. self
        }
    }

    pub fn mutbl(self) -> Self {
        Builder {
            mutbl: Mutability::Mutable,
            .. self
        }
    }


    pub fn struct_field<I, T>(self, ident: I, ty: T) -> StructField
            where I: Make<Ident>, T: Make<P<Ty>> {
        let ident = ident.make(&self);
        let ty = ty.make(&self);
        StructField {
            span: DUMMY_SP,
            ident: Some(ident),
            vis: self.vis,
            id: DUMMY_NODE_ID,
            ty: ty,
            attrs: Vec::new(),
        }
    }

    pub fn struct_item<I>(self, name: I, fields: Vec<StructField>) -> P<Item>
            where I: Make<Ident> {
        let name = name.make(&self);
        P(Item {
            ident: name,
            attrs: Vec::new(),
            id: DUMMY_NODE_ID,
            node: ItemKind::Struct(VariantData::Struct(fields, DUMMY_NODE_ID),
                                   self.generics),
            vis: self.vis,
            span: DUMMY_SP,
        })
    }

    pub fn path_ty<Pa>(self, path: Pa) -> P<Ty>
            where Pa: Make<Path> {
        let path = path.make(&self);
        P(Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Path(None, path),
            span: DUMMY_SP,
        })
    }

    pub fn field<I, E>(self, ident: I, expr: E) -> Field
            where I: Make<Ident>, E: Make<P<Expr>> {
        let ident = ident.make(&self);
        let expr = expr.make(&self);
        Field {
            ident: Spanned {
                node: ident,
                span: DUMMY_SP,
            },
            expr: expr,
            span: DUMMY_SP,
            is_shorthand: false,
            attrs: ThinVec::new(),
        }
    }

    pub fn struct_expr<Pa>(self, path: Pa, fields: Vec<Field>) -> P<Expr>
            where Pa: Make<Path> {
        let path = path.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Struct(path, fields, None),
            span: DUMMY_SP,
            attrs: ThinVec::new(),
        })
    }

    pub fn struct_expr_base<Pa, E>(self, path: Pa, fields: Vec<Field>, base: E) -> P<Expr>
            where Pa: Make<Path>, E: Make<P<Expr>> {
        let path = path.make(&self);
        let base = base.make(&self);
        P(Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Struct(path, fields, Some(base)),
            span: DUMMY_SP,
            attrs: ThinVec::new(),
        })
    }

    pub fn static_item<I, T, E>(self, name: I, ty: T, init: E) -> P<Item>
            where I: Make<Ident>, T: Make<P<Ty>>, E: Make<P<Expr>> {
        let name = name.make(&self);
        let ty = ty.make(&self);
        let init = init.make(&self);
        P(Item {
            ident: name,
            attrs: Vec::new(),
            id: DUMMY_NODE_ID,
            node: ItemKind::Static(ty, self.mutbl, init),
            vis: self.vis,
            span: DUMMY_SP,
        })
    }
}

pub fn mk() -> Builder {
    Builder::new()
}
