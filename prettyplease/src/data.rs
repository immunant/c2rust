use crate::algorithm::Printer;
use crate::iter::IterDelimited;
use crate::INDENT;
use syn::{Field, Fields, FieldsUnnamed, Variant, VisCrate, VisPublic, VisRestricted, Visibility};

impl Printer {
    pub fn variant(&mut self, variant: &Variant) {
        self.outer_attrs(&variant.attrs);
        self.ident(&variant.ident);
        match &variant.fields {
            Fields::Named(fields) => {
                self.nbsp();
                self.word("{");
                self.cbox(INDENT);
                self.space();
                for field in fields.named.iter().delimited() {
                    self.field(&field);
                    self.trailing_comma_or_space(field.is_last);
                }
                self.offset(-INDENT);
                self.end();
                self.word("}");
            }
            Fields::Unnamed(fields) => {
                self.cbox(INDENT);
                self.fields_unnamed(fields);
                self.end();
            }
            Fields::Unit => {}
        }
        if let Some((_eq_token, discriminant)) = &variant.discriminant {
            self.word(" = ");
            self.expr(discriminant);
        }
    }

    pub fn fields_unnamed(&mut self, fields: &FieldsUnnamed) {
        self.word("(");
        self.zerobreak();
        for field in fields.unnamed.iter().delimited() {
            self.field(&field);
            self.trailing_comma(field.is_last);
        }
        self.offset(-INDENT);
        self.word(")");
    }

    pub fn field(&mut self, field: &Field) {
        self.outer_attrs(&field.attrs);
        self.visibility(&field.vis);
        if let Some(ident) = &field.ident {
            self.ident(ident);
            self.word(": ");
        }
        self.ty(&field.ty);
    }

    pub fn visibility(&mut self, vis: &Visibility) {
        match vis {
            Visibility::Public(vis) => self.vis_public(vis),
            Visibility::Crate(vis) => self.vis_crate(vis),
            Visibility::Restricted(vis) => self.vis_restricted(vis),
            Visibility::Inherited => {}
        }
    }

    fn vis_public(&mut self, vis: &VisPublic) {
        let _ = vis;
        self.word("pub ");
    }

    fn vis_crate(&mut self, vis: &VisCrate) {
        let _ = vis;
        self.word("crate ");
    }

    fn vis_restricted(&mut self, vis: &VisRestricted) {
        self.word("pub(");
        // TODO: If we have a path which is not "self" or "super" or "crate",
        // automatically add the "in" token.
        if vis.in_token.is_some() {
            self.word("in ");
        }
        self.path(&vis.path);
        self.word(") ");
    }
}
