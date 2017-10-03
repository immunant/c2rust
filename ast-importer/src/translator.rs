
use syntax::ast::*;
use name_manager::NameManager;
use convert_type::TypeConverter;
use idiomize::ast_manip::make_ast::*;
use clang_ast::*;

pub struct Translation {
    name_manager: NameManager,
    items: Vec<Item>,
    type_converter: TypeConverter,
    ast_context: AstContext,
}

impl Translation {

    pub fn add_struct(&mut self, name: Ident, fields: Vec<(&str, u64)>) {
        let struct_fields =
            fields
                .into_iter()
                .map(|(id,ty)| {
                    let ty = self.type_converter(ty);
                    mk().struct_field(id,ty) })
                .collect();

        let item = mk().struct_item(name, struct_fields);

        self.items.push(*item);
    }
}