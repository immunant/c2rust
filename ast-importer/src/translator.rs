
use syntax::ast::*;
use name_manager::NameManager;
use convert_type::TypeConverter;
use idiomize::ast_manip::make_ast::*;
use clang_ast::*;
use syntax::ptr::*;
use syntax::print;
use syntax::print::pprust::*;
use std::io;

pub struct Translation {
    pub items: Vec<P<Item>>,
    pub type_converter: TypeConverter,
    pub ast_context: AstContext,
}

pub fn translate(ast_context: AstContext) -> String {
    use clang_ast::*;
    let mut t = Translation::new(ast_context.clone());

    for x in ast_context.ast_nodes.values() {
        if x.tag == ASTEntryTag::TagFunctionDecl {

            let name = expect_string(&x.extras[0]).expect("Expected a name");

            let ty = ast_context.get_type(x.type_id.expect("Expected a type")).expect("Expected a number");
            let funtys = expect_array(&ty.extras[0]).unwrap();
            let ret = expect_u64(&funtys[0]).expect("Expected a return type");

            let args_n = x.children.len() - 1;
            let args : Vec<(String,u64)> =
                x.children[0 .. args_n]
                 .iter().map(|x| {
                     let p = ast_context.ast_nodes.get(&x.expect("Missing parameter id")).expect("Bad parameter id");
                     let param_name = expect_string(&p.extras[0]).expect("Parameter name required");
                     (param_name, p.type_id.expect("Parameter type required"))
                 }).collect();

            let args : Vec<(&str, u64)> = args.iter().map(|&(ref x,y)| (x.as_str(),y)).collect();

            t.add_function(&name, &args, ret, 0);
        }
    }

    to_string(|s| {

        for x in t.items.iter() {
            s.print_item(x)?
        }

        Ok(())
    })
}



impl Translation {

    pub fn new(ast_context: AstContext) -> Translation {
        Translation {
            items: vec![],
            type_converter: TypeConverter::new(),
            ast_context,
        }
    }

    pub fn add_struct(&mut self, name: Ident, fields: &[(&str, u64)]) {
        let struct_fields =
            fields
                .iter()
                .map(|&(id,ty)| {
                    let ty = self.type_converter.convert(&self.ast_context, ty);
                    mk().struct_field(id,ty) })
                .collect();

        let item = mk().struct_item(name, struct_fields);

        self.items.push(item);
    }

    pub fn add_typedef(&mut self, name: &str, typeid: u64) {
        let ty = self.convert_type(typeid);
        let item = mk().type_item(name, ty);
        self.items.push(item);
    }

    pub fn add_function(&mut self, name: &str, arguments: &[(&str, u64)], return_type: u64, body: u64) {

        let args : Vec<Arg> = arguments.iter().map(|&(var, ty)| {
            mk().arg(self.convert_type(ty), mk().ident_pat(var))
        }).collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type));

        let decl = mk().fn_decl(args, ret);
        let block = mk().block(vec![] as Vec<Stmt>);

        self.items.push(mk().fn_item(name, decl, block));
    }

    fn convert_type(&self, type_id: u64) -> P<Ty> {
        self.type_converter.convert(&self.ast_context, type_id)
    }
}