
use syntax::ast::*;
use renamer::Renamer;
use convert_type::TypeConverter;
use idiomize::ast_manip::make_ast::*;
use clang_ast::*;
use syntax::ptr::*;
use syntax::print;
use syntax::print::pprust::*;
use std::io;
use std::collections::HashSet;

pub struct Translation {
    pub items: Vec<P<Item>>,
    pub type_converter: TypeConverter,
    pub ast_context: AstContext,
    renamer: Renamer<String>,
}

pub fn translate(ast_context: AstContext) -> String {
    use clang_ast::*;
    let mut t = Translation::new(ast_context.clone());

    for top_id in ast_context.top_nodes.to_owned() {
        let x = match ast_context.ast_nodes.get(&top_id) {
            Some(n) => n.clone(),
            None => continue,
        };

        if x.tag == ASTEntryTag::TagFunctionDecl {

            let name = expect_string(&x.extras[0]).expect("Expected a name");

            let ty = ast_context.get_type(x.type_id.expect("Expected a type")).expect("Expected a number");
            let funtys = expect_array(&ty.extras[0]).expect("Function declaration type expected");
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
            let body = x.children[args_n].expect("Expected body id");

            t.add_function(&name, &args, ret, body);
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
            renamer: Renamer::new(HashSet::new()), // XXX: Populate reserved words
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

        // Start scope for function parameters
        self.renamer.add_scope();

        let args : Vec<Arg> = arguments.iter().map(|&(var, ty)| {
            let rust_var = self.renamer.insert(var.to_string(), var).expect("Failed to insert argument");
            mk().arg(self.convert_type(ty), mk().ident_pat(rust_var))
        }).collect();

        let ret = FunctionRetTy::Ty(self.convert_type(return_type));

        let decl = mk().fn_decl(args, ret);

        let block = self.convert_function_body(body);

        // End scope for function parameters
        self.renamer.drop_scope();

        self.items.push(mk().fn_item(name, decl, block));
    }

    fn convert_function_body(&mut self, body_id: u64) -> P<Block> {

        let node =
            self.ast_context
                .ast_nodes
                .get(&body_id)
                .expect("Expected function body node")
                .to_owned(); // release immutable borrow on self

        assert_eq!(node.tag, ASTEntryTag::TagCompoundStmt);

        // Open function body scope
        self.renamer.add_scope();

        let stmts : Vec<Stmt> =
            node.children
                .iter()
                .flat_map(|&stmt_id| {
                    self.convert_stmt(stmt_id.unwrap())
                }).collect();

        // Close function body scope
        self.renamer.drop_scope();

        mk().block(stmts)
    }

    fn convert_stmt(&mut self, stmt_id: u64) -> Vec<Stmt> {
        let node : AstNode =
            self.ast_context
                .ast_nodes
                .get(&stmt_id)
                .unwrap()
                .to_owned(); // release immutable borrow on self

        match node.tag {
            ASTEntryTag::TagDeclStmt =>
                node.children.iter().flat_map(|decl_id| self.convert_decl_stmt(decl_id.unwrap())).collect(),
            t => panic!("Statement translation not support for {:?}", t),
        }
    }

    fn convert_decl_stmt(&mut self, decl_id: u64) -> Vec<Stmt> {

        let node : AstNode =
            self.ast_context
                .ast_nodes
                .get(&decl_id)
                .unwrap()
                .to_owned(); // release immutable borrow on self

        match node.tag {
            ASTEntryTag::TagVarDecl => {
                let var_name = expect_string(&node.extras[0]).unwrap();
                let rust_name = self.renamer.insert(var_name.clone(), &var_name).unwrap();
                let pat = mk().ident_pat(rust_name);
                let init : Option<P<Expr>> = None; // TODO
                let ty = self.convert_type(node.type_id.unwrap());
                let local = mk().local(pat, Some(ty), init);
                vec![mk().local_stmt(P(local))]
            }
            t => panic!("Declaration not implemented {:?}", t),
        }

    }

    fn convert_type(&self, type_id: u64) -> P<Ty> {
        self.type_converter.convert(&self.ast_context, type_id)
    }

}
