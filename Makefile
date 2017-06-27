
RUSTC = ~/install/rust/build/x86_64-unknown-linux-gnu/stage1/bin/rustc
GEN_SRCS = src/ast_equiv_gen.inc.rs

idiomize: $(GEN_SRCS)
	 $(RUSTC) src/main.rs -C rpath -o $@

src/ast_equiv_gen.inc.rs: $(wildcard gen/*.py) gen/ast.txt
	python3 gen/process_ast.py ast_equiv $@
