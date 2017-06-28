
RUSTC = ~/install/rust/build/x86_64-unknown-linux-gnu/stage1/bin/rustc
GEN_SRCS = \
		   src/ast_equiv_gen.inc.rs \
		   src/matcher_impls_gen.inc.rs \
		   src/rewrite_impls_gen.inc.rs \

idiomize idiomize.d: $(GEN_SRCS)
	 $(RUSTC) src/main.rs -C rpath --crate-name idiomize --emit link,dep-info

src/ast_equiv_gen.inc.rs: $(wildcard gen/*.py) gen/ast.txt
	python3 gen/process_ast.py ast_equiv $@

src/matcher_impls_gen.inc.rs: $(wildcard gen/*.py) gen/ast.txt
	python3 gen/process_ast.py matcher $@

src/rewrite_impls_gen.inc.rs: $(wildcard gen/*.py) gen/ast.txt
	python3 gen/process_ast.py rewrite $@

include idiomize.d
